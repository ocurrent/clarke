open Eio

let json_option f = function `Null -> None | v -> Some (f v)
let rec last = function [ x ] -> x | _ :: ys -> last ys | [] -> assert false

let info_of_line line = function
  | `Json ->
      let json = Ezjsonm.value_from_string line in
      let timestamp, _, _ =
        Ezjsonm.find json [ "timestamp" ]
        |> Ezjsonm.get_string |> Ptime.of_rfc3339 |> Result.get_ok
      in
      let watts = Ezjsonm.find json [ "watts" ] |> Ezjsonm.get_float in
      let intensity =
        Ezjsonm.find json [ "intensity" ] |> json_option Ezjsonm.get_float
      in
      Info.v ?intensity timestamp watts
  | `Csv -> (
      match String.split_on_char ',' line with
      | timestamp :: watts :: [ intensity ] ->
          let timestamp, _, _ = Ptime.of_rfc3339 timestamp |> Result.get_ok in
          let watts = float_of_string watts in
          let intensity =
            if intensity = "" then None else float_of_string_opt intensity
          in
          Info.v ?intensity timestamp watts
      | _ -> Fmt.failwith "Failed to parse info from %s" line)

let read_info format reader =
  let lines = Buf_read.lines reader in
  let infos = Seq.map (fun line -> info_of_line line format) lines in
  List.of_seq infos

let time_diff = function
  | i :: j :: _ -> Ptime.diff (Info.timestamp j) (Info.timestamp i)
  | _ -> Fmt.failwith "Expected at least two energy entries!"

let total_time_ns info =
  let start = List.hd info in
  let last = last info in
  Ptime.diff (Info.timestamp last) (Info.timestamp start)
  |> Ptime.Span.to_float_s |> Float.mul 1_000_000_000.

let kilowatts i = Info.watts i /. 1000.
let seconds_to_hours s = s /. (60. *. 60.)

(* Sometime we have missing values for intensity -- if there is a previous
   value we fill with that. *)
let fill_intensities i =
  let prev = ref None in
  try
    List.map
      (function
        | Some v ->
            prev := Some v;
            v
        | None -> Option.get !prev)
      i
  with _ -> []

type t = {
  total_time : int64; (* nanoseconds *)
  total_energy : float; (* kJ *)
  total_emissions : float option; (* gCO2 *)
}

let total_time t = t.total_time
let total_emissions t = t.total_emissions
let total_energy t = t.total_energy

let pp ppf t =
  Fmt.pf ppf "Total time: %a@.Total energy: %fkJ@.Emissions: %agCO2@."
    Fmt.uint64_ns_span t.total_time t.total_energy
    Fmt.(option ~none:(Fmt.const Fmt.string "N/A") float)
    t.total_emissions

let summary ?(format = `Json) reader =
  let infos = read_info format reader in
  let diff = time_diff infos |> Ptime.Span.to_float_s |> seconds_to_hours in
  let kws = List.map (fun i -> kilowatts i) infos in
  let total_energy =
    List.fold_left (fun acc kw -> acc +. (kw *. diff)) 0. kws
  in
  let intensities =
    try List.map (fun i -> Info.intensity i) infos |> fill_intensities
    with _ -> []
  in
  let total_emissions =
    match intensities with
    | [] -> None
    | intensities ->
        let kw_in = List.combine kws intensities in
        let emissions =
          List.fold_left (fun acc (k, i) -> acc +. (diff *. k *. i)) 0. kw_in
        in
        Some emissions
  in
  Ok
    {
      total_time = total_time_ns infos |> Int64.of_float;
      total_energy;
      total_emissions;
    }

module Reporter = struct
  module type S = sig
    type conf

    val report :
      machine:string -> conf -> t -> (unit, [ `Msg of string ]) result
  end

  type t = Reporter : ((module S with type conf = 'a) * 'a) -> t

  module Stdout = struct
    type conf = Eio.Flow.sink

    let report ~machine flow s =
      Ok (Flow.copy_string (Fmt.str "machine:%s\n%a\n" machine pp s) flow)
  end

  module File = struct
    type conf = Fs.dir Path.t

    let report ~machine path s =
      Path.with_open_out ~append:true ~create:(`If_missing 0o644) path
      @@ fun flow -> Stdout.report ~machine flow s
  end

  module Slack = struct
    open Cohttp_eio

    type conf = Net.t * string (* The slack endpoint *)

    type status =
      [ `Getaddr_info_empty of string | `Connection_failure | Http.Status.t ]

    let pp_status ppf = function
      | `Connection_failure -> Fmt.string ppf "Failed to connect"
      | `Getaddr_info_empty s ->
          Fmt.pf ppf "Getaddr info returned no IP addresses: %s" s
      | #Http.Status.t as v -> Http.Status.pp ppf v

    let null_auth ?ip:_ ~host:_ _ = Ok None

    let with_tls_conn ~net hostname fn =
      let addrs =
        try Net.getaddrinfo_stream ~service:"https" net hostname with _ -> []
      in
      match addrs with
      | [] -> `Getaddr_info_empty hostname
      | addr :: _ -> (
          Switch.run @@ fun sw ->
          let authenticator = null_auth in
          let socket =
            try `Socket (Eio.Net.connect ~sw net addr)
            with Io (Net.E (Net.Connection_failure _exn), _ctx) ->
              Logs.info (fun f -> f "Connection failur");
              `Connection_failure
          in
          match socket with
          | `Connection_failure -> fn `Connection_failure
          | `Socket socket ->
              let conn =
                let host =
                  Result.to_option
                    (Result.bind
                       (Domain_name.of_string hostname)
                       Domain_name.host)
                in
                Tls_eio.client_of_flow
                  Tls.Config.(
                    client ~version:(`TLS_1_1, `TLS_1_3) ~authenticator ())
                  ?host socket
              in
              fn (`Ok conn))

    let hostname = "hooks.slack.com"

    let format machine t =
      `O
        [
          ("type", `String "section");
          ( "fields",
            `A
              [
                `O
                  [
                    ("type", `String "mrkdwn");
                    ("text", `String ("*Machine:*\n" ^ machine));
                  ];
                `O
                  [
                    ("type", `String "mrkdwn");
                    ("text", `String (Fmt.str "*Info:*\n%a" pp t));
                  ];
              ] );
        ]

    let format_msgs machine msgs = `O [ ("blocks", `A [ format machine msgs ]) ]

    let headers s =
      Http.Header.of_list
        [
          ("host", hostname);
          (* ("Content-Type", "application/json"); *)
          ("Content-Length", string_of_int s);
        ]

    let body ~machine msgs = format_msgs machine msgs |> Ezjsonm.value_to_string

    let report ~machine (net, t) s =
      let run s =
        with_tls_conn ~net hostname @@ function
        | `Connection_failure -> `OK
        | `Ok conn ->
            let resp, _ =
              Client.post
                ~headers:(headers (String.length s))
                ~body:(Body.Fixed s) ~conn (hostname, None) t
            in
            (Http.Response.status resp :> status)
      in
      let status =
        let s = body ~machine s in
        run s
      in
      match status with
      | `OK -> Ok ()
      | s -> Error (`Msg ("Slack post failed: " ^ Fmt.str "%a\n" pp_status s))
  end

  let make_file path = ((module File : S with type conf = File.conf), path)
  let make_sink sink = ((module Stdout : S with type conf = Stdout.conf), sink)

  let make_slack net path =
    let endpoint = Path.load path |> String.trim in
    ((module Slack : S with type conf = Slack.conf), (net, endpoint))

  type spec = [ `File of string | `Slack of string | `Stdout ]

  let of_spec env : spec -> t = function
    | `File p -> Reporter (make_file Path.(env#fs / p))
    | `Slack e -> Reporter (make_slack env#net Path.(env#fs / e))
    | `Stdout -> Reporter (make_sink env#stdout)

  let spec_of_string s =
    match Astring.String.cut s ~sep:":" with
    | Some ("file", file) -> `File file
    | Some ("slack", e) -> `Slack e
    | _ -> `Stdout

  let pp_spec f = function
    | `File path -> Fmt.pf f "file:%s" path
    | `Slack path -> Fmt.pf f "slack:%s" path
    | `Stdout -> Fmt.string f "stdout"
end
