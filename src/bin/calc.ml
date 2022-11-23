open Clarke
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

open Cmdliner

let data_file_term =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"A file containing JSON values" [ "data" ]

let cmd setup_log fs =
  let main () file =
    Path.(with_open_in (fs / file)) @@ fun flow ->
    let reader = Buf_read.of_flow ~max_size:max_int flow in
    let infos = read_info `Json reader in
    let diff = time_diff infos |> Ptime.Span.to_float_s |> seconds_to_hours in
    let kws = List.map (fun i -> kilowatts i) infos in
    let total_energy =
      List.fold_left (fun acc kw -> acc +. (kw *. diff)) 0. kws
    in
    let intensities =
      try List.map (fun i -> Info.intensity i) infos |> fill_intensities
      with _ -> []
    in
    Fmt.pr "Total time: %as@." Fmt.uint64_ns_span
      (total_time_ns infos |> Int64.of_float);
    (match intensities with
    | [] -> Fmt.pr "Total energy: %fkJ@." total_energy
    | intensities ->
        let kw_in = List.combine kws intensities in
        let emissions =
          List.fold_left (fun acc (k, i) -> acc +. (diff *. k *. i)) 0. kw_in
        in
        Fmt.pr "Total energy: %fkJ@.Emissions: %fgCO2@." total_energy emissions);
    Ok ()
  in
  let info = Cmd.info "calc" in
  Cmd.v info Term.(const main $ setup_log $ data_file_term)
