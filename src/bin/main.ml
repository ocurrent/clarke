open Eio
open Clarke

(* Adding a metric for a carbon intensity gauge *)
module Metrics = struct
  let namespace = "Clarke"
  let subsystem = "meter"

  let carbon_intensity =
    let help = "Current carbon intensity in gCO2/kWh" in
    Prometheus.Gauge.v ~help ~namespace ~subsystem "intensity"
end

type output = O : (module S.Output with type t = 'a) * 'a -> output

let with_socket ~sw net v fn =
  let listener = Net.listen ~sw ~backlog:5 net v in
  while true do
    Net.accept_fork ~sw listener ~on_error:(fun e ->
        traceln "Err: %s" (Printexc.to_string e))
    @@ fun socket _addr ->
    fn
      (O
         ( (module Outputs.Flow : S.Output with type t = Flow.sink),
           (socket :> Flow.sink) ))
  done

module Specs = struct
  type output_spec =
    | Flow of [ `Stdout | Net.Sockaddr.stream | `File of string ]
    | Capnp of string

  let with_output ~sw ~fs ~stdout ~net spec fn =
    match spec with
    | Flow `Stdout ->
        fn
          (O
             ( (module Outputs.Flow : S.Output with type t = Flow.sink),
               (stdout :> Flow.sink) ))
    | Flow (`File f) ->
        Path.with_open_out ~append:true ~create:(`If_missing 0o644)
          Path.(fs / f)
        @@ fun flow ->
        fn
          (O
             ( (module Outputs.Flow : S.Output with type t = Flow.sink),
               (flow :> Flow.sink) ))
    | Flow (#Net.Sockaddr.stream as v) -> with_socket ~sw net v fn
    | Capnp f ->
        let uri = Path.(load (fs / f)) |> String.trim |> Uri.of_string in
        let client_vat = Capnp_rpc_unix.client_only_vat ~sw net in
        let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
        Capnp_rpc_lwt.Sturdy_ref.with_cap_exn sr @@ fun client ->
        fn
          (O
             ( (module Outputs.Capnp : S.Output with type t = Capnp_client.t),
               client ))

  let output_spec_of_string s =
    match String.lowercase_ascii s with
    | "stdout" -> Ok (Flow `Stdout)
    | v -> (
        match String.split_on_char ':' v with
        | [ "file"; path ] -> Ok (Flow (`File path))
        | [ "tcp"; addr; port ] -> (
            try
              let addr =
                if addr = "loopback" then Net.Ipaddr.V4.loopback
                else Net.Ipaddr.of_raw addr
              in
              Ok (Flow (`Tcp (addr, int_of_string port)))
            with _ -> Error (`Msg "Port parsing failed"))
        | [ "unix"; addr ] -> Ok (Flow (`Unix addr))
        | [ "capnp"; path ] -> Ok (Capnp path)
        | _ -> Error (`Msg ("Unknown " ^ v)))

  let pp_output_spec ppf = function
    | Flow (`File f) -> Fmt.pf ppf "file:%s" f
    | Flow `Stdout -> Format.pp_print_string ppf "stdout"
    | Flow (#Net.Sockaddr.stream as v) -> Net.Sockaddr.pp ppf v
    | Capnp file -> Fmt.pf ppf "capnp:%s" file

  let output_spec = Cmdliner.Arg.conv (output_spec_of_string, pp_output_spec)

  type meter_spec = [ `Const of float | `Ipmi of string | `Variorum ]

  let meter_of_meter_spec ~clock = function
    | `Const f -> Models.const ~clock f
    | `Ipmi sensor -> S.Meter ((module Clarke.Models.Ipmi), { clock; sensor })
    | `Variorum -> S.Meter ((module Clarke.Models.Variorum), { clock })

  let meter_spec_of_string s =
    match String.lowercase_ascii s with
    | "variorum" -> Ok `Variorum
    | "ipmi" -> Ok (`Ipmi "Pwr Consumption")
    | v -> (
        match String.split_on_char ':' v with
        | [ "const"; f ] -> (
            try Ok (`Const (float_of_string f))
            with _ -> Error (`Msg "Float parsing failed"))
        | [ "ipmi"; _ ] ->
            let sensor = String.split_on_char ':' s |> List.tl |> List.hd in
            Ok (`Ipmi sensor)
        | _ -> Error (`Msg ("Unknown " ^ v)))

  let pp_meter_spec ppf = function
    | `Const f -> Format.fprintf ppf "const:%.2fW" f
    | `Variorum -> Format.pp_print_string ppf "variorum"
    | `Ipmi s -> Format.fprintf ppf "ipmi:%s" s

  let meter_spec = Cmdliner.Arg.conv (meter_spec_of_string, pp_meter_spec)
end

(* Command Line *)
open Cmdliner

let meter_spec_term =
  Arg.value
  @@ Arg.opt Specs.meter_spec (`Const 250.)
  @@ Arg.info
       ~doc:
         "The meter by which to measure power consumption which could be a \
          constant value (const:123), variorum or ipmi. Ipmi can be configured \
          to use a different sensor with --meter=ipmi:<sensor>. By default it \
          will use 'Pwr Consumption'"
       [ "m"; "meter" ]

let output_spec_term =
  Arg.value
  @@ Arg.opt Specs.output_spec (Flow `Stdout)
  @@ Arg.info
       ~doc:
         "The output location of the monitoring data, which can be a capnp \
          capability written to a file"
       [ "o"; "output" ]

let period_term =
  Arg.value @@ Arg.opt Arg.int 5
  @@ Arg.info ~doc:"The frequency in seconds for monitoring the power usage"
       [ "p"; "period" ]

let country_code =
  let of_string s =
    try Ok (ISO3166.alpha2_of_string (String.uppercase_ascii s))
    with _ -> Error (`Msg "Unknown country code")
  in
  let pp ppf v = Fmt.pf ppf "%s" (ISO3166.alpha2_to_string v) in
  Cmdliner.Arg.conv (of_string, pp)

let country_code_term =
  Arg.value
  @@ Arg.opt (Arg.some country_code) None
  @@ Arg.info ~doc:"The country code (alpha2) for carbon intensity calculations"
       [ "c"; "country" ]

let api_term =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"A file containing the CO2-Signal API code" [ "co2-signal" ]

let machine_term =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The machine name" [ "machine" ]

let reporter_term =
  let open Summary in
  let spec =
    Arg.conv ((fun s -> Ok (Reporter.spec_of_string s)), Reporter.pp_spec)
  in
  Arg.value
  @@ Arg.opt Arg.(some spec) None
  @@ Arg.info
       ~doc:
         "An optional reporter to use: stdout, file:<path>, \
          slack:<endpoint-in-file>. Note that when a reporter is enabled it \
          will only work with a file output for the data as it must persist. \
          The reporter will then delete the data file after"
       [ "reporter" ]

let report_period_term =
  Arg.value
  @@ Arg.opt Arg.float (24. *. 60. *. 60.)
  @@ Arg.info ~doc:"How often to summarise a report in seconds"
       [ "report-period" ]

let update_info_with_intensity intensity info =
  let watts = Info.watts info in
  let timestamp = Info.timestamp info in
  Info.v ?intensity timestamp watts

let get_intensity ?country ?api_code net =
  let open Carbon in
  match (country, api_code) with
  | None, None -> None
  | Some code, None ->
      if `GB = code then
        Carbon.Gb.(get_intensity (v net))
        |> Carbon.Gb.Intensity.actual |> Option.map float_of_int
      else (
        Logs.warn (fun f ->
            f "Skipping carbon intensity for %s"
              (ISO3166.alpha2_to_country code |> ISO3166.Country.name));
        None)
  | Some country_code, Some api ->
      Logs.info (fun f ->
          f "Calculating carbon intensity for %s"
            (ISO3166.alpha2_to_string country_code));
      let v = Carbon.Co2_signal.v ~api_key:api net in
      let i = Co2_signal.get_intensity v ~country_code in
      Co2_signal.Intensity.intensity i |> float_of_int |> Option.some
  | _ -> None

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.Src.set_level Clarke.log_src level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(
    const setup_log $ Fmt_cli.style_renderer ~docs () $ Logs_cli.level ~docs ())

let log_err = function
  | Ok () -> ()
  | Error (`Msg s) -> Logs.err (fun f -> f "%s" s)

let report ~env ~machine spec file =
  let open Summary in
  match spec with
  | None -> ()
  | Some spec ->
      let (Reporter ((module T), conf)) = Reporter.of_spec env spec in
      let s =
        Path.(with_open_in (env#fs / file)) @@ fun flow ->
        let reader = Buf_read.of_flow ~max_size:max_int flow in
        Clarke.Summary.summary ~format:`Csv reader |> Result.get_ok
      in
      Path.(unlink (env#fs / file));
      T.report ~machine conf s |> log_err

let monitor ~env ~stdout ~net ~clock =
  let run () machine output_spec meter period prom country api_code
      reporter_spec reporter_period =
    let api_code =
      Option.map (fun f -> Path.(load (env#fs / f) |> String.trim)) api_code
    in
    let fs = Eio.Stdenv.fs env in
    let get_intensity () =
      let i = get_intensity ?country ?api_code net in
      Option.iter (Prometheus.Gauge.set Metrics.carbon_intensity) i;
      i
    in
    Logs.info (fun f -> f "Monitoring...");
    (* The log file is used for summaries, it should be optional
       if the reporter is defined or not. *)
    let log_file =
      Filename.(temp_file ~temp_dir:(get_temp_dir_name ()) "clarke" ".log")
    in
    Switch.run @@ fun sw ->
    let intensity = ref (get_intensity ()) in
    let (S.Meter ((module M), t) : S.meter) =
      Specs.meter_of_meter_spec ~clock meter
    in
    if not M.supported then Error "Unsupported meter choice, consult the manual"
    else (
      Fiber.all
        [
          Prometheus_eio.serve env prom;
          (fun () ->
            while true do
              Eio_unix.sleep (30. *. 60.);
              intensity := get_intensity ()
            done);
          (fun () ->
            Logs.info (fun f -> f "Reporter log: %s" log_file);
            while true do
              Eio_unix.sleep reporter_period;
              report ~env ~machine reporter_spec log_file
            done);
          (fun () ->
            (* The reporter deletes the file after reporting for similicty
               so it needs to be recreated. *)
            Specs.with_output ~sw ~fs ~stdout ~net output_spec
              (fun (O ((module O), o) : output) ->
                while true do
                  Path.(
                    with_open_out ~append:true ~create:(`If_missing 0o644)
                      (fs / log_file))
                  @@ fun log ->
                  let info = M.collect t in
                  let info = update_info_with_intensity !intensity info in
                  O.send ~machine o info;
                  Flow.copy_string (Info.to_csv info) log;
                  Flow.copy_string "\n" log;
                  Prometheus.Gauge.set Clarke.Metrics.watts (Info.watts info);
                  Eio_unix.sleep (float_of_int period)
                done));
        ];
      Ok ())
  in
  let info = Cmd.info "monitor" in
  Cmd.v info
    Term.(
      const run $ setup_log $ machine_term $ output_spec_term $ meter_spec_term
      $ period_term $ Prometheus_eio.opts $ country_code_term $ api_term
      $ reporter_term $ report_period_term)

let cmds env =
  [
    monitor ~env ~stdout:env#stdout ~net:env#net ~clock:env#clock;
    Server.cmd setup_log ~net:env#net;
    Calc.cmd setup_log env#fs;
  ]

let main_cmd env =
  let doc = "a power monitoring process" in
  let info = Cmd.info "clarke" ~doc in
  let default = Term.(ret @@ const (`Help (`Pager, None))) in
  Cmd.group info ~default (cmds env)

let () =
  Eio_luv.run @@ fun env ->
  exit (Cmd.eval_result (main_cmd (env :> Eio.Stdenv.t)))
