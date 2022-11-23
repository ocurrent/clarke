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

let with_socket ~sw net v fn =
  let listener = Net.listen ~sw ~backlog:5 net v in
  while true do
    Net.accept_fork ~sw listener ~on_error:(fun e ->
        traceln "Err: %s" (Printexc.to_string e))
    @@ fun socket _addr -> fn (socket :> Flow.sink)
  done

module Specs = struct
  type output_spec = [ `Stdout | Net.Sockaddr.stream ]

  let with_sink ~sw ~stdout ~net spec fn =
    match spec with
    | `Stdout -> fn (stdout :> Flow.sink)
    | #Net.Sockaddr.stream as v -> with_socket ~sw net v fn

  let output_spec_of_string s =
    match String.lowercase_ascii s with
    | "stdout" -> Ok `Stdout
    | v -> (
        match String.split_on_char ':' v with
        | [ "tcp"; addr; port ] -> (
            try
              let addr =
                if addr = "loopback" then Net.Ipaddr.V4.loopback
                else Net.Ipaddr.of_raw addr
              in
              Ok (`Tcp (addr, int_of_string port))
            with _ -> Error (`Msg "Port parsing failed"))
        | [ "unix"; addr ] -> Ok (`Unix addr)
        | _ -> Error (`Msg ("Unknown " ^ v)))

  let pp_output_spec ppf = function
    | `Stdout -> Format.pp_print_string ppf "stdout"
    | #Net.Sockaddr.stream as v -> Net.Sockaddr.pp ppf v

  let output_spec = Cmdliner.Arg.conv (output_spec_of_string, pp_output_spec)

  type meter_spec = [ `Const of float | `Variorum ]

  let meter_of_meter_spec ~clock = function
    | `Const f -> Models.const ~clock f
    | `Ipmi -> S.Meter ((module Clarke.Models.Ipmi), { clock })
    | `Variorum -> S.Meter ((module Clarke.Models.Variorum), { clock })

  let meter_spec_of_string s =
    match String.lowercase_ascii s with
    | "variorum" -> Ok `Variorum
    | "ipmi" -> Ok `Ipmi
    | v -> (
        match String.split_on_char ':' v with
        | [ "const"; f ] -> (
            try Ok (`Const (float_of_string f))
            with _ -> Error (`Msg "Float parsing failed"))
        | _ -> Error (`Msg ("Unknown " ^ v)))

  let pp_meter_spec ppf = function
    | `Const f -> Format.fprintf ppf "const:%f" f
    | `Variorum -> Format.pp_print_string ppf "variorum"
    | `Ipmi -> Format.pp_print_string ppf "ipmi"

  let meter_spec = Cmdliner.Arg.conv (meter_spec_of_string, pp_meter_spec)
end

(* Command Line *)
open Cmdliner

let meter_spec_term =
  Arg.required
  @@ Arg.opt Arg.(some Specs.meter_spec) None
  @@ Arg.info [ "m"; "meter" ]

let output_spec_term =
  Arg.value
  @@ Arg.opt Specs.output_spec `Stdout
  @@ Arg.info ~doc:"The output location of the monitoring data"
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
        Carbon.Gb.get_intensity net
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
      let v = Carbon.Co2_signal.v api in
      let i = Co2_signal.get_intensity ~net v ~country_code in
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

let monitor ~env ~stdout ~net ~clock =
  let run () output meter period prom country api_code =
    let api_code =
      Option.map (fun f -> Path.(load (env#fs / f) |> String.trim)) api_code
    in
    let get_intensity () =
      let i = get_intensity ?country ?api_code net in
      Option.iter (Prometheus.Gauge.set Metrics.carbon_intensity) i;
      i
    in
    Logs.info (fun f -> f "Monitoring...");
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
            Specs.with_sink ~sw ~stdout ~net output (fun sink ->
                while true do
                  let info = M.collect t in
                  let info = update_info_with_intensity !intensity info in
                  Outputs.Flow.send sink info;
                  Prometheus.Gauge.set Clarke.Metrics.watts (Info.watts info);
                  Eio.Flow.copy_string "\n" sink;
                  Eio_unix.sleep (float_of_int period)
                done));
        ];
      Ok ())
  in
  let info = Cmd.info "monitor" in
  Cmd.v info
    Term.(
      const run $ setup_log $ output_spec_term $ meter_spec_term $ period_term
      $ Prometheus_eio.opts $ country_code_term $ api_term)

let cmds env =
  [
    monitor ~env ~stdout:env#stdout ~net:env#net ~clock:env#clock;
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
