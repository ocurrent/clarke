open Eio
open Clarke

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
  Arg.value @@ Arg.opt Specs.output_spec `Stdout @@ Arg.info [ "o"; "output" ]

let period_term = Arg.value @@ Arg.opt Arg.int 5 @@ Arg.info [ "p"; "period" ]

let monitor ~env ~stdout ~net ~clock =
  let run output meter period prom =
    Switch.run @@ fun sw ->
    let (S.Meter ((module M), t) : S.meter) =
      Specs.meter_of_meter_spec ~clock meter
    in
    if not M.supported then Error "Unsupported meter choice, consult the manual"
    else (
      Fiber.both (Prometheus_eio.serve env prom) (fun () ->
          Specs.with_sink ~sw ~stdout ~net output (fun sink ->
              while true do
                let info = M.collect t in
                Outputs.Flow.send sink info;
                Prometheus.Gauge.set Clarke.Metrics.watts (Info.watts info);
                Eio.Flow.copy_string "\n" sink;
                Eio_unix.sleep (float_of_int period)
              done));
      Ok ())
  in
  let info = Cmd.info "monitor" in
  Cmd.v info
    Term.(
      const run $ output_spec_term $ meter_spec_term $ period_term
      $ Prometheus_eio.opts)

let cmds env = [ monitor ~env ~stdout:env#stdout ~net:env#net ~clock:env#clock ]

let main_cmd env =
  let doc = "a power monitoring process" in
  let info = Cmd.info "clarke" ~doc in
  let default = Term.(ret @@ const (`Help (`Pager, None))) in
  Cmd.group info ~default (cmds env)

let () =
  Eio_luv.run @@ fun env ->
  exit (Cmd.eval_result (main_cmd (env :> Eio.Stdenv.t)))
