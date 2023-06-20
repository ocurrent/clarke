open Eio

let const ~clock f =
  let module M = struct
    type t = { clock : Time.clock }

    let supported = true

    module Info = Info

    let collect { clock } =
      Info.v (Option.get @@ Ptime.of_float_s (Time.now clock)) f
  end in
  S.Meter ((module M : S.Meter with type t = M.t), M.{ clock })

let time ~clock f =
  let module M = struct
    type t = { clock : Time.clock }

    let supported = true

    module Info = Info

    let collect { clock } =
      let now = Option.get @@ Ptime.of_float_s (Time.now clock) in
      Info.v now (f now)
  end in
  S.Meter ((module M : S.Meter with type t = M.t), M.{ clock })

module Variorum = struct
  open Variorum
  open Eio

  type t = { clock : Time.clock }

  let supported = Variorum.supported

  let collect t =
    match Node_power.get () with
    | Error (`Msg m) -> failwith m
    | Ok s ->
        Info.v
          (Option.get (Ptime.of_float_s @@ Time.now t.clock))
          (Node_power.power_node s)
end

module Ipmi = struct
  module Cmd = struct
    let ipmi args = ("sudo", "sudo" :: "ipmitool" :: args)
    let power_consumption sensor = ipmi [ "sensor"; "reading"; sensor; "-c" ]

    let parse_power_consumption ~sensor s =
      match String.split_on_char ',' (String.trim s) with
      | v :: watts :: _ when v = sensor -> int_of_string watts
      | _ -> failwith ("Couldn't parse the power consumption: " ^ s)
  end

  type t = {
    clock : Eio.Time.clock;
    process_mgr : Eio_unix.Process.mgr;
    sensor : string;
  }

  let supported = true

  let get_power_consumption ~process_mgr sensor =
    let cmd, args = Cmd.power_consumption sensor in
    Logs.debug (fun f -> f "command: %a" Fmt.(list string) args);
    Eio.Process.parse_out process_mgr Buf_read.take_all (cmd :: args)

  let collect t =
    let pc =
      get_power_consumption ~process_mgr:t.process_mgr t.sensor
      |> Cmd.parse_power_consumption ~sensor:t.sensor
      |> float_of_int
    in
    Info.v (Option.get (Ptime.of_float_s @@ Time.now t.clock)) pc
end
