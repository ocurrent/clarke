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
  open Eio_luv.Low_level

  module Cmd = struct
    let ipmi args = ("sudo", "sudo" :: "ipmitool" :: args)
    let power_consumption sensor = ipmi [ "sensor"; "reading"; sensor; "-c" ]

    let parse_power_consumption ~sensor s =
      match String.split_on_char ',' (String.trim s) with
      | v :: watts :: _ when v = sensor -> int_of_string watts
      | _ -> failwith ("Couldn't parse the power consumption: " ^ s)
  end

  type t = { clock : Eio.Time.clock; sensor : string }

  let supported = true

  let read_all handle buf =
    let rec read acc =
      match Eio_luv.Low_level.Stream.read_into handle buf with
      | i -> read (acc + i)
      | exception End_of_file -> acc
    in
    read 0

  let get_power_consumption sensor =
    let cmd, args = Cmd.power_consumption sensor in
    Switch.run @@ fun sw ->
    let parent_pipe = Eio_luv.Low_level.Pipe.init ~sw () in
    let buf = Luv.Buffer.create 64 in
    let redirect =
      Eio_luv.Low_level.Process.
        [ to_parent_pipe ~fd:Luv.Process.stdout ~parent_pipe () ]
    in
    let t = Process.spawn ~sw ~redirect cmd args in
    let _ = Process.await_exit t in
    let read = read_all parent_pipe buf in
    Luv.Buffer.to_string (Luv.Buffer.sub buf ~offset:0 ~length:read)

  let collect t =
    let pc =
      get_power_consumption t.sensor
      |> Cmd.parse_power_consumption ~sensor:t.sensor
      |> float_of_int
    in
    Info.v (Option.get (Ptime.of_float_s @@ Time.now t.clock)) pc
end
