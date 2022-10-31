(* Ipmi-based monitor *)
open Eio
open Eio_luv.Low_level

module Ipmi = struct
  let ipmi args = ("sudo", "sudo" :: "ipmitool" :: args)
  let power_consumption = ipmi [ "sensor"; "reading"; "Pwr Consumption"; "-c" ]

  let parse_power_consumption s =
    match String.split_on_char ',' (String.trim s) with
    | "Pwr Consumption" :: watts :: _ -> int_of_string watts
    | _ -> failwith "Couldn't parse the power consumption"
end

type t = { clock : Eio.Time.clock }

let supported = true

let read_all handle buf =
  let rec read acc =
    try
      let i = Eio_luv.Low_level.Stream.read_into handle buf in
      read (acc + i)
    with End_of_file -> acc
  in
  read 0

let get_power_consumption () =
  let cmd, args = Ipmi.power_consumption in
  let parent_pipe = Eio_luv.Low_level.Pipe.init () in
  Switch.run @@ fun sw ->
  let handle = Eio_luv.Low_level.Pipe.to_handle ~sw parent_pipe in
  let buf = Luv.Buffer.create 64 in
  let redirect =
    Eio_luv.Low_level.Process.
      [ to_parent_pipe ~fd:Luv.Process.stdout ~parent_pipe () ]
  in
  let t = Process.spawn ~redirect cmd args in
  let _ = Process.await_exit t in
  let read = read_all handle buf in
  Luv.Buffer.to_string (Luv.Buffer.sub buf ~offset:0 ~length:read)

let collect t =
  let pc =
    get_power_consumption () |> Ipmi.parse_power_consumption |> float_of_int
  in
  Info.v (Option.get (Ptime.of_float_s @@ Time.now t.clock)) pc
