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
