module Flow = struct
  type t = Eio.Flow.sink

  let send ~machine:_ sink info =
    Eio.Flow.copy_string (Info.to_json info) sink;
    Eio.Flow.copy_string "\n" sink
end

module Capnp = struct
  type t = Capnp_client.t

  let send ~machine cap info =
    match Capnp_client.report cap ~machine info with
    | Ok () -> ()
    | Error (`Msg m) -> failwith m
end
