module Flow = struct
  type t = Eio.Flow.sink

  let send sink info = Eio.Flow.copy_string (Info.to_json info) sink
end
