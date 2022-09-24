open Eio

let const ~clock f =
  let module M = struct
    type t = { clock : Time.clock }

    let supported = true

    module Info = Info

    let collect { clock } =
      Info.v (Option.get @@ Ptime.of_float_s (Time.now clock)) f
  end in
  S.Measurer ((module M : S.Measurer with type t = M.t), M.{ clock })

let time ~clock f =
  let module M = struct
    type t = { clock : Time.clock }

    let supported = true

    module Info = Info

    let collect { clock } =
      let now = Option.get @@ Ptime.of_float_s (Time.now clock) in
      Info.v now (f now)
  end in
  S.Measurer ((module M : S.Measurer with type t = M.t), M.{ clock })
