open Eio

module Info = struct
  type t = {
    watts : float;
    timestamp : Ptime.t
  }
  let watts t = t.watts
  let timestamp t = t.timestamp
end

let const ~clock f =
  let module M = struct
    type t = {
      clock : Time.clock;
    }

    let supported = true
    
    module Info = Info
    
    let collect { clock } = Info.{ watts = f; timestamp = Option.get @@ Ptime.of_float_s (Time.now clock) }
  end
  in
  (S.Measurer ((module M : S.Measurer with type t = M.t), M.{ clock }))

let time ~clock f =
  let module M = struct
    type t = {
      clock : Time.clock;
    }

    let supported = true

    module Info = Info

    let collect { clock } = 
      let now = Option.get @@ Ptime.of_float_s (Time.now clock) in
      Info.{ watts = f now; timestamp = now }
  end
  in
  (S.Measurer ((module M : S.Measurer with type t = M.t), M.{ clock }))