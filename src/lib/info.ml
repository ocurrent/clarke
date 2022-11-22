type t = { watts : float; timestamp : Ptime.t; intensity : float option }

let v ?intensity timestamp watts = { timestamp; watts; intensity }
let timestamp t = t.timestamp
let watts t = t.watts
let intensity t = t.intensity

let pp_intensity ppf = function
  | None -> ()
  | Some f -> Fmt.pf ppf "%f gCO2/kWh" f

let pp ppf t =
  Format.fprintf ppf "%a: %f %a" Ptime.pp t.timestamp t.watts pp_intensity
    t.intensity

let to_json t =
  let intensity = match t.intensity with None -> `Null | Some f -> `Float f in
  `O
    [
      ("watts", `Float t.watts);
      ("timestamp", `String (Ptime.to_rfc3339 t.timestamp));
      ("intensity", intensity);
    ]
  |> Ezjsonm.to_string
