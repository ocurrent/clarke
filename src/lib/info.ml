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

let of_json v =
  let j = Ezjsonm.value_from_string v in
  match
    ( Ezjsonm.find_opt j [ "watts" ],
      Ezjsonm.find_opt j [ "timestamp" ],
      Ezjsonm.find_opt j [ "intensity" ] )
  with
  | Some w, Some ts, Some i ->
      let watts = Ezjsonm.get_float w in
      let timestamp =
        Ezjsonm.get_string ts |> Ptime.of_rfc3339 |> Result.get_ok
        |> fun (v, _, _) -> v
      in
      let intensity = if i = `Null then None else Some (Ezjsonm.get_float i) in
      { watts; timestamp; intensity }
  | _ -> failwith "Malformed carbon information"

let to_csv t =
  Fmt.str "%s,%s,%a"
    (Ptime.to_rfc3339 t.timestamp)
    (string_of_float t.watts)
    Fmt.(option string)
    (Option.map string_of_float t.intensity)
