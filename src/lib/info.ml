type t = { watts : float; timestamp : Ptime.t }

let v timestamp watts = { timestamp; watts }
let timestamp t = t.timestamp
let watts t = t.watts
let pp ppf t = Format.fprintf ppf "%a: %f" Ptime.pp t.timestamp t.watts

let to_json t =
  Format.fprintf Format.str_formatter {|{"watts": %f, "timestamp": "%a"}|}
    t.watts (Ptime.pp_rfc3339 ()) t.timestamp;
  Format.flush_str_formatter ()
