open Prometheus

let namespace = "Clarke"
let subsystem = "meter"

let watts =
  let help = "Current power usage measured in watts" in
  Gauge.v ~help ~namespace ~subsystem "watts"
