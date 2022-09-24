type t

val v : Ptime.t -> float -> t
val watts : t -> float
val timestamp : t -> Ptime.t
val pp : Format.formatter -> t -> unit
val to_json : t -> string
