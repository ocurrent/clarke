type t

val v : ?intensity:float -> Ptime.t -> float -> t
val watts : t -> float
val timestamp : t -> Ptime.t
val intensity : t -> float option
val pp : Format.formatter -> t -> unit
val to_json : t -> string
val to_csv : t -> string
