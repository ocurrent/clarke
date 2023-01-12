open Eio

(** {1 Models}

    Models are functions that produce wattages based on some function. Some primitves
    are provided for construction simple models. They are are {! S.Measurer}s so feel
    free to build your own more elaborate versions.
*)

val const : clock:Time.clock -> float -> S.meter
(** [const ~clock f] is the model which will always report the same energy
    usage. *)

val time : clock:Time.clock -> (Ptime.t -> float) -> S.meter
(** [time ~clock fn] is a model that uses [fn] to model the energy usage.
    [fn] is a function from the current time to the energy usage. *)

(** Variorum-based metric collecting *)
module Variorum : sig
  type t = { clock : Time.clock }

  include S.Meter with type t := t
end

(** IPMI-based metric collecting *)
module Ipmi : sig
  type t = { clock : Time.clock; sensor : string }

  include S.Meter with type t := t
end
