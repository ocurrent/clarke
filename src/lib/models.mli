open Eio

(** {1 Models}

    Models are functions that produce wattages based on some function. Some primitves
    are provided for construction simple models. They are are {! S.Measurer}s so feel
    free to build your own more elaborate versions.
*)

module Info : S.Info

val const : clock:Time.clock -> float -> S.measurer
(** [const ~clock f] is the model which will always report the same energy
    usage. *)

val time : clock:Time.clock -> (Ptime.t -> float) -> S.measurer
(** [time ~clock fn] is a model that uses [fn] to model the energy usage. 
    [fn] is a function from the current time to the energy usage. *)