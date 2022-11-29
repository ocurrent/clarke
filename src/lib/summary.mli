type t
(** A summary of energy and emissions over a period of time *)

val total_time : t -> int64
(** [total_time t] is how long we were monitoring in nanoseconds. *)

val total_energy : t -> float
(** Total energy consumed measured in kilojoules. *)

val total_emissions : t -> float option
(** Total emissions measures in grams of CO{_2}-eq. *)

val summary :
  ?format:[ `Json | `Csv ] -> Eio.Buf_read.t -> (t, [ `Msg of string ]) result
(** [summary ?format buf] reads [buf] line by line using [format]
    to produce some summary stats. *)

val pp : t Fmt.t
(** A pretty printer for summaries *)
