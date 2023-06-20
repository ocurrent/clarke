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

module Reporter : sig
  module type S = sig
    type conf
    (** A configuration for the reporter *)

    val report :
      machine:string -> conf -> t -> (unit, [ `Msg of string ]) result
    (** Report a summary [t] about a [machine] *)
  end

  type t =
    | Reporter : ((module S with type conf = 'a) * 'a) -> t
        (** Generic reporters *)

  module File : S with type conf = Eio.Fs.dir Eio.Path.t
  (** A filesystem reporter that writes the summaries into a file. *)

  module Stdout : S with type conf = Eio.Flow.sink
  (** A generic stdout reporter *)

  module Slack : S with type conf = Eio.Net.t * string
  (** A slack reporter that seconds summaries to slack *)

  type spec = [ `File of string | `Slack of string | `Stdout ]
  (** Useful for cmdliner *)

  val of_spec :
    fs:Eio.Fs.dir Eio.Path.t ->
    net:Eio.Net.t ->
    stdout:Eio_unix.sink ->
    spec ->
    t

  val spec_of_string : string -> spec
  (** Default to [`Stdout]. *)

  val pp_spec : spec Fmt.t
  (** Pretty print a spec. *)
end
