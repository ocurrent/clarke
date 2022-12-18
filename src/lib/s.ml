module type Meter = sig
  type t

  val supported : bool
  (** Runtime check to see if a particular measuring implementation 
      is supported at runtime. *)

  val collect : t -> Info.t
end

type meter = Meter : (module Meter with type t = 'a) * 'a -> meter

module type Output = sig
  type t

  val send : machine:string -> t -> Info.t -> unit
  (** [send ~machine t info] sends information [info] to [t] about the [machine]. *)
end

module type S = sig
  module Meter : Meter
  module Output : Output
end
