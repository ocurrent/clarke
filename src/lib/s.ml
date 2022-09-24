module type Measurer = sig
  type t

  val supported : bool
  (** Runtime check to see if a particular measuring implementation 
      is supported at runtime. *)

  val collect : t -> Info.t
end

type measurer = Measurer : (module Measurer with type t = 'a) * 'a -> measurer

module type Output = sig
  type t

  val send : t -> Info.t -> unit
end

module type S = sig
  module Measure : Measurer
  module Output : Output
end
