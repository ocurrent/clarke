module type Info = sig
  type t

  val watts : t -> float

  val timestamp : t -> Ptime.t
end

module type Measurer = sig
  module Info : Info
  type t

  val supported : bool
  (** Runtime check to see if a particular measuring implementation 
      is supported at runtime. *)

  val collect : t -> Info.t
end

type measurer = Measurer : (module Measurer with type t = 'a) * 'a -> measurer

module type Output = sig
  module Info : Info
  type t

  val send : t -> Info.t -> unit
end

module type S = sig
  module Info : Info
  module Measure : Measurer with module Info = Info
  module Output : Output with module Info = Info
end 