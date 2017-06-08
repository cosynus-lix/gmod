(*

module type S = 
  (** Totally ordered set that can be embedded into the real line. The
      function "compare" must be compatible with repect to the functions
      "min" and "max". *)
sig
  type t
  val least_regular_value: t (** least element *)
  val min: t -> t -> t (** binary greatest lower bound *)
  val max: t -> t -> t (** binary least upper bound *)
  val compare: t -> t -> int (** in order to apply functors like Set, Map, etc. *)
  val string_of: t -> string 
  val of_string: string -> t 
end

*)

module Integer:(Sig.Bound with type t = int)

module Float:(Sig.Bound with type t = float) 

module Lexico:
sig
  module Array (HL:Sig.Bound) : (Sig.Bound with type t = (HL.t array))
  module List (HL:Sig.Bound) : (Sig.Bound with type t = (HL.t list))
end

module Graded:
sig
  module Array (HL:Sig.Bound) : (Sig.Bound with type t = (HL.t array))
  module List (HL:Sig.Bound) : (Sig.Bound with type t = (HL.t list))
end
