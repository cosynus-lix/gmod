(***********************************************************************)
(*                                                                     *)
(*                               ALCOOL                                *)
(*                                                                     *)
(*                              CEA LIST                               *)
(*                                                                     *)
(*                          AreaOverTorus.mli                          *)
(*                                                                     *)
(***********************************************************************)

(** Graded Boolean algebra of cubical areas over finite dimensional
    real vector spaces and finite dimensional tori. Unless otherwise states, the arguments should be (pre)normal 
    forms.*)

module type In =
sig
  module IntSet:Set.S with type elt = int
  module Array:
    sig
      val for_all: ('a -> bool) -> ('a array) -> bool  
    end
  module List:
    sig
      val initial_segment: ?start:int -> int -> int list    
    end
end with type IntSet.elt = int

module type S = Area.S
    
module Make(Overall:In)(T:Torus.S(*with type ints = Overall.IntSet.t*)):
    S
    with type regular_value = T.regular_value 
    and type point = T.point
    and type generator = T.arc
    and type brick = T.t
    (*and type ints = T.ints*)
