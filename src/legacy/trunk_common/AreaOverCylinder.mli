(***********************************************************************)
(*                                                                     *)
(*                               ALCOOL                                *)
(*                                                                     *)
(*                              CEA LIST                               *)
(*                                                                     *)
(*                         AreaOverCylinder.mli                        *)
(*                                                                     *)
(***********************************************************************)

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
    
module Make(Overall:In)(X:Cylinder.S with type ints = Overall.IntSet.t):
    S
    with type regular_value = X.regular_value 
    and type point = X.point
    and type generator = X.generator
    and type brick = X.t
    and type ints = X.ints
