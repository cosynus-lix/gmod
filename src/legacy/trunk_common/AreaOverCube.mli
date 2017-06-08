(***********************************************************************)
(*                                                                     *)
(*                               ALCOOL                                *)
(*                                                                     *)
(*                              CEA LIST                               *)
(*                                                                     *)
(*                          AreaOverCube.mli                           *)
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

module type S =
sig
  include Area.S
    (** Set of integers.*)
  val above: int -> regular_value -> t -> t (* to be tested *)
  val below: int -> regular_value -> t -> t (* to be tested *)
  val strictly_above: int -> regular_value -> t -> t (* to be tested *)
  val strictly_below: int -> regular_value -> t -> t (* to be tested *)
  val cubic_hull: t -> brick
    (** Smallest cube containing the argument. *)
  val departure: t -> t
    (** Return the area of cubes that cannot be left running along
        an anti-directed path. *)
  val arrival: t -> t
    (** Return the area of cubes that cannot be left running along
        a directed path. *)
  val future_extension: brick -> t -> t
    (** future_extension c a returns the sub-family of a whose
	elements x are such that there exists some directed path
	from c to x whose trace lies in the union of c and x. *)
  val past_extension: brick -> t -> t
    (** past_extension c a returns the sub-family of a whose
	elements x are such that there exists some anti-directed
	path from c to x whose trace lies in the union of c and
	x. *)
  val future_neighborhood: brick -> t -> t
    (** Used in the calculation of the dihomotopy classes of a
	cubical area. *)
  val past_neighborhood: brick -> t -> t
    (** Used in the calculation of the dihomotopy classes of a
	cubical area. *)
  val cubical_order_convex: t -> t
    (** Smallest order-convex area containing the argument. *)
  val deadlock_attractor_weak: t -> t
    (** Same as [deadlock_attractor] but the algorithm implemented is
	the one described in a paper published in Concur98. This
	algorithm requires an extra hypothesis: any pairs of cubes of
	the covering which are in touch actually meets. This property
	is satisfied when all the cubes of the covering are
	closed. Actually [deadlock_attractor_weak a] is included in
	[deadlock_attractor a] hence the function provides an
	underapproximation. *)
  val might_go_infinity_in_all_dimensions: t -> t
  (** [cech_simplex n mcl] returns all the n-simplices of the Čech complex. [mcl] must be the list of maximal subcubes*)
  val cech_simplex: int -> brick array -> (int list * brick) list
  (** [cech_complex a] returns the Čech complex associated to the cubical area [a] given as a normal form *)
  val cech_complex: t -> (int list * brick) list array
  (** Dimension of the Čech complex*)
  val cech_complex_dimension: (int list * brick) list array -> int
end

module Make(Overall:In)(C:Cube.S with type ints = Overall.IntSet.t):
    S
    with type regular_value = C.regular_value 
    and type point = C.point
    and type generator = C.interval 
    and type brick = C.t
    and type ints = C.ints
