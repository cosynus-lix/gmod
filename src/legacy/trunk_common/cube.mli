(***********************************************************************)
(*                                                                     *)
(*                               ALCOOL                                *)
(*                                                                     *)
(*                              CEA LIST                               *)
(*                                                                     *)
(*                              Cube.mli                               *)
(*                                                                     *)
(***********************************************************************)

module type In =
sig
  module IntSet:
    sig 
      include Set.S
      val segment_initial: ?start:t -> int -> t
    end with type elt = int
  module Array:
  sig
    val cartesian_next_array_consumer: int array -> int -> unit
  end
end with type IntSet.elt = int

module type S =
sig
  type interval
    (** Convex subset of the real line.*)
  type point 
    (** Point of the underlying space.*)
  type underlying
    (** Type in which the values of bounds of intervals range.*)
  type regular_value
    (** Type in which the values of bounds of intervals range.*)
  type ints
    (** Set of integers.*)
  type t
    (** Implementation of cubes.*)
  exception Base2D of ((t list)*(t list)) 
  exception Undefined
  val of_interval: interval -> t
  val compare_points: point -> point -> int
    (** Comparing points in order to use the functor Set.Make.*)
  val above: int -> regular_value -> t -> t (* to be done *) 
  val affine_dimension: t -> int
    (** Affine dimension of a cube i.e. the least integer n such that
	there exists some set of points with n elements whose convex hull
	is the cube given as an argument. *)
  val after: int -> t -> t 
    (** Involved in the calculation of the complement of a cubical
	area. *)
  val atom: point -> t
    (** Turn a point into a singleton that contains it. In particular
	\{p\}=\{p.(1)\}x...x\{p.(n)\} is an atom in the complete
	lattice of cubes. *)
  val are_cofinal: t -> t -> bool
    (** [are_cofinal c1 c2] returns true when any point of [c1] and
	[c2] can be reached from [meet c1 c2] by an [anti-directed]
	path, and false otherwise. *)
  val are_coinitial: t -> t -> bool
    (** [are_coinitial c1 c2] returns true when any point of [c1] and
	[c2] can be reached from [meet c1 c2] by a [directed] path,
	and false otherwise. *)
  val back: int -> t -> t
    (** Back face of a cube. *)
  val back_generalized: bool array -> t -> t
    (** [back_generalized ba c] returns [(back kn...(back k2 (back k1 c)...))] where [k1],...,[kn] are the indices [k] such that [ba.(k)] is [true]. *)
  val base_coordinates: t -> ints (*Globals.*)(*Common.Soi.t*)
    (** Base of a cylinder. If the output is empty then the argument
	is a degenerate cylinder. *)
  val base_dimension: t -> int
    (** Dimension of the base. If the output is null then the argument
	is a degenerate cylinder. *)
  val below: int -> regular_value -> t -> t (* to be done *)
  val before: int -> t -> t 
    (** Involved in the calculation of the complement of a cubical area. *)
  val belongs_to: point -> t -> bool 
    (** Test whether a point belongs to a cube. *)
  val belongs_to_boundary: point -> t -> bool 
    (** Test whether a point belongs to the topological boundary of a
	cube. *)
  val bounded: bool -> bool -> regular_value -> regular_value -> t 
    (** Bounded interval \[a,b\], \]a,b\[, \[a,b\[ ou \]a,b\]. *)
  val cast_shadow_after: int -> t -> t
    (** [cast_shadow_after k c] is the the set of points which do not
	belong to c but that can be reached by an half-line parallel
	to the axis k whose origin belongs to c.*)
  val cast_shadow_before: int -> t -> t
    (** [cast_shadow_before k c] is the the set of points which do not
	belong to c but that can be reached by an half-line parallel
	to the axis k whose end belongs to c.*)
  val closure: t -> t 
    (** Topological closure. *)
  val compare: t -> t -> int
    (** Comparing cubes in order to use the functor Set.Make in
	particular in the module Area. *)
  val compare_interval: interval -> interval -> int
    (** Comparing intervals so the module built over Cube inherits from Interval. *)
  val complement: t -> (t list)
    (** Set theoretic complement of a cube as a list of cubes covering
	it. The covering is normalized. *)
  val cylinder: int -> ((int*interval) array) -> t
    (** An n-dimensional cylinder (1st argument), so that for each k,
	if there exists some ordered pair (k,i) among the entries of
	the array, then the kth interval is i, otherwise the kth
	intervalle is the positive half-line.*)
  val dimension: t -> int
    (** Dimension of the underlying space of the cube, i.e. its rank
	in the graded algebra of cubes. *)
  val downward: ?ai:(int array) -> t -> t 
    (** Downward cone i.e. the set of points which are below some
	points of the cube given as argument. *)
  val degenerate_coordinates: t -> ints (*Common.Soi.t*)
  val disjoint: t -> t -> bool
  val during: int -> t -> t (* Nécessaire au calcul du complémentaire d'une région cubique. *)
  val empty: int -> t
    (** The n-dimensional empty cube. The need for distinguish them
	arise from the graded structure of the algebra : further
	details are provided in the mathematical documentation.  *)
  val face: bool -> int -> t -> t 
    (** Front/back face of a cube. *)
  val front: int -> t -> t 
    (** Front face of a cube. *)
  val front_generalized: bool array -> t -> t
    (** [front_generalized ba c] returns [(front kn...(front k2 (front k1 c)...))] where [k1],...,[kn] are the indices [k] such that [ba.(k)] is [true]. *)
  val full: int -> t 
    (** The n-dimensional full cube. *)
  val ginzu_complement: t -> (t list) 
  val cset2D_complement: t -> (t list)
  val glb: t -> point
    (** Lower corner of the cube. *)
  val greatest_regular_value: t -> regular_value 
  val in_the_future_of: ?it:bool -> t -> t -> t
    (** To be described. *) 
  val in_the_past_of: ?it:bool -> t -> t -> t
    (** To be described. *)
  val in_touch: t -> t -> bool
  val initial: bool -> regular_value -> t (* [0,x] ou [0,x[ selon la valeur true/false du booléen. *)
    (** Renvoie le plus grand ouvert inclus dans le cube passé en
	argument. *)
  val interior: t -> t
    (** Teste si le premier cube est inclus dans le second. *)
  val is_included: t -> t -> bool
  val is_empty: t -> bool 
  val is_not_empty: t -> bool 
  val is_full: t -> bool 
  val is_followed_by: bool array -> t -> t -> bool
  val is_not_full: t -> bool 
    (** Enveloppe cubique de deux cubes. *)
  val join: t -> t -> t  
    (** Coin supérieur du cube. Cette fonction peut lever une exception. *)
  val lub: t -> point 
    (** Intersection ensembliste de deux cubes : c'est toujours un
	cube. *)
  val meet: t -> t -> t 
  val non_degenerate_coordinates: t -> ints (*Common.Soi.t*)
  val not_disjoint: t -> t -> bool
    (** Forme normale d'un cube. *)
  val normalize: t -> t 
    (** Le plus petit point pour l'ordre produit. *)
  val origin: int -> point 
  val of_list: (interval list) -> t
    (** Returns a cube from a list of intervals. If one of them is empty the n-dimensional empty cube is returned with n being the length of the argument *)
  val of_array: (interval array) -> t
    (** Returns a cube from an array of intervals. If one of them is empty the n-dimensional empty cube is returned with n being the length of the argument *)
  val to_list: t -> interval list
    (** Returns the list of intervals the cube is the cartesian product of. Raise an exception if the argument is the empty cube*)
  val to_array: t -> interval array
    (** Returns the array of intervals the cube is the cartesian product of. Raise an exception if the argument is the empty cube*)
  val product: t -> t -> t 
    (** Cartesian product *)
  val projection: ints -> t -> t
  val slash: int -> regular_value -> t -> t (* to be done *)
  val slice: int -> t -> interval
  val strictly_above: int -> regular_value -> t -> t (* to be done *)
  val strictly_below: int -> regular_value -> t -> t (* to be done *)
  val string_of: t -> string (* Display *)
  val string_of_interval: interval -> string (* Display *)
  val terminal: bool -> regular_value -> t (* [x,-[ or ]x,-[ according to the boolean value of the first argument *)
  val underlying: t -> underlying
  val upward: ?ai:(int array) -> t -> t (** Cone over a cube. *)
end

(** Finite products of intervals. Gathers in a single module all the cubes of all the n-dimensional real vector 
spaces for n ranging in the set of natural numbers. A cube is either empty or the product of a family of non empty 
intervals (see Interval).*)

module Make(Overall:In)(I:Interval.S):
  (
    S with type regular_value = I.regular_value
	     and type point = I.regular_value array
	     and type interval = I.t
       and type ints = Overall.IntSet.t
  )
