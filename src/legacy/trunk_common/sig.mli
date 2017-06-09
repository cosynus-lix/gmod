(** Compendium of signatures. *)

(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                             Bound                               *)
(*                                                                 *)
(* --------------------------------------------------------------- *)

(** Totally ordered sets. *)
module type Bound =
sig
  (** Totally ordered set that can be embedded into the real line. The
      function "compare" must be compatible with repect to the
      functions "min" and "max". *)
  type t

  (** Least element. *)
  val least_regular_value : t

  (** Greatest lower bound. *)
  val min: t -> t -> t

  (** Least upper bound. *)
  val max : t -> t -> t

  (** Comparison function on elements. *)
  val compare: t -> t -> int

  (** String representation. *)
  val string_of: t -> string

  (** From a string representation. *)
  val of_string: string -> t
end

(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                             Interval                            *)
(*                                                                 *)
(* --------------------------------------------------------------- *)



(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                               Arc                               *)
(*                                                                 *)
(* --------------------------------------------------------------- *)



(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                               Mix                               *)
(*                                                                 *)
(* --------------------------------------------------------------- *)


(** {2 Elementary blocks for constructing areas.} *)

(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                             Generic                             *)
(*                                                                 *)
(* --------------------------------------------------------------- *)

(** Module type of the parameter for Area.Make.OverGeneric. A module
  Generic should also be written so as to be able to create a
  module of type generic from a "directed one dimensional
  structure"
  *)
module type Block =
sig
  (** An elementary block. *)
  type t

  (** Underlying space (necessary for example to compute complements). *)
  type underlying

	(** Returns the underlying space*)
	val underlying: t -> underlying
	
  (** A point in the topological space (the geometrical realization). *)
  type point

  (** Operation not defined on given input. *)
  exception Undefined

  (** Comparison function on points. *)
  val compare_points : point -> point -> int

  (** Comparison function on blocks. *)
  val compare : t -> t -> int

  (** Block containing only one point. *)
  val atom : underlying -> point -> t

  (** Complement of a block. *)
  val complement : underlying -> t -> t list

  (** Cartesian product of two blocks. *)
  val product : t -> t -> t

  (** Intersection of two blocks. *)
  val intersection : t -> t -> t list

  (** Empty block. *)
  val empty : underlying -> t

  (** Block covering the whole space. *)
  val full : underlying -> t

  (** Is a point in the block? *)
  val belongs_to : point -> t -> bool

  (** Topological interior. *)
  val interior : t -> t

  (** Topological closure. *)
  val closure : t -> t

  (** Is the block empty? *)
  val is_empty : t -> bool

  (** Is the block covering the whole space? *)
  val is_full : underlying -> t -> bool

  (** [is_included u v] returns true when [u] is include in [v], false otherwise. *)
  val is_included: t -> t -> bool

  (** Subset of the points of the second argument that can be reached
    from a point of the first argument running along a directed path
    whose trace is included in the set theoretic union of both
    arguments. *)
  val in_the_future_of : t -> t -> t

  val in_the_past_of : t -> t -> t

  (** Human-readable representation. *)
  val string_of : t -> string

  (** The union of the two blocks is connected. *)
  val in_touch : t -> t -> bool

  (** Maximal points. *)
  val maximal : t -> point list

  (** Minimal points. *)
  val minimal : t -> point list
end

module type Cartesian_block =
sig
  include Block

  (** Generators for elementary blocks (intervals, arcs, etc.). *)
  type generator

  val make: generator list -> t
  val of_list: generator list -> t
  val of_array: generator array -> t
  val dimension: t -> int

  (** Empty space. *)
  val zero : underlying

  (** [sub m n u] projects on coordinates [m] to [n]. *)
  val sub : int -> int -> underlying -> underlying

  (** [add u v] computes the cartesian product of [u] with [v]. *)
  val add : underlying -> underlying -> underlying
end

module type Generic = Cartesian_block

(** {2 Built-in cartesian blocks for constructing areas} *)
(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                              Cube                               *)
(*                                                                 *)
(* --------------------------------------------------------------- *)

(* module type Cube = sig end *)

(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                              Tore                               *)
(*                                                                 *)
(* --------------------------------------------------------------- *)

(* module type Tore = sig end *)

(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                             Cylinder                            *)
(*                                                                 *)
(* --------------------------------------------------------------- *)

(* module type Cylinder = sig end *)

(** {2 Area} *)
(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                              Area                               *)
(*                                                                 *)
(* --------------------------------------------------------------- *)

(* module type Area = sig end *)

(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                           CubicalArea                           *)
(*                                                                 *)
(* --------------------------------------------------------------- *)

(* module type CubicalArea = sig end (* CubicalArea *) *)

(** {2 Algebra} *)
(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                              Ring                               *)
(*                                                                 *)
(* --------------------------------------------------------------- *)

module type Ring = 
sig
  type t
  val compare: t -> t -> int
  val default: t 
  val of_string: string -> t
  val zero: t
  val one: t
  val neg: t -> t
  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val name: unit -> string
end

(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                          MatrixAlgebra                          *)
(*                                                                 *)
(* --------------------------------------------------------------- *)

module type MatrixAlgebra = 
sig
  include Ring
  type s 
  val scp: s -> t -> t
  val entry: int -> int -> t -> s
  val trans: t -> t
end

(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                          FactorialRing                          *)
(*                                                                 *)
(* --------------------------------------------------------------- *)

module type FactorialRing =
sig
  include Ring
  val is_prime: t -> bool
  val factorization: t -> ((t*int) list)
  val string_of_factorization: ?utf8:bool -> ((t*int) list) -> string
end

(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                          EuclideanRing                          *)
(*                                                                 *)
(* --------------------------------------------------------------- *)

module type EuclideanRing =
sig
  include FactorialRing
  val div: t -> t -> t
  val rst: t -> t -> t
end

(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                              Field                              *)
(*                                                                 *)
(* --------------------------------------------------------------- *)

module type Field =
sig
  include EuclideanRing
  val inv: t -> t
end

(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                          PolynomialRing                         *)
(*                                                                 *)
(* --------------------------------------------------------------- *)

module type PolynomialRing =
  functor (R:Ring) ->
sig
  include Ring
  val degree: t -> int
  val monomial: ?coef:R.t -> int -> t
  val coefficient: int -> R.t
end

(* --------------------------------------------------------------- *)
(*                                                                 *)
(*                     PolynomialFactorialRing                     *)
(*                                                                 *)
(* --------------------------------------------------------------- *)

module type PolynomialFactorialRing =
  functor (R:FactorialRing) ->
sig
  include FactorialRing
  val degree: t -> int
  val monomial: ?coef:R.t -> int -> t
  val coefficient: int -> R.t
end
