(*
 Space - Module for Polymorphic Polytopes
 A component of the topology library TopLib wrapping the functor Space.Make
 which implements the datatype for a presheaf over a PRO

 written by the ALCOOL team, CEA LIST 2007
 *)

module type TOPE =
  sig
   type t
   val compare : t -> t -> int
   (** total ordering of t; [compare g h] returns 0 iff g and h represent the
    same operation *)
   val hom : Nat.t -> Nat.t -> (t -> 'a -> 'a) -> 'a -> 'a
   (** [hom m n f a] returns [f(gn,(...,f(g2,f(g1,a))...))], where
   [g1,...,gn] represent all the operations from an [m]-tope to an [n]-tope,
    in some random order. *)
   val malcev : t -> t -> t -> t option
   (** [malcev f1 f2 f3] returns [Some(f4)] where [f4] is [f1] with [f2] 
    replaced by [f3] or [None] if this is not possible.
    Note: used to create a rewriting system of operations *)
   val id : Nat.t -> t
   (** [id n] returns the identity operation on an [n]-tope *)
   val compose : t -> t -> t
   (** [compose f1 f2] returns the composite [(f1)(f2)] of operations *)
   val dom : t -> Nat.t
   (** [dom f] returns the degree of the tope on which [f] is defined *)
   val codom : t -> Nat.t
   (** [codom f] returns the degree of the topes in which [f] takes values *)
   val tex : t -> string -> string
   (** [tex f l] returns a LaTex pretty-orinting of the operator [f] applied to
    a tope labelled [l] *)
  end
(** signature of input module, implementing the abstract datatype of a "shape" operator on basic topes of a polytope.
 Examples of such operators are the morphisms of the category Ord of finite ordinals, used to define simplicial sets *)

module type POLYTOPE = 
  sig
    type o
    type 'a t 
    exception Dimension_mismatch of (int*int)
    val null : 'a t
    val standard : Nat.t -> 'a -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val union : 'a t -> 'a t -> 'a t
    val quotient : 'a t -> o -> 'a -> o -> 'a -> 'a t
    val topes : 'a t -> Nat.t -> (o * 'a) PSet.t
    val solve : 'a t -> o -> o -> 'a -> (o * 'a) PSet.t
    val dim : 'a t -> Nat.t option
    val mem : o -> 'a -> 'a t -> bool
    val fold : (o -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val tex : string t -> string
  end 

module Make : functor (Tope : TOPE) -> POLYTOPE with type o = Tope.t
(** Functor creates a module implementing a polymorphic polytope, given the data of a shape encoded in Tope *)

