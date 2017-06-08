(*
 Predel - Site for Presimplicial sets
 A component of the topology library TopLib implementing the datatype for a
 morphism in the (opposite of a) site for presimplicial sets

 written by the ALCOOL team, CEA LIST 2007
 *)

(** Composition of face operators between simplices. *)
type t

exception Dimension_mismatch of (int * int)

(** total ordering of t; [compare g h] returns [0] iff [g] and [h] represent the same morphism *)
val compare : t -> t -> int

(** [hom m n f a] returns [f(gn (...,f(g2 f(g1 a))...))], where [g1],...,[gn] represent all the face operations from an [m]-simplex to an [n]-simplex, in some random order *)
val hom : Nat.t -> Nat.t -> (t -> 'a -> 'a) -> 'a -> 'a

(** [face i f] composes [f] with the [i]th face operator.
 Raises [Dimension_mismatch((Nat.sgn i),j)] if no such composition is possible,
 where [j] is the closest integer index of a face which does compose with [f] *)
val face : Nat.t -> t -> t

(** [compose f1 f2] returns the composition (f1)(f2).
Raises [Dimension_mismatch(i,j)] if no such composition is possible, where [i]
is the integer value of the codomain of [f1] and [j] is the integer value of the
domain of [f2]. *)
val compose : t -> t -> t

(** [malcev f1 f2 f3] returns [Some(f4)] where [f4] is [f1] with [f2] replaced by [f3] or [None] if this is not possible.
 Note: used to create a rewriting system of morphisms *)
val malcev : t -> t -> t -> t option

(* [dom f] returns the domain of [f]. *)
val dom : t -> Nat.t

(* [codom f] returns the codomain of [f]. *)
val codom : t -> Nat.t 

(* [id n] returns the identity at [n]. *)
val id : Nat.t -> t

(* Pretty printing. *)
val tex : t -> string -> string
