(*
 sSet - Polymorphic Presimplicial Sets
 A component of the topology library TopLib implementing the datatype for a
 labelled presimplicial set

 written by the ALCOOL team, CEA LIST 2007
 *)

type o
(** Abstract datatype for a composition of face operators. *)

type 'a t 
(** Abstract datatype for a presimplicial set, some of whose simplices -
 including all of its maximal simplices - in each dimension are labelled by
 elements of type ['a].
 
 A simplex inside a presimplicial set is represented by elements of type [o*'a].
 For example, given values [f:o] and [l:'a], [l] would represent a labelled
 simplex in a presimplicial set and [(f,l)] would represented the application of
 [f] to that simplex.
 * *)

exception Dimension_mismatch of (int*int)

val null : 'a t
(** Empty presimplicial set. *)

val standard : Nat.t -> 'a -> 'a t
(** [standard n l] returns the standard [n]-simplex with the unique [n]-simplex
labelled as [(l,n)] *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f x] applies [f] to each label in [x], quotienting [x] wherever [f]
* identifies two similarly labeled simplices in [x]. *)

val union : 'a t -> 'a t -> 'a t
(** [union x y] returns the coproduct of [x] and [y], quotiented by the equivalence relation identifying similarly labelled simplices *)

val quotient : 'a t -> o -> 'a -> o -> 'a -> 'a t
(** [quotient x f1 l1 f2 l2] returns the quotient of [x] by the equivalence
relation [(f1,l1)]~[(f2,l2)]. *)

val simplices : 'a t -> Nat.t -> (o * 'a) PSet.t
(** [simplices x n] returns the set of all [n]-simplices of [x]. *)

val solve : 'a t -> o -> o -> 'a -> (o * 'a) PSet.t
(** [solve x f g l] returns the preimage of the simplex [(g,l)] under [f] in [x], returns the empty set if the simplex [(g,l)] does not lie in [x] *)

val dim : 'a t -> Nat.t option
(** [dim x] returns [None] if [x] is empty and [Some(n)] if [x] has dimension [n]. *)

val mem : o -> 'a -> 'a t -> bool
(** [mem f l x] returns true iff [(f,l)] lies inside [x]. *)

val fold : (o -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold f x a] returns [f(fn ln f(f2 l2 (f(f1 l1 a)))...)], where
[(f1,l1)],...[(fn,ln)] are all the labelled simplices inside [x]. *)

val tex : string t -> string
(** [tex x] returns a description of [x] in LaTex format. *)
