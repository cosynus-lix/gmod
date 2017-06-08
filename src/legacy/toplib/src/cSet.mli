(*
 cSet - Polymorphic Precubical Sets
 A component of the topology library TopLib implementing the datatype for a
 labelled precubical set

 written by the ALCOOL team, CEA LIST 2007
 *)

type o = Prebox.t
(** Abstract datatype for a composition of face operators *)

type 'a t 
(** Abstract datatype for a precubical set, some of whose cubes - including all
of its maximal cubes - in each dimension are labelled by elements of type ['a].
A cube inside a precubical set is represented by elements of type [o * 'a].  
For example, given values [f:o] and [l:'a], [l] would represent a labelled cube
of dimension given by the domain of [f], and [(f,l)] would represented the
application of [f] to that cube.
 *)

exception Dimension_mismatch of (int*int)

val null : 'a t
(** Empty precubical set *)

val standard : Nat.t -> 'a -> 'a t
(** [standard n l] returns the standard [n]-cube with the unique [n]-cube labelled as [l]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f x] applies [f] to each label in [x], quotienting x in the process wherever f identifies two labelled cubes in [x] in the same dimension *)

val union : 'a t -> 'a t -> 'a t
(** [union x y] returns the coproduct of [x] and [y], quotiented by the
 equivalence relation identifying similarly labelled cubes *)

val quotient : 'a t -> o -> 'a -> o -> 'a -> 'a t
(** [quotient x f1 l1 f2 l2] returns the quotient of [x] by the equivalence
relation [(f1,l1)]~[(f2,l2)] *)

val cubes : 'a t -> Nat.t -> (o * 'a) PSet.t
(** [cubes x n] returns the set of all n cubes of x *)

val solve : 'a t -> o -> o -> 'a -> (o * 'a) PSet.t
(** [solve x f g l] returns the preimage of the cube [(g,l)] under [f] in [x],
 * returns the empty set if the cube [(g,l)] does not lie in [x] *)

val dim : 'a t -> Nat.t option
(** [dim x] returns None if [x] is empty and [Some(n)] if [x] has dimension [n].*)

val mem : o -> 'a -> 'a t -> bool
(** [mem f l x] returns true iff [(f,l)] lies inside [x] *)

val fold : (o -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold f x a] returns [f(fn ln f(f2 l2 (f(f1 l1 a)))...)], where
[(f1,l1)],...[(fn,ln)] are all the labelled cubes inside [x] *)

val tex : string t -> string
(** Pretty printing *)
