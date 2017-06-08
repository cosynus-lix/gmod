(*
 Pospace - Hyperrectangular Regions in Partially Ordered Euclidean Space
 A component of the topology library TopLib implementing the datatype for a
 compact subset of Euclidean space which is the union of hyperrectangles,
 equipped with the partial order it inherits from R^n.
 
 written by the ALCOOL team, CEA LIST 2007
 *)

type t
(** abstract datatype for a subset of Euclidean space which is the finite union of hyperrectangles whose edges have integral coordinates, which are represented by the datatype int list *)

exception Dimension_mismatch of (int * int)

val null : t
(** Empty set *)

val singleton : int list -> t
(** [singleton x] returns a set having the single point x *)

val dim : t -> Nat.t option
(** [dim x] returns None if x is empty and [Some d] if x lies in R^d. *)

val interval : int list -> int list -> t
(** [interval x y] returns the hyperrectangle whose minimum and maximum points are x and y, respectively, with respect to the standard partial order on Euclidean space, returns the empty set if x and y have different sizes (and thus specify points in different dimensions) *)

val min : t -> int list PSet.t
(** [min x] returns the set of all minima of [x], with respect to the standard partial order on Euclidean space *)

val max : t -> int list PSet.t
(** [max x] returns the set of all maxima of [x], with respect to the standard partial order on Euclidean space *)

val prod : t -> t -> t
(** [prod x y] returns the Cartesian product of [x] and [y]. *)

val mem : int list -> t -> bool
(** [mem e x] returns true iff [e] lies in [x]. *)

val union : t -> t -> t
(** [union x y] returns the union of [x] and [y]. *)

val intersect : t -> t -> t
(** [intersect x y] returns the intersection of [x] and [y]. *)

val minus : t -> t -> t
(** [minus x y] returns [x] \ [y]. *)

val subset : t -> t -> bool
(** [subset x y] returns true iff [x] is a subset of [y]. *)

val proj : t -> Nat.t -> t
(** [proj x n] returns the projection of [x] onto all but the [n]th dimension *)

val lattices : t -> t PSet.t
(** [lattices x] returns the set of maximal continuous sub-lattices of [x].*)

val tex : t -> string
(** Pretty printing. *)
