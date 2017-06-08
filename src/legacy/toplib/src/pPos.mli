(*
 pPos - Polymorphic posets
 A component of the topology library TopLib implementing the polymorphic poset
 datatype PPos.t
 
 written by the ALCOOL team, CEA LIST 2007
 *)

type 'a t
(** Abstract datatype for a poset of elements having type ['a]. *)

val null : 'a t
(** Empty poset. *)

val mem : 'a -> 'a t -> bool
(** [mem e x] returns true iff [x] contains [e]. *)

val succ : 'a -> 'a t -> 'a PSet.t
(** [succ e x] returns the set of immediate successors to [e] in [x]. *)

val pred : 'a -> 'a t -> 'a PSet.t
(** [pred e x] returns the set of immediate predecessors to [e] in [x]. *)

val up : 'a PSet.t -> 'a t -> 'a PSet.t
(** [up s x] returns the set of all elements in [x] greater than or equal to an
 * element in [s]. *)

val leq : 'a -> 'a -> 'a t -> bool
(** [leq a b x] returns true iff [a] is less than or equal to [b] in [x]. *) 

val add : ('a -> 'a -> int) -> 'a -> 'a t -> 'a t
(** [add f e x] returns poset [x] with [e] added to it if it does not already exist in [x], where the order-relation of [e] to other elements is determined by the comparison function [f], where [f i j]<0 if [i] precedes [j], [f i j]=0 if i is unrelated to [j], and [f i j]>0 if [i] succeeds [j]. *)

val chains: 'a t -> 'a list PSet.t
(** [chains x] returns set of maximal chains of [x], presented as lists. *)

val min : 'a t -> 'a PSet.t
(** [min x] returns set of minima. *)

val max : 'a t -> 'a PSet.t
(** [max x] returns set of maxima. *)

val forget : 'a t -> 'a PSet.t
(** [forget x] returns underlying set of [x]. *)

val discrete : 'a PSet.t -> 'a t
(** [discrete x] returns poset of elements of [x] with trivial order. *)

val order : ('a -> 'a -> int) -> 'a PSet.t -> 'a t
(** [order f x] returns poset of elements of [x] ordered as per [f]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f x] applies [f] to each element of [x].  
 Warning: [f] is assumed to have order-convex fibres. *)

val ord : Nat.t -> Nat.t t
(** [ord n] returns ordinal {0<...<n}. *)

val tex : string t -> string
(** pretty printing *)
