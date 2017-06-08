(*
 pSet - Polymorphic sets
 A component of the topology library TopLib implementing the polymorphic set
 datatype PSet.t
 
 written by the ALCOOL team, CEA LIST 2007
 *)

type 'a t
(** Abstract datatype for a set of elements having type ['a]. *)

val empty : 'a t
(** Empty set *)

val is_empty : 'a t -> bool
(** [is_empty s] returns true iff [s] is the empty set *) 

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold f s a] returns [f(en .... f(e2 (f(e1 a)))...)], where [s] is the set with elements [{e1,...,en}] in some random order *)

val card : 'a t -> Nat.t
(** [card s] returns the cardinality of [s] *)

val mem : 'a -> 'a t -> bool
(** [mem e s] returns true iff [s] contains [e] *)

val subset : 'a t -> 'a t -> bool
(** [subset s t] returns true iff [t] contains [s] *)

val compare : 'a t -> 'a t -> int
(** Total ordering of datatype [t]; [compare s t] returns 0 iff [s] and [t] represent the same set *)

val add : 'a -> 'a t -> 'a t
(** [add e s] returns [s] with [e] added to it *)

val singleton : 'a -> 'a t
(** [singleton e] returns the singleton containing [e]. *)

val remove : 'a -> 'a t -> 'a t
(** [remove e s] removes [e] from [s]; raises [Not_found] if [e] does not belongto [s] *)

val union : 'a t -> 'a t -> 'a t
(** [union s t] returns union of [s] and [t] *)

val minus : 'a t -> 'a t -> 'a t
(** [minus s t] returns [s] minus [t]; it does not raise an exception if [t] is not a subset of [s] *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f s] applies [f] to each element of [s] *)

val choice : 'a t -> 'a
(** [choice x] randomly returns an element of x, raises Not_found if [x] is
    empty. *)

val prod : 'a t -> 'a t -> ('a * 'a) t
(** [prod x y] returns the Cartesian product of [x] and [y] *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f m] applies [f] to all bindings in map [m].
    The order in which the bindings are passed to [f] is unspecified. *)

val tex : string t -> string
(** Pretty printing. *)
