(*
 Seq - Polymorphic sequences
 A component of the topology library TopLib implementing the sequences
 datatype Seq.t

 written by the ALCOOL team, CEA LIST 2007
 *)

type 'a t 
(** abstract datatype of a sequence, either empty or indexed from [Nat.zero] to a natural number, of elements having type ['a] *)

val null : 'a t
(** empty sequence *)

val constant : Nat.t -> 'a -> 'a t
(** [constant n x] returns the sequence of [n] copies of [x]. *)

val access : 'a t -> Nat.t -> 'a
(** [access s n] returns the [n]th element of the sequence [s] (starting from
 Nat.zero); raises [Not_found] if no such element exists. *)

val size : 'a t -> Nat.t
(** [size s] returns the number of elements in [s]. *)

val change : Nat.t -> 'a -> 'a t -> 'a t
(** [change n x s] returns the same sequence as [x], except that the [n]th
element is now [x]; raises [Not_found] if [x] does not have an element indexed at [n]. *)

val expand : 'a -> 'a t -> 'a t
(** [expand x s] returns the same sequence as [s], but with an extra element
[x] attached at the end. *)

val contract : 'a t -> 'a t
(** [contract s] returns the same sequence as [s], but with the last element
 chopped off. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f s] applies [f] to each element of [s]. *)

val prod : 'a t -> 'a t -> ('a option * 'a option) t
(** [prod s t] returns a sequence whose size is the maximum of the sizes of [s]
 and [t], and whose [i]th term is [(Some(access s i),Some(access t i))] if [s]
 and [t] each have an [i]th term, [(Some(access s i),None)] if only [s] has an
 [i]th term, and [(None,Some(i))] if only [t] has an [i]th term.
 Note: [prod], in conjuction with [map] and a function ['a option * 'a option -> 'a], is useful in combining two sequences. *) 

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold f s a] returns [f(sn...f(s2 (f(s0 a)))...)], where [s] is the sequence
([s0],....,[sn]) *) 

val list_from_seq : 'a t -> 'a list
(** type conversion from sequence to list *)

val seq_from_list : 'a list -> 'a t
(** type conversion from list to sequence *)

val concat : 'a t -> 'a t -> 'a t
(** [concat s t] returns [s], followed by [t] *)

val tex : string t -> string
(** pretty printing of a sequence of strings, in LaTex format *)
