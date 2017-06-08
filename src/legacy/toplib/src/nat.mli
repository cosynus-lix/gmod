(*
 Nat - Natural numbers
 A component of the topology library TopLib implementing the natural numbers
 datatype Nat.t

 written by the ALCOOL team, CEA LIST 2007
 *)

type t
(** Abstract datatype for a natural number.
 Note: we can assume type equality is structural equality *)

val succ : t -> t
(** [succ n] returns the successor of n *)

val pred : t -> t
(** [pred n] returns the predecessor of [n] if [n] is non-zero.
Raises [not_Found] if [n] is zero. *)

val sum : t -> t -> t
(** [sum m n] returns [m]+[n]. *)

val prod : t -> t -> t
(** [prod m n] returns [m]*[n]. *)

val zero : t
(** The natural number [0] *)

val one : t
(** The natural number [1] *)

val mag : int -> t
(** [mag i] returns the magnitude, or absolute value, of [i]. *)

val sgn : t -> int
(** [sgn n] returns the positive integer whose absolute value is [n]. *)

val max : t -> t -> t
(** [max m n] returns the maximum of [m] and [n]. *)

val min : t -> t -> t
(** [min m n] returns the minimum of [m] and [n]. *)

val compare : t -> t-> int
(** [compare m n] returns the integer difference [(sgn m)-(sgn n)]. *)

val tex : t -> string
(** [tex n] returns the string form of [n]. *)
