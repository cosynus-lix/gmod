(***********************************************************************)
(*                                                                     *)
(*                               ALCOOL                                *)
(*                                                                     *)
(*                              CEA LIST                               *)
(*                                                                     *)
(*                               mix.mli                               *)
(*                                                                     *)
(***********************************************************************)

module type S =
sig
  exception Unbounded
  exception Undefined
  type regular_value
    (** Admissible values for bounds *)
  val compare_regular_values: regular_value -> regular_value -> int 
  type t
    (** Either interval or arc. *)
  exception Split of t * t
  type underlying
  val line: underlying
  val circle: underlying
  val underlying: t -> underlying
  val least_regular_value: regular_value 
    (** Least admissible value *)
  val empty: underlying -> t
    (** Either empty interval or empty arc *)
  val full: underlying -> t
    (** Either full interval or full arc *)
  val compare: t -> t -> int
    (** Total order over the type regular_value. The output is strictly
	negative (strictly positive) if the first (second) argument is
	strictly less than the second (first) one, it is null if and
	only if the arguments are equal *)
  val atom: underlying -> regular_value -> t
    (** Singleton whose only element is given by the argument, it has
	to be specified whether it is containded in an interval or an
	arc. *)
  val terminal: bool -> regular_value -> t
    (** Either the final segment \[x,~ \[ or \]x,~ \[ depending on the
	arguments or The terminal segment of the directed circle, either
	(x,0( or )x,0( according to the boolean argument arguments. The
	symbol ~ means "unbounded on the right", which cannot be
	represented by any value of type regular_value *)
  val initial: bool -> regular_value -> t
    (** Either the initial segment \[0,x\] or \[0,x\[ depending on the
	arguments or the initial segment of the directed circle, namely
	(0,x) or (0,x( according to the boolean argument arguments. *)
  val bounded: bool -> bool -> regular_value -> regular_value -> underlying -> t 
    (** Either \[a,b\], \]a,b\[, \[a,b\[, \]a,b\] (a,b), )a,b(, (a,b(
	or )a,b) depending on the arguments *)
  val belongs_to: regular_value -> t -> bool
    (** Test whether a regular_value belongs to an interval/arc. *)
  val is_included: t -> t -> bool
    (** Test whether the first argument is included in the second one *)
  val glb: t -> regular_value 
    (** Greatest lower bound of an interval/arc. The exception
	Undefined is raised if and only if the argument is empty an
	empty interval/arc or the full circle. The notion is sound
	once a direction on the circle has been chosen. *)
  val lub: t -> regular_value 
    (** Least upper bound of an interval/arc. The exception Undefined is
	raised if and only if the argument is a final interval, the
	empty arc or the circle.The notion is sound once a direction on
	the circle has been chosen. *)
  val is_empty: t -> bool
    (** The output value is true if and only if the argument is
	empty *)

  val is_full: t -> bool
    (** The output value is true if and only if the argument is
	full *)
  val is_not_empty: t -> bool
    (** The output value is true if and only if the argument is not
	empty *)
  val is_not_full: t -> bool
    (** The output value is true if and only if the argument is not
	full *)
  val is_atomic: t -> bool
  val in_touch: t -> t -> bool
    (** The output value is true if and only if the set theoretic union
	of both arguments is connected *)
  val meet: t -> t -> t 
    (** Set theoretic intersection of both arguments. May raise the
	exception Split in the case when both arguments are arcs. *)
  val intersection: t -> t -> t 
  val join: t -> t -> t 
  val not_disjoint: t -> t -> bool (* to be done *)
  val string_of: t -> string
  val interior: t -> t
    (** Topological interior of the argument with respect to the underlying 
	topology of the argument *)
  val closure: t -> t
    (** Topological closure of the argument with respect to the
	topology of the positive half-line *)
  val normalize: t -> t
    (** Provide the normal form of an interval with respect to their
	implementation. Mostly for internal use. *)
  val complement: t -> t 
    (** Return the set theoretic complement of an interval/arc. The
	exception Split is raised if and only if it is not connected,
	in other words when the argument is a bounded interval which
	do not contain the least regular value, or equivalntly when
	both before and after return an non empty interval. *)
  val before: t -> t
    (** Set of all lower bounds of the argument which do not belong to
	it *)
  val after: t -> t
    (** Set of all upper bounds of the argument which do not belong to
	it *)
  val downward: t -> t 
  val upward: t -> t
  val in_the_future_of: t -> t -> t 
  val in_the_past_of: t -> t -> t 
  val embed: t -> t
end


(** Functor which builds a module in which both intervals and arcs can
be handled. *)

module Make: 
sig 
  module FromHalfLine(B:Sig.Bound):S with type regular_value = B.t
 end
