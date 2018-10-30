
(***********************************************************************)
(*                                                                     *)
(*                               ALCOOL                                *)
(*                                                                     *)
(*                              CEA LIST                               *)
(*                                                                     *)
(*                            interval.mli                             *)
(*                                                                     *)
(***********************************************************************)

(** Convex subsets of the positive half-line. Given a totally ordered
    type that can be embedded in the positive half-line (which has a
    least element and no greatest one), the resulting module is an
    implementation of the structure formed by the intervals of the
    positive half-line. *)

(** Intervals. Gives both lattice and boolean algebra structures over
    the collection of intervals of the positive half-line. *)
module type S =
sig
  (** Raised when an operation is not defined on an input. *)
  exception Undefined

  (** Admissible values for bounds. *)
  type regular_value

  (** Least admissible value. *)
  val least_regular_value: regular_value

  (** Total order over the type regular_value. The output is
      strictly negative (strictly positive) if the first (second)
      argument is strictly less than the second (first) one, it is
      null if and only if the arguments are equal. *)
  val compare_regular_values: regular_value -> regular_value -> int

  (** Least upper bound *)
  val max_regular_value: regular_value -> regular_value -> regular_value

  (** Greatest lower bound *)
  val min_regular_value: regular_value -> regular_value -> regular_value

  type underlying

  (** An interval. *)
  type t

  val underlying : t -> underlying

  (** The set theoretical complement of an interval may not be an
      interval. *)
  exception Split of t * t

  (** Empty interval *)
  val empty: t

  (** Full interval i.e. the whole positive half-line *)
  val full: t

  (** Total order over the type interval. The output is strictly
      negative (strictly positive) if the first (second) argument is
      strictly less than the second (first) one, it is null if and
      only if the arguments are equal. Required by functors like
      Set.Make and Map.make. *)
  val compare: t -> t -> int

  (** Singleton whose only element is given by the argument *)
  val atom: ?u:underlying -> regular_value -> t

  (** Final segment \[x,~ \[ or \]x,~ \[ depending on the
      arguments. The symbol ~ means "unbounded on the right", which cannot
      be represented by any value of type regular_value *)
  val terminal: bool -> regular_value -> t

  (** Initial segment \[0,x\] or \[0,x\[ depending on the
      arguments *)
  val initial: bool -> regular_value -> t

  (** Either \[a,b\], \]a,b\[, \[a,b\[ or \]a,b\] depending on the
      arguments *)
  val bounded: bool -> bool -> regular_value -> regular_value -> t

  val action: (regular_value -> regular_value) -> t -> t
  (** Action of an endomorphism of regular values *)

  (** Test whether a regular_value belongs to an interval *)
  val belongs_to: regular_value -> t -> bool

  (** Test whether the first argument is included in the second one *)
  val is_included: t -> t -> bool

  (** Greatest lower bound of an interval, the exception Undefined is
      raised if and only if the argument is empty *)
  val glb: t -> regular_value

  (** Least upper bound of an interval, the exception Undefined is raised if and
      only if the argument is a final segment *)
  val lub: t -> regular_value

  val is_singleton: t -> bool
  (** Test whether the argument is a singleton *)
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
  (** Same as is_singleton *)
  val in_touch: t -> t -> bool
  (** The output value is true if and only if the set theoretic union
      of both arguments is connected *)
  val meet: t -> t -> t
  (** Set theoretic intersection of both arguments. It is also the
      binary meet in the lattice of intervals of the positive
      half-line.*)
  val join: t -> t -> t
  (** Convex hull of both argument, it is also the binary join in the
      lattice of intervals of the positive half-line. The output is
      the set theoretic union of both arguments if and only if it is
      connected (see in_touch) *)
  val union: t -> t -> t
  (** Set theoretic union of both arguments. May raise the exception
      (Split x) where x is the list of connected components of the
      intersection, the length of x is 2. Raise the exception Split
      if and only if it is not connected (see in_touch) *)
  val intersection: t -> t -> t
  (** Same as meet *)
  val not_disjoint: t -> t -> bool
  (** Test whether two intervals are disjoint *)
  val above: regular_value -> t -> t
  val below: regular_value -> t -> t
  val strictly_above: regular_value -> t -> t
  val strictly_below: regular_value -> t -> t
  val string_of: ?sd:(string * string) -> t -> string
  (** Turn an interval into a string. The option allows to choose the delimiters. *)
  val mirror_string_of: ?sd:(string * string) -> t -> string
  val interior: t -> t
  (** Topological interior of the argument with respect to the
      topology of the positive half-line *)
  val closure: t -> t
  (** Topological closure of the argument with respect to the topology
      of the positive half-line *)
  (*
  (** Ouvre à gauche et ferme à droite l'intervalle passé en argument.*)
    val open_close: t -> t

  (** Ferme à gauche et ouvre à droite l'intervalle passé en argument.*)
    val close_open: t -> t
  *)
  val normalize: t -> t
  (** Provide the normal form of an interval with respect to their
      implementation. Mostly for internal use. *)
  val before: t -> t
  (** Set of all lower bounds of the argument which do not belong to
      it *)
  val after: t -> t
  (** Set of all upper bounds of the argument which do not belong to
      it *)
  val downward: t -> t 
  (** Least initial segment containing the argument a.k.a. downward
      closure *)
  val upward: t -> t
  (** Least final segment containing the argument a.k.a. upward
      closure *)
  val is_bounded: t -> bool
  (** Test whether an interval is bounded as a subset of a metric space. *)
  val complement: t -> t
  (** Return the set theoretic complement of an interval. The
      exception Split is raised if and only if it is not connected, in
      other words when the argument is a bounded interval which do not
      contain the least regular value, or equivalently when both before and
      after return an non empty interval. *)
  val lft_bool_of_bound: t -> bool
  (** Return true if the argument is closed on the left, false
      otherwise. *)
  val rgt_bool_of_bound: t -> bool
  (** Return true if the argument is open on the left, false
      otherwise. *)
  val in_the_future_of: t -> t -> t
  (** Subset of the points of the second argument that can be reached
      from a point of the first argument running along a directed path
      whose trace is included in the set theoretic union of both
      arguments. *)
  val in_the_past_of: t -> t -> t
  (** Subset of the points of the second argument that can be reached
      from a point of the first argument running along an
      anti-directed path whose trace is included in the set theoretic
      union of both arguments. *)
  val in_the_future_of_point: regular_value -> t -> t
  (** Subset of the points of the second argument that can be reached
      from the first argument running along a directed path whose
      trace is included in the set theoretic union of both
      arguments. *)
  val in_the_past_of_point: regular_value -> t -> t
(** Subset of the points of the second argument that can be reached
    from a the first argument running along an anti-directed path
    whose trace is included in the set theoretic union of both
    arguments. *)
  val next: (regular_value -> regular_value) -> t -> t
  val next_region: (regular_value -> regular_value) -> t list -> t list
end


module Make(B:Sig.Bound): S with type regular_value = B.t
