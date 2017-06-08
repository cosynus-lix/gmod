(***********************************************************************)
(*                                                                     *)
(*                                GMOD                                 *)
(*                                                                     *)
(*                               arc.mli                               *)
(*                                                                     *)
(***********************************************************************)

module type S =
  (** Both lattice and boolean algebra structures over the collection of
      arcs of the positive half-line wrapped around the circle. *)
sig
  exception Undefined
  type regular_value
    (** Admissible values for bounds *)
  val least_regular_value: regular_value
    (** Least admissible value *)
  val compare_regular_values: regular_value -> regular_value -> int
    (** Total order over the type regular_value. The output is strictly
	negative (strictly positive) if the first (second) argument is
	strictly less than the second (first) one, it is null if and
	only if the arguments are equal *)
  type underlying
  type t
    (** Arcs of the directed circle *)
  val underlying: t -> underlying
  exception Split of t * t
  val empty: t
  (** Empty arc *)
  val full: t
  (** Full arc i.e. the whole circle *)
  val compare: t -> t -> int
  (** Total order over the type interval. The output is strictly
      negative (strictly positive) if the first (second) argument is
      strictly less than the second (first) one, it is null if and
      only if the arguments are equal. Required by the functors like
      Set.Make and Map.make. *)
  val atom: ?u:underlying -> regular_value -> t
  (** Singleton whose only element is given by the argument *)
  val coatom: ?u:underlying -> regular_value -> t
  (** Punctured circle at point given by the argument i.e. set
      theoretical complement of a singleton *)
  val bounded: bool -> bool -> regular_value -> regular_value -> t 
  (** Either [(a,b)], [)a,b(], [(a,b(] or [)a,b)] depending on the
      arguments. One may have [a<b] or [a>b] but [a<>b] must be
      satisfied.*)
  val action: (regular_value -> regular_value) -> t -> t
  (** Action of an endomorphism of regular values *)
  val belongs_to: regular_value -> t -> bool
  (** Test whether a regular_value belongs to an arc *)
  val is_included: t -> t -> bool
  (** Test whether the first argument is included in the second one *)
  val glb: t -> regular_value 
  (** Greatest lower bound of an arc, the exception [Undefined] is
      raised if and only if the argument is empty or full. The
      notion is sound once a direction on the circle has been
      chosen. *)
  val lub: t -> regular_value 
  (** Least upper bound of an arc, the exception [Undefined] is raised
      if and only if the argument is empty or full. The notion is
      sound once a direction on the circle has been chosen. *)
  val is_empty: t -> bool
  (** The output value is [true] if and only if the argument is
      empty *)
  val is_full: t -> bool
  (** The output value is [true] if and only if the argument is
      full *)
  val is_not_empty: t -> bool
  (** The output value is [true] if and only if the argument is not
      empty *)
  val is_not_full: t -> bool
  (** The output value is [true] if and only if the argument is not
      full *)
  val is_atomic: t -> bool
  (** Same as [is_singleton] *)
  val is_coatomic: t -> bool
  (** Test whether the argument is the commplement of a singleton *)
  val in_touch: t -> t -> bool
  (** The output value is [true] if and only if the set theoretic
      union of both arguments is connected *)
  val union: t -> t -> t
  (** Set theoretic union of both arguments. Raise the exception
      [Split] if and only if it is not connected (see [in_touch]) *)
  val dummy_union: t -> t -> t
  (** For internal use only *)
  val intersection: t -> t -> t
  (** Set theoretic intersection of both arguments. Raise the
      exception [Split] if and only if it is not connected (see
      [in_touch]) *)
  val not_disjoint: t -> t -> bool 
  val string_of: ?sd:(string * string) -> t -> string
  (* val mirror_string_of: ?sd:(string * string) -> t -> string *)
  val interior: t -> t
  (** Topological interior of the argument with respect to the
      topology of the positive half-line *)
  val closure: t -> t
  (** Topological closure of the argument with respect to the
      topology of the positive half-line *)
  val normalize: t -> t
  (** Provide the normal form of an interval with respect to their
      implementation. Mostly for internal use. *)
  val complement: t -> t
  (** Return the set theoretic complement of an arc. *)
  (* val lft_bool_of_bound: t -> bool *)
  (* Return true if the argument is closed on the left, false
     otherwise *)
  (* val rgt_bool_of_bound: t -> bool *)
  (* Return true if the argument is open on the left, false
     otherwise *)
  val in_the_future_of: t -> t -> t 
  (** Subset of the points of the second argument that can be reached
      from a point of the first argument running along a directed path
      whose trace is included in the set theoretic union of both
      arguments *)
  val in_the_past_of: t -> t -> t 
  (** Subset of the points of the second argument that can be reached
      from a point of the first argument running along an
      anti-directed path whose trace is included in the set theoretic
      union of both arguments *)
  val in_the_future_of_point: regular_value -> t -> t 
  (** Subset of the points of the second argument that can be reached
      from the first argument running along a directed path whose
      trace is included in the set theoretic union of both
      arguments *)
  val in_the_past_of_point: regular_value -> t -> t 
  (** Subset of the points of the second argument that can be reached
      from a the first argument running along an anti-directed path
      whose trace is included in the set theoretic union of both
      arguments *)
  val initial: bool -> regular_value -> t
  (** The initial segment of the directed circle, either [(0,x)] or
      [(0,x(] according to the boolean argument arguments. *)
  val terminal: bool -> regular_value -> t
(** The terminal segment of the directed circle, either (x,0( or )x,0(
    according to the boolean argument arguments. In particular
    [tarminal true least_regular_value] returns the full circle.*)

end


(** Convex subsets of the circle. Given either a totally ordered type
    that can be embedded in the positive half-line (which has a least
    element and no greatest one), the resulting module is an
    implementation of the structure formed by arcs. *)

module Make(B:(Sig.Bound)): S with type regular_value = B.t

