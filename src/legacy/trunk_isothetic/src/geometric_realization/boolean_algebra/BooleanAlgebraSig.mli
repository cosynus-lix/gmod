(**{2 Ungraded}*)

module type BooleanAlgebra = sig

  type t
  (**Several boolean algebras being represented by the same type. E.g. all the boolean algebras of finite union of connected
  subsets of the (geometric realization) of a finite graph*)

  val compare: t -> t -> int
  (**[compare x y] returns [0] iff [x=y]; nonpositive (nonnegatove) if [x] less (greater) than [y]; thus providing a linear
  order*)

  val string_of: t -> string

  val is_empty: t -> bool
  (**Tests whether the argument is empty or not*)

  val is_included: t -> t -> bool
  (**Tests whether the first argument is less or equal than the second one according to the underlying partial order of the
  boolean algebra. The second argument has to be a normal form*)

  val full: t -> t
  (**[full x] is the the top element of the boolean algebra [x] belongs to*)

  val empty: t -> t
  (**[empty x] is the the bottom element of the boolean algebra [x] belongs to*)

  val complement: t -> t
  (**[complement x] si the complement of [x] in the boolean algebra [x] belongs to*)

  val normalize: t -> t
  (**[normalize x] returns the normal form of [x] that is [x] and [y] represent the same thing if their normal form are the
  same. For theoretic reason [normalize x] is often computed as [complement (comlpement x)]*)

  val intersection: t -> t -> t
  (**[intersection x y] is the intersection of [x] and [y] in the boolean algebra both [x] end [y] belong to. Should raise
  [Invalid_argument "intersection"] otherwise*)

  val intersection_list: ?default:t -> t list -> t
  (**[intersection_list lx] is the intersection of all the elements of [lx] in the boolean algebra all the elements of [lx]
  belong to. Should raise [Invalid_argument "intersection_list"] otherwise or if [lx] is the empty list and [?full] is not
  specified. Indeed one cannot guess the underlying boolean algebra*)

  val intersection_array: ?default:t -> t array -> t
  (**Same as [intersection_list] with array instead of list*)

  val union: t -> t -> t
  (**[union x y] is the union of [x] and [y] in the boolean algebra both [x] end [y] belong to. Should raise
  [Invalid_argument "union"] otherwise*)

  val union_list: ?default:t -> t list -> t
  (**[union_list lx] is the union of all the elements of [lx] in the boolean algebra all the elements of [lx] belong to.
  Should raise [Invalid_argument "intersection_list"] otherwise or if [lx] is the empty list and [?empty] is not specified.
  Indeed one cannot guess the underlying boolean algebra*)

  val union_array: ?default:t -> t array -> t
  (**Same as [union_list] with array instead of list*)

end(*BooleanAlgebra*)

module type Topological = sig

  (**According to Kuratowski's axioms a topology can be defined by the interior or closure operator*)

  include BooleanAlgebra

  val interior: t -> t
  (**[interior x] returns the topological interior of [x] *)

  val closure: t -> t
  (**[closure x] returns the topological closure of [x] *)

end(*Topological*)

module type Directed = sig

  (**The direction is defined through the future/past operators  *)

  include Topological

  val future: t -> t -> t
  (**[future x y] returns the locus of [y] than can be reached from [x] following a directed path whose image is included in
  the union of [x] and (y]*)

  val past: t -> t -> t
  (**[past x y] returns the locus of [y] than can be reached from [x] following an antidirected path whose image is included in
  the union of [x] and (y]*)

end(*DirectedBooleanAlgebra*)

(** {2 Graded} *)

module type Graded = sig
  include BooleanAlgebra

  type generator
  (**Up to monoidal structure*)

  val one: t
  (**Neutral element of the product*)

  val zero: t
  (**Absorbing element of the product*)

  val product: t -> t -> t
  (**Associative binary product*)

  val product_list: t list -> t
  val product_array: t array -> t
  (**Makes sense because the product is associative*)

  (**Combinatorial extensions*)

	val generalized_product: int -> t array -> t
  (**See Algebra*)

  val levelwise_product: ?g:int array -> int -> t array array -> t
  (**See Algebra*)

  val locus_higher_than: ?filtration:bool -> ?g:int array -> int -> t array array -> t
  (**See Algebra*)

  val locus_lower_than: ?chain:bool -> ?g:int array -> int -> t array array -> t
  (**See Algebra*)

  val factorize: t -> (bool array) list
  (**The type [t] should represent tensor products of (directed geometric) realizations of (finite) graphs. In this case
  [factorize x] returns the unique (up to isomorphisms) decomposition of [x]*)

end(*GradedBooleanAlgebra*)

module type GradedTopological = sig
  include Graded
  include Topological with type t := t
end(*GradedTopologicalBooleanAlgebra*)

module type GradedDirected = sig
  include Graded
  include Directed with type t := t
end(*GradedBooleanAlgebra*)


