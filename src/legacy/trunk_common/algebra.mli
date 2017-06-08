(**{2 Algebra}*)

(**Raised when the result is {e not defined} e.g. when a division by zero is attempted.*)
exception Does_not_know

(**Some functions raised this exception instead of returning zero.*)
exception Zero

(**The ring of {e integers} using OCaml integers.*)
module Integer:Sig.EuclideanRing with type t=int

(**The field of {e real numbers} using OCaml float.*)
module Real:Sig.Field with type t=float

(**The ring of {e matrices} with coefficients in a given ring.*)
module Matrix(R:Sig.Ring)(D:sig val initial:int end):Sig.MatrixAlgebra with type s = R.t

(**The ring of {e polynomials} with coefficients over a given ring.*)
module Polynomial:
sig
  module ByArray:
  sig
    module Ring(R:Sig.Ring):Sig.Ring
    module FactorialRing(R:Sig.FactorialRing):Sig.FactorialRing
  end
  module ByMap:
  sig
    module Ring(R:Sig.Ring):Sig.Ring
    module FactorialRing(R:Sig.FactorialRing):Sig.FactorialRing
  end

end

(**The field of {e fractions} over a given {e factorial} ring.*)
module Fraction(R:Sig.FactorialRing):Sig.Field

module BooleanArray:
sig
  val extract: bool array -> 'a array -> 'a array
  val extract_complement: bool array -> 'a array -> 'a array
  val next_levelwise: bool array -> bool array
  val previous_levelwise: bool array -> bool array
  val next_levelwise_side_effect: bool array -> unit
  val previous_levelwise_side_effect: bool array -> unit
  val next_on_its_level: bool array -> bool array
  val previous_on_its_level: bool array -> bool array
  val next_side_effect: bool array -> unit
  val previous_side_effect: bool array -> unit
  val next_on_its_level_side_effect: bool array -> unit
  val fold: (bool array -> bool array) -> (bool array -> 'a -> 'a) -> bool array -> 'a -> 'a
  val iter: (bool array -> bool array) -> (bool array -> unit) -> bool array -> unit
  val fold_up: (bool array -> 'a -> 'a) -> bool array -> 'a -> 'a
  val fold_down: (bool array -> 'a -> 'a) -> bool array -> 'a -> 'a
  val fold_up_levelwise: (bool array -> 'a -> 'a) -> bool array -> 'a -> 'a
  val fold_down_levelwise: (bool array -> 'a -> 'a) -> bool array -> 'a -> 'a
  val iter_up: (bool array -> unit) -> bool array -> unit
  val iter_down: (bool array -> unit) -> bool array -> unit
  val iter_up_levelwise: (bool array -> unit) -> bool array -> unit
  val iter_down_levelwise: (bool array -> unit) -> bool array -> unit
  val unpack: bool array list -> bool array list
  val string_of: ?rev:unit -> ?t:string -> ?f:string -> bool array -> string
  val support_of: ?negative:unit -> bool array -> int list
  val of_support: int list -> int -> bool array
  val string_of_support: ?left:string -> ?separator:string -> ?right:string -> ?negative:unit -> bool array -> string
  val string_of_factors: ?left:string -> ?separator:string -> ?right:string -> bool array list -> string
end

(**Gathering some well-known free structures.*)
module Free:
sig
  module Graded:
  sig
    module SemiLattice(Ord:sig type t val compare: t -> t -> int val string_of: t -> string end):
      (
        sig
          type word
          type polynomial
          val make: word list -> polynomial
          val string_of: polynomial -> string
          val zero: polynomial
          val add: word -> polynomial -> polynomial
          val homogeneous: polynomial -> bool
          val factorize: ?degree:int -> ?unpacked:bool -> polynomial -> bool array list
        end
        with type word = Ord.t array
      )
  end(*Graded*)
end(*Free*)

(**Gathering some well-known monoids.*)
module Monoid:
sig
  module Philosopher:
  sig
    val string_of: int array -> string
    val string_of_class: int array list -> string
    val equivalence_class: ?bound:int -> int array -> int array list
    val traces: int -> int array list list
    val orbits: int -> int array list list list
  end
end

(**Provide functions to enumerate finite collection of arrays and lists of integers. The collection of arrays of integers is
implicitly provided with a total order which is unspecified but fixed by the module. Restricted to a finite collection of
such arrays this total order induces a well order. The terminology related to order over arrays of integers met in the
sequel refer to it. We write that the array of integers [a] is {e dominated} by the array of integers [b] when [a] is shorter
than [b] and [a.(i)] is less than [b.(i)] for all [i]. If [a] and [b] are two arrays of integers then [a+b] denotes the
array abtained by adding the entries of [a] and [b] termwise*)
module SequencesOfIntegers:
sig

	val first_list: ?start:int -> int -> int list
  (**[segment_initial ~start n] returns the list of [n] consecutive integers whose first element is [start]*)

  val next_list: int list -> int -> int list
  (**[next l n] is the successor of [l] in the {e lexicographically} (hence totally) ordered set of {e strictly monotonic}
  sequences of integers of length [List.length l] whose terms are less or equal than [n]. This poset is finite and the
  function [next_list] raises the exception [Not_found] when [l] it the greatest element of the chain.*)

  val fold_list: ?start:int -> (int list -> 'a -> 'a) -> int -> int -> 'a -> 'a
  (**[let fold_list ~start f length_or_dimension upper_bound accumulator]*)

  val iter_list: ?start:int -> (int list -> unit) -> int -> int -> unit

  val weight: int array -> int
  (**[weight n a] is the sum of all the entries of [a]*)

  val weight_up_to: int -> int array -> int
  (**[weight n a] is the sum of entries of [a] from index [0] to index [n]*)

  val first_array: ?g:int array -> int -> int -> int array -> int array
  (**[first_array l w a] returns the first array of length [l] whose sum is {e equal to} [w], and which is dominated by [a].
  Unspecified behaviour if [l] is strictly greater than [Array.length a]. @raise Not_found if no such sequence exists i.e.
  when the sum of entries of [Array.sub 0 length a] is strictly less than [w]*)

	val next_array: ?g:int array -> int -> int array -> int array -> int array
  (**[next_array l s a] returns the next array of length [l] whose sum is {e equal to} [weight s], and dominated by [a].
  Unspecified behaviour if [l] is strictly greater than [Array.length a]. @raise Not_found if no such sequence exists i.e.
  when [s] is the last one or if the sum of entries of [Array.sub 0 l a] is strictly less than [weight s]*)

  val fold_array: ?g:int array -> (int array -> 'a -> 'a) -> int -> int array -> 'a -> 'a
  (**[fold_array ~g f w a accu] returns the last term of the sequence x inductively defined as follows: if the arrays of
  integers of weight [w] that dominates [g] and that are dominatred by [g+a] are sorted from ar(1) to ar(N), then x(0) is
  [a] and x(n+1) is [f] ar(n+1) x(n). If there is no such array, then the sequence x is reduced to x(0) and [accu] is
  returned. The arrays [a] and [g] should be of the same length*)

  val iter_array: ?g:int array -> (int array -> unit) -> int -> int array -> unit
  (**If the arrays of integers of weight [w] that dominates [g] and that are dominatred by [g+a] are sorted from ar(1) to
  ar(N), then [iter_array ~g f w bound a] is the result of the sequence of actions [f] ar(1);...;[f] ar(N). If there is no
  such array, then [iter_array ~g f w bound] does nothing. The arrays [a] and [g] should be of the same length*)

end

(**Provide Boolean algebras with additional operation. Suppose we have a family of Boolean algebras Bi and type [t] represents
any value of the product of any subfamily. In particular, the empty product is the two valued boolean algebra whose elements
are represented by [zero] and [one]. For all [x:t], [full x] is the top element of the boolean algebra [x] belongs to. For
all [x:t] and [y:t] both taken in the Boolean algebra Bi, [union x y] is the join element of [x] and [y] in the Boolean
algebra Bi. If [x] is an element of Bi and [y] is an element of Bj, then [product x y] is the element of the Cartesian product
Bi x Bj whose projections are [x] and [y].*)
module Combinator
(X:sig
  type t
  val zero: t
  val full: t -> t
  val product: t -> t -> t
  val product_list: t list -> t
  val product_array: t array -> t
  val product_fun: (int -> t) -> int -> int -> t
  val union: t -> t -> t
  val normalize: t -> t
  val string_of: t -> string
end):
sig
	val generalized_product: int -> X.t array -> X.t
  (**Assume [a] is an array of length [l] such that [a.(k)] belongs to some boolean algebra {b B}([k]). For each {b S} subset
  of \{[0],...,[l-1]\} of cardinal [n], we denote by {b PS} the [l]-fold product {b C}([0]) x ... x {b C}([l-1]) where each
  {b C}([k]) is [a.(k)] if [k] belongs to {b S} and [full a.(k)] otherwise - [full a.(k)] is the greatest element of the
  boolean algebra [a.(k)] belongs to. The function [generalized_product] then return the union (least upper bound) of the
  family of products {b PS} for {b S} subset of \{[0],...,[l-1]\} of cardinal [n] in the tensor product, in the category of
  commutative idempotent monoids, of the family of booleran algebras {b B}([1]),...,{b B}([l-1])*)

  val levelwise_product: ?g:int array -> int -> X.t array array -> X.t
  (**Let [d] be [(Array.length a)-1]. Then [levelwise_product w a] is the union of all the products ([a.(0).(f 0)] x ... x
  [a.(d).(f d)]), with [f] defined over \{[0],...,[d]\} with integer values satisfying [f i] strictly less than
  [Array.length a.(i)] for all [i] and [(f 0) + ... + (f d)] = [w]. In particular, if each line of [a] is a {e partition}
  (i.e. the intersection of two entries is empty) then each line of [a.(i)] can be seen as a staircase map [hi] with
  [a.(i).(j)] being the locus where the value of the function of index [i] is [j]. Hence [levelwise_product w a] can be
  interpreted as the locus of points [(p0,...,pd)] where the sum [h0 p0] + ... + [hd pd] is [w]. If each line of [a] is a {e
  filtration} (i.e. [a.(i).(j)] contains [a.(i).(j+1)] for all [j] and [a.(i).(0)] is the whole domain of definition of
  [hi]) then [levelwise_product n a] can be interpreted as the locus of points [(p0,...,pd)] where the sum [h0 p0] + ... +
  [hd pd] greater or equal than [w].*)

  val locus_higher_than: ?filtration:bool -> ?g:int array -> int -> X.t array array -> X.t
  (**Note that if [a.(i)] is a filtration then the first and the last entries of the line [a.(i)] should be respectively the
  domain of definition (of the staircase mapping represented by the filtration) and the bottom element of the corresponding
  boolean algebra. The bottom element can be omited. If the option [~ground:g] is specified then the least value of [hi]
  should be given by [g i]. The default ground is the zero function. Therefore [a.(i).(j)] is the locus of points where the
  value of [hi] is greater or equal than [(min hi) + j]*)

  val locus_lower_than: ?chain:bool -> ?g:int array -> int -> X.t array array -> X.t
  (**Note that if [a.(i)] is a chain then the first and the last entries of then line [a.(i)] should be respectively the
  bottom element of the corresponding boolean algebra and the domain of definition (of the staircase mapping represented by
  the filtration). The bottom element can be omitted. If the option [~ground:g] is specified then the least value of [hi]
  should be given by [g i]. The default ground is the zero function. Therefore [a.(i).(j)] is the locus of points where the
  value of [hi] is less or equal than [(min hi) + j]*)
end

(**Polymorphic Combinator. Provide a module instead of a functor.*)
module PCombinator:
sig
  val levelwise_product:   zero:'a ->                    union:('a -> 'a -> 'a) -> product_fun:((int -> 'a) -> int -> int -> 'a) -> ?g:(int array) -> int -> 'a array array -> 'a
	val generalized_product: zero:'a -> full:('a -> 'a) -> union:('a -> 'a -> 'a) -> product_fun:((int -> 'a) -> int -> int -> 'a) ->                   int -> 'a array       -> 'a
	(*val full_aux: 'a -> ('a -> 'a) -> ('a -> 'a -> 'a) -> ?fc:int -> ?lc:int -> 'a array array -> 'a
	val empty_aux: 'a -> 'a -> ('a -> 'a) -> ('a -> 'a -> 'a) -> ?fc:int -> ?lc:int -> 'a array array -> 'a*)
(*
  val local_product: int -> 'a -> ('a -> 'a) -> ('a -> 'a -> 'a) -> int array -> 'a array array -> 'a
*)
  val locus_higher_than: zero:'a -> full:('a -> 'a) -> union:('a -> 'a -> 'a) -> product_fun:((int -> 'a) -> int -> int -> 'a) -> ?filtration:bool -> ?g:(int array) -> int -> 'a array array -> 'a
  val locus_lower_than: zero:'a -> full:('a -> 'a) -> union:('a -> 'a -> 'a) -> product_fun:((int -> 'a) -> int -> int -> 'a) -> ?chain:bool -> ?g:(int array) -> int -> 'a array array -> 'a
end(*PCombinator*)
