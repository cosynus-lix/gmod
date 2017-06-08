(***********************************************************************)
(*                                                                     *)
(*                               ALCOOL                                *)
(*                                                                     *)
(*                              CEA LIST                               *)
(*                                                                     *)
(*                              cyle.mli                               *)
(*                                                                     *)
(***********************************************************************)

(** Combine both cubes of real vector spaces and tori. *)

module type In =
sig
  module IntSet:
    sig 
      include Set.S
      val segment_initial: ?start:t -> int -> t
    end with type elt = int
  module Array:
    sig
      val cartesian_next_array_consumer: int array -> int -> unit
      val cartesian_next_array_consumer2: int array -> int array -> unit
    end
end with type IntSet.elt = int

module type S =
sig
  type regular_value
  type point
  type generator
  type underlying
  type ints
    (** Set of integers.*)
  type t 
  exception Undefined
  val sub: int -> int -> underlying -> underlying (* Generic *)
  val add: underlying -> underlying -> underlying (* Generic *)
  val zero: underlying (* Generic *)
  val compare_points: point -> point -> int (* Generic *)
  val compare: t -> t -> int (* Generic *)
  val compare_generator: generator -> generator -> int
  val string_of_generator: generator -> string (* Affichage *)
    (** Comparing generators so the module built over Cyle inherits from Mix. *)
  val dimension: t -> int 
  val atom: underlying -> point -> t (* Generic *)
  (* val make: (generator list) -> t *) (* Generic *)
  val of_list: (generator list) -> t (* Generic *)
  val of_array: (generator array) -> t (* Generic *)
  val to_list:  t -> (generator list) (* Generic *)
  val to_array: t -> (generator array) (* Generic *)
  (* val from_generator_array: (generator array) -> t *) (* probably useless *)
  val normalize:t -> t (* can be made useless *)
  val complement: t -> (t list) (* Generic *)
  val product: t -> t -> t (* Generic *)
  val intersection: t -> t -> (t list) (* Generic *)
  val full: underlying -> t (* Generic *)
  val empty: underlying -> t (* Generic *)
  val belongs_to: point -> t -> bool (* Generic *)
  val interior: t -> t (* Generic *)
  val closure: t -> t (* Generic *)
  val is_empty: t -> bool (* Generic *)
  val is_not_empty: t -> bool (* Generic *)
  val is_full: t -> bool (* Generic *)
  val is_not_full: t -> bool (* Generic *)
  val is_included: t -> t -> bool (* Generic *)
  val in_the_future_of: t -> t -> t (* Generic *)
  val in_the_past_of: t -> t -> t (* Generic *)
  val string_of: t -> string (* Generic *)
  val in_touch: t -> t -> bool (* Generic *)
  val lub: t -> point (* ? *)
  val glb: t -> point (* ? *)
  val underlying: t -> underlying (* Generic *)
end

module Make(Overall:In)(M:Mix.S): 
  S with type regular_value = M.regular_value 
     and type point = (M.regular_value) array
     and type underlying = (M.underlying) array
     and type generator = M.t
     and type ints = Overall.IntSet.t
