(* ALCOOL: addtional_sig.mli *)
(* CEA, LIST *)

module type Tile =
sig
  (**Algebra of Tiles*)
  type brick
  type t
  (**Constructor*)
  val brick: brick -> t
  (**Boolean structure*)
  val one: t
  val zero: t
  val empty: t
  val full: t
  val is_valid: t -> bool
  val intersection: t -> t -> t
  val union: t -> t -> t
  val complement: t -> t -> t
  val normalize: t -> t -> t
  (**Topological structure*)
  val interior: t -> t
  val closure: t -> t
  (**Directed structure*)
  val future: t -> t -> t
  val past: t -> t -> t
  (**Graded structure*)
  val product: t -> t -> t
  (**Display*)
  val string_of: t -> string
  val of_string: string -> t
end (* Tiles *)

module type OdaShared =
sig
  exception Undefined
  type value
  val (<):  value -> value -> bool
  val (>):  value -> value -> bool
  val (<=): value -> value -> bool
  val (>=): value -> value -> bool
  val zero: value
  type bound
  val iso: value -> bound
  val pun: value -> bound
  val cls: value -> bound
  val opn: value -> bound
  val rvb: bound -> value
  val reverse_bound: bound -> bound
  val close_bound: bound -> bound
  val open_bound: bound -> bound
  type t
  (*val string_of: t -> string*)
  val is_valid: t -> bool
  val is_empty: t -> bool
  val is_not_empty: t -> bool
  val is_full: t -> bool
  val is_not_full: t -> bool
  val contains_zero: t -> bool
  val does_not_contain_zero: t -> bool
  val contains_more_than_zero: t -> bool
  val contains_at_most_zero: t -> bool
  val lacks_at_most_zero: t -> bool
  val lacks_more_than_zero: t -> bool
  val belongs_to: value -> t -> bool
  val is_included: t -> t -> bool
  val full: t
  val empty: t
  val make: bound list -> t
  val atom: value -> t
  val coatom: value -> t
  val interval: bool -> bool -> value -> value -> t
  val cointerval: bool -> bool -> value -> value -> t
  val initial: bool -> value -> t
  val final: bool -> value -> t
  val normalize: t -> t
  val union: t -> t -> t
  val intersection: t -> t -> t
  val difference: t -> t -> t
  val complement: t -> t
  val add_zero: t -> t
  val remove_zero: t -> t
  val first_connected_component: ?flag:bool ref -> t -> t
  val last_connected_component: ?parity:bool -> t -> t
end

module type OdaHalfLine =
sig
  include OdaShared
  val is_bounded: t -> bool
  val is_not_bounded: t -> bool
  val closure_contains_zero: t -> bool
  val interior_contains_zero: t -> bool
  val interior_does_not_contain_zero: t -> bool
  val glb: t -> value
  val lub: t -> value
  val interior: t -> t
  val closure: ?unbounded_answer:(bool ref) -> t -> t
  val boundary: t -> t
  val string_of: ?empty_set_denotation:string -> ?infinity_denotation:string -> ?open_infinity:bool -> t -> string
  val future_extension: ?flag:bool ref -> t -> t -> t
  val future_closure: t -> t * bool
  val past_extension: t -> t -> t
  val past_closure: t -> t * bool
end

module type OdaCircle =
sig
  include OdaShared
  val closure_contains_zero: t -> bool
  val interior_contains_zero: t -> bool
  val interior_does_not_contain_zero: t -> bool
  val boundary_contains_zero: t -> bool
  val boundary_does_not_contain_zero: t -> bool
  val interior: t -> t
  val closure: t -> t
  val boundary: t -> t
  val string_of: ?empty_set_denotation:string -> ?full_set_denotation:string -> t -> string
  val future_extension: ?flag:bool ref -> t -> t -> t
  val future_closure: t -> t * bool
  val past_extension: t -> t -> t
  val past_closure: t -> t * bool
end
