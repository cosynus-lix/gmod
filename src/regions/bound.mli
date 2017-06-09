(** Totally ordered set that can be embedded into the positive half-line. The 
function "compare" must be compatible with respect to the functions "min" and 
"max". *)

module type S = sig

type t

(** Least element. *)
val least_regular_value : t

(** Greatest lower bound. *)
val min: t -> t -> t

(** Least upper bound. *)
val max : t -> t -> t

(** Comparison function on elements. *)
val compare: t -> t -> int

(** String representation. *)
val string_of: t -> string

(** From a string representation. *)
val of_string: string -> t

end
