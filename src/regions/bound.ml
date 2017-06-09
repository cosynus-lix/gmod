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
