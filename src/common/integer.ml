(* To be used as a functor parameter. *)

type t = int
let least_regular_value = 0
let min = Pervasives.min
let max = Pervasives.max
let compare = Pervasives.compare
let equal = Pervasives.(=)
let string_of = string_of_int
let of_string = int_of_string
let strict_upper_bound ?(gap=1) a b = (max a b)+1
let hash n = n
