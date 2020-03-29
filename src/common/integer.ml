(* To be used as a functor parameter. *)

type t = int
let least_regular_value = 0
let min = Stdlib.min
let max = Stdlib.max
let compare = Stdlib.compare
let equal = Stdlib.(=)
let string_of = string_of_int
let of_string = int_of_string
let strict_upper_bound ?(gap=1) a b = (max a b)+1
let hash n = n
