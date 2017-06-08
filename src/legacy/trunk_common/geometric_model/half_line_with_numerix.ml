(* Allowing the module Numerix *)

module Numerix:(Sig.Bound with type t = Numerix.Slong.t) =
struct
  type t = Numerix.Slong.t
  let least_regular_value = Numerix.Slong.zero
  let compare = Numerix.Slong.cmp
  let min x y = if Numerix.Slong.cmp x y < 0 then x else y
  let max x y = if Numerix.Slong.cmp x y > 0 then x else y
  let string_of = Numerix.Slong.string_of
  let of_string = Numerix.Slong.of_string
end

