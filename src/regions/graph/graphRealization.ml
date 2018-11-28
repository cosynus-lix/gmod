module type OrderedType = sig
  type t
  val compare: t -> t -> t
end


module type S = sig
end (* S *)

module Raw(A:OrderedType)(V:OrderedType) = struct



end (* Raw *)

module Make(A:OrderedType)(V:OrderedType):S = Raw(A:OrderedType)(V:OrderedType) 
