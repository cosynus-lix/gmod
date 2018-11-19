module type S = sig
  
  type value
  type interval
  type t
  
end (* S *)

module Raw(I:Interval2.S) = struct

  type value = I.value

  type interval = I.t

  type t = interval list
  
end (* Raw *)

module Make(I:Interval2.S): S with type value = I.value and type interval = I.t 
  = Raw(I)
