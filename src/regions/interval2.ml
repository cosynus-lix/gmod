module type S = sig
  
  (** {2 Exceptions}*)

  exception Undefined

  (** {2 Types}*)
  
  type value
  type t
  
  (** The set of upper bounds of the argument that do not belong to it.*)
  val strict_upper_bounds: t -> t
  
  (** The set of lower bounds of the argument that do not belong to it.*)
  val strict_lower_bounds: t -> t
  
end (* S *)

module Raw(B:Bound.S) = struct

  let (<) x y = B.compare x y < 0

  let (>) x y = B.compare x y > 0

  let (<=) x y = B.compare x y <= 0

  let (>=) x y = B.compare x y >= 0

  exception Undefined

  type value = B.t

  type t =
  | Te of (bool * value)                (* final     *)
  | Bn of (bool * value * value * bool) (* bounded   *)
  | Si of value                         (* singleton *)
  | Em                                  (* empty     *)
  
end (* Raw *)

module Make(B:Bound.S): S with type value = B.t 
  = Raw(B)
