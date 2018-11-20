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

  let zero = B.least_regular_value

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
  
  let empty = Em
  
  let full = Te (true,zero)
  
  let singleton x = Si x
  
  let interval a x y b = Bn (a,x,y,b)
  
  let terminal a x = Te (a,x)
  
  let initial y b = 
    if y <> zero then interval true zero y b 
    else if b then singleton zero
      else empty
  
  let strict_upper_bounds i = 
    match i with
    | Em -> full
    | Si x -> terminal false x
    | Bn (_,_,y,b) -> terminal (not b) y
    | Te _ -> empty
  
  let strict_lower_bounds i = 
    match i with
    | Em -> full
    | Si x -> initial x false
    | Bn (a,x,_,_) | Te (a,x) -> initial x (not a)

end (* Raw *)

module Make(B:Bound.S): S with type value = B.t 
  = Raw(B)
