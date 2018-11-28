module type S = sig
  
  (** {2 Exceptions}*)

  exception Undefined

  (** {2 Types}*)
  
  type value
  type t
  
  (** {2 Constants} *)

  val zero: value
  val full: t

  (** {2 Constructors}*)
  
  val atom: value -> t
  val bounded: bool -> value -> value -> bool -> t
  val co_bounded: t -> t -> t
  val initial: value -> bool -> t
  val terminal: bool -> value -> t

 (** {2 Iterator and Enumerator}*)
  
  val next: (value -> value) -> t -> t

  val nonempty_disconnected_next: (value -> value) -> t -> t

  (** {2 Unary operators}*)

  (** The set of upper bounds of the argument that do not belong to it.*)
  val strict_upper_bounds: t -> t
  
  (** The set of lower bounds of the argument that do not belong to it.*)
  val strict_lower_bounds: t -> t

  (** The set of points that are lower than some point of the argument.*)
  val initial_hull: t -> t

  (** The set of points that are greater than some point of the argument.*)
  val terminal_hull: t -> t
  
  (** Least upper bound.*)
  val lub: t -> value
  
  (** Greatest lower bound.*)
  val glb: t -> value

  val interior: t -> t

  val closure: t -> t

  val remove_zero: t -> t
  
  val add_zero: t -> t
  
  (** {2 Binary operators}*)
  
  (** Returns the (set theoretic) intersection of the arguments.*)
  val meet: t -> t -> t
  
  (** Returns the (set theoretic) union of the arguments when they are 
  connected. Raise the exception Undefined otherwise*)
  val join: t -> t -> t
  val ordered_join: t -> t -> t


  (** Assuming that there exists a value which is both a strict upper 
  bound of the first argument and a strict lower bound of the second argument, 
  the function returns the interval of all such values.*)
  val between: t -> t -> t

  (** {2 Display}*)
  
  (** The two first argument provide the delimiters for intervals *)
  val string_of: string -> string -> string -> string -> string -> t -> string
  
  (** {2 Compare}*)

  (** it1 << it2 means that the set of strict lower bounds of it1 is included 
  in that of it2. When they are equal, the least initial segment contenaining 
  it1 is included in that contenaining it2.*)
  val compare: t -> t -> int

  (** {2 Tests}*)

  val is_atom: t -> bool
  val is_bounded: t -> bool
  val ordered_disjoint: t -> t -> bool
  val ordered_disconnected: t -> t -> bool
  val is_in_the_initial_hull_of: t -> t -> bool
  val is_in_the_terminal_hull_of: t -> t -> bool
  val mem: value -> t -> bool
  val contains_zero: t -> bool
  val is_included: t -> t -> bool
  
end (* S *)

module Raw(B:Bound.S) = struct

let zero = B.least_regular_value

exception Undefined

type value = B.t

type t =
| Te of (bool * value)                (* final     *)
| Bn of (bool * value * value * bool) (* bounded   *)
| Si of value                         (* atom *)

(* Display *)

let string_of ld rd ls rs infty it =
  match it with 
  | Bn (a,x,y,b) -> Printf.sprintf "%s%s,%s%s" 
      (if a then ld else rd)
      (B.string_of x)
      (B.string_of y)
      (if b then rd else ld)
  | Te (a,x) -> Printf.sprintf "%s%s,%s%s" 
      (if a then ld else rd)
      (B.string_of x)
      infty
      ld
  | Si x -> ls^(B.string_of x)^rs 

(* Constants *)

let zero = B.least_regular_value

let full = Te (true,zero)

(* Constructors *)

let atom x = Si x

let bounded a x y b = Bn (a,x,y,b)

let terminal a x = Te (a,x)

let initial y b = 
  if y <> zero then bounded true zero y b 
  else if b then atom zero
    else raise Undefined

let co_bounded it1 it2 = 
  match (it1,it2) with
  | Bn(a',x',y,b),Te(a,x) -> 
    if a' && x' = zero then (if x<>y then Bn(a,x,y,b) else Si x) 
    else raise Undefined 
  | Si x',Te(a,x) -> 
    if x' = zero then Bn(a,x,zero,true) 
    else raise Undefined 
  | _ -> raise Undefined

(* Enumerator *)

let next next it = 
  let next_bound b = match b with
    | y,false -> y,true
    | y,true -> (next y),false in
  match it with
    | Si x -> (try bounded true x (next x) false with Exit -> terminal true x)
    | Bn (a,x,y,b) -> (
        try 
          let y,b = next_bound (y,b) in 
          bounded a x y b 
        with Exit -> terminal a x) 
    | Te (true,x) -> (
        try bounded false x (next x) false 
        with Exit -> terminal false x)
    | Te (false,x) -> Si (next x) 

let nonempty_disconnected_next next it =
  match it with
    | Te _ -> raise Exit
    | Bn (_,_,x,false) -> (
        try bounded false x (next x) false
        with Exit -> terminal false x)
    | Si y | Bn (_,_,y,true) -> Si (next y)

(* Internals *)

let left_bound it = 
  match it with 
  | Si x -> true,x
  | Te (a,x) | Bn (a,x,_,_) -> a,x
  
let right_bound it = 
  match it with
  | Si x -> x,true
  | Bn (_,_,y,b) -> y,b
  | Te _ -> raise Undefined

(* Tests *)

let is_atom it = 
  match it with
  | Si _ -> true
  | _ -> false

let is_bounded it =
  match it with
  | Te _ -> false
  | _ -> true

let ordered_disjoint it1 it2 = 
  try
    let y1,b1 = right_bound it1 in
    let a2,x2 = left_bound it2 in
    let delta = B.compare y1 x2 in
    (delta < 0) || (delta = 0 && (not b1 || not a2))
  with Undefined -> false

let (<|<) = ordered_disjoint

let ordered_disconnected it1 it2 = 
  try
    let y1,b1 = right_bound it1 in
    let a2,x2 = left_bound it2 in
    let delta = B.compare y1 x2 in
    delta < 0 || (delta = 0 && not b1 && not a2)
  with Undefined -> false

let (<-<) = ordered_disconnected

let is_in_the_initial_hull_of it1 it2 =
  try 
    let y2,b2 = right_bound it2 in
    (try 
      let y1,b1 = right_bound it1 in
      let delta = B.compare y1 y2 in
      delta < 0 || (delta = 0 && (b2 || not b1))
    with Undefined -> false)
  with Undefined -> true

let is_in_the_terminal_hull_of it1 it2 =
    let a1,x1 = left_bound it1 in
    let a2,x2 = left_bound it2 in
    let delta = B.compare x2 x1 in
    delta < 0 || (delta = 0 && (a2 || not a1))

let mem v it =
  match it with 
  | Bn(a,x,y,b) -> 
      let delta_a = B.compare x v in
      let delta_b = B.compare v y in
      ((delta_a < 0) || (delta_a = 0 && a))
      && ((delta_b < 0) || (delta_b = 0 && b))
  | Te(a,x) -> 
      let delta_a = B.compare x v in
      (delta_a < 0) || (delta_a = 0 && a) 
  | Si x -> v = x
  
let contains_zero it =
  match it with 
  | Bn(a,x,_,_) | Te(a,x) -> a && x = zero
  | Si x -> x = zero

let is_included it1 it2 =
  is_in_the_initial_hull_of it1 it2
  && is_in_the_terminal_hull_of it1 it2
    
(* Compare *)

let compare it1 it2 = 
  if it1 = it2 then 0
    else if not (is_in_the_terminal_hull_of it2 it1) then 1
      else if not (is_in_the_terminal_hull_of it1 it2) then -1 
        else if is_in_the_initial_hull_of it1 it2 then -1
          else 1

(* Unary operators *)

let strict_upper_bounds i = 
  match i with
  | Si x -> terminal false x
  | Bn (_,_,y,b) -> terminal (not b) y
  | Te _ -> raise Undefined

let strict_lower_bounds i = 
  match i with
  | Si x -> initial x false
  | Bn (a,x,_,_) | Te (a,x) -> initial x (not a)

let initial_hull it = match it with
  | Bn(_,_,y,b) -> initial y b
  | Si y -> initial y true
  | Te _ -> full

let terminal_hull it = match it with
  | Bn(a,x,_,_) -> terminal a x
  | Si x -> terminal true x  
  | _ -> it

let lub it = match it with
  | Bn (_,_,y,_) | Si y -> y
  | Te _ -> raise Undefined

let glb it = match it with
  | Bn (_,x,_,_) | Si x | Te (_,x) -> x

let interior it = match it with
  | Bn(a,x,y,_) -> Bn(a && x = zero,x,y,false)
  | Te(a,x) -> Te(a && x = zero,x)
  | Si _ -> raise Undefined

let closure it = match it with
  | Bn(_,x,y,_) -> Bn(true,x,y,true)
  | Te(_,x) -> Te(true,x)
  | Si _ -> it

let remove_zero it = 
  match it with
  | Bn (a,x,y,b) -> if x <> zero then it else Bn (false,x,y,b)
  | Te (a,x) -> if x <> zero then it else  Te (false,x)
  | Si x -> if x <> zero then it else raise Undefined

let add_zero it = 
  match it with
  | Bn (a,x,y,b) -> if x = zero then Bn (true,x,y,b) else raise Undefined
  | Te (a,x) -> if x = zero then Te (true,x) else raise Undefined
  | Si x -> if x = zero then  it else raise Undefined


(* Binary operators *)

(* meet *)

let rightmost_left_bound it1 it2 =
  let a1,x1 as u = left_bound it1 in
  let a2,x2 = left_bound it2 in
  let delta = B.compare x1 x2 in
  if delta < 0 || (delta = 0 && a1) 
  then (a2,x2) else (a1,x1)

let leftmost_right_bound it1 it2 =
  try
    let y1,b1 = right_bound it1 in (
    try
      let y2,b2 = right_bound it2 in
      let delta = B.compare y1 y2 in
      if delta < 0 || (delta = 0 && b2)
      then y1,b1 else y2,b2
    with Undefined -> y1,b1)
  with Undefined -> right_bound it2

let meet it1 it2 =
  if it1 <|< it2 || it2 <|< it1 
  then raise Undefined
  else 
    let a,x = rightmost_left_bound it1 it2 in (
    try
      let y,b = leftmost_right_bound it1 it2 in
      if x <> y then bounded a x y b else atom x 
    with Undefined -> terminal a x)

(* join *)

let leftmost_left_bound it1 it2 =
  let a1,x1 = left_bound it1 in
  let a2,x2 = left_bound it2 in
  let delta = B.compare x1 x2 in
  if delta < 0 || (delta = 0 && a1) 
  then (a1,x1) else (a2,x2)

let rightmost_right_bound it1 it2 =
  let y1,b1 = right_bound it1 in 
  let y2,b2 = right_bound it2 in
  let delta = B.compare y1 y2 in
  if delta < 0 || (delta = 0 && b2)
  then (y2,b2) else (y1,b1)

let join it1 it2 =  
  if (it1 <-< it2) || (it2 <-< it1) then raise Undefined
    else
      let (a,x) = leftmost_left_bound it1 it2 in 
      try
        let (y,b) = rightmost_right_bound it1 it2 in
        if x <> y then bounded a x y b else atom x
      with Undefined -> terminal a x

(* More efficient implementation under the hypothesis that it1 << it2. *)

let ordered_join it1 it2 =
  if it1 <-< it2 then raise Undefined
    else
      let (a,x) = left_bound it1 in 
      try
        let (y,b) = rightmost_right_bound it1 it2 in
        if x <> y then bounded a x y b else atom x
      with Undefined -> terminal a x

let between it1 it2 =
  let y1,b1 = right_bound it1 in
  let a2,x2 = left_bound it2 in
  if B.compare y1 x2 < 0 then bounded (not b1) y1 x2 (not a2)
  else atom y1

end (* Raw *)

module Make(B:Bound.S): S with type value = B.t 
  = Raw(B)
