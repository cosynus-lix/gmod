module type S = sig
  
  (** {2 Exceptions}*)

  exception Undefined

  (** {2 Types}*)
  
  type value
  
  type interval
  
  type t
  
  (** {2 Constants} *)

  val empty: t
  
  val full: t

  (** {2 Constructor} *)

  val of_interval: interval -> t

  val connected_components: t -> interval list
  
  (** {2 Tests} *)
  
  val is_empty: t -> bool

  (** {2 Display} *)
  
  val string_of: t -> string
  
end (* S *)

module Raw(I:Interval2.S) = struct

exception Undefined

type value = I.value

type interval = I.t

type t = interval list

(* Internals *)

let head_and_tail a = 
  match a with 
  | it::a -> it,a
  | [] -> raise Undefined

(* Constants *)

let empty = []

let full = [I.terminal true I.zero]

(* Constructor *)

let of_interval it = [it]

let connected_components x = x

(* Tests *)

let is_empty a = (a = [])

(* Display *)

let string_of a = List.fold_right (fun x accu -> (I.string_of x)^" "^accu) a "" 

(* Binary operators *)
  
let meet at1 at2 =
  let answer = ref [] in
  let at1 = ref at1 in
  let at2 = ref at2 in  
  let () =
    try
      while true do
        let it1,at3 = head_and_tail !at1 in
        let it2,at4 = head_and_tail !at2 in
        let () = try answer := (I.meet it1 it2) :: !answer
          with Undefined -> () in
        if I.is_in_the_initial_hull_of it1 it2
        then at1 := at3
        else at2 := at4
      done 
    with Undefined -> () in
  List.rev !answer

let meet at1 at2 =
  if is_empty at1 || is_empty at2 then empty 
  else meet at1 at2

let complement at =
  let last',at' = head_and_tail at in
  let answer = ref (
    try of_interval (I.strict_lower_bounds last') 
    with Undefined -> empty) in
  let at = ref at' in
  let last = ref last' in
  let () =
    try
      while true do
        let last',at' = head_and_tail !at in
        answer := (I.between !last last') :: !answer;
        at := at';
        last := last';
      done
    with Undefined -> answer := I.strict_upper_bounds !last :: !answer in
  List.rev !answer

let complement at = 
  if is_empty at then full
  else complement at

let (<<) it1 it2 = I.compare it1 it2 <= 0

let join at1 at2 =
  let answer = ref empty in
  let at1 = ref at1 in
  let at2 = ref at2 in
  let (it1,at3) = head_and_tail !at1 in
  let (it2,at4) = head_and_tail !at2 in
  let () = 
    if it1 << it2
    then update it1 at1 at3
    else update it2 at2 at4 in
  let accu = ref empty in
  let update it at at' =
    try accu := I.ordered_join !accu it; at := at'  
    with Undefined -> (
      push !accu answer;
      accu := it;
      at := at') in
  let () =
    try
      while true do
        let (it1,at3) = try head_and_tail !at1 
          with Undefined -> (empty,empty) in
        let (it2,at4) = try head_and_tail !at2 
          with Undefined -> (empty,empty) in
        if is_empty it1 && is_empty it2 then raise Exit;
        if it1 << it2
        then update it1 at1 at3
        else update it2 at2 at4
      done 
    with Exit -> () in
  of_intervals (List.rev (!accu::!answer))


let join a1 a2 =
  if is_empty a1 then a2
    else if is_empty a2 then a1
      else join a1 a2


end (* Raw *)

module Make(I:Interval2.S): S with type value = I.value and type interval = I.t 
  = Raw(I)
