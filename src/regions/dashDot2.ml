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

  (** {2 Destructor} *)

  val connected_components: t -> interval list
  
  (** {2 Tests} *)
  
  val is_empty: t -> bool

  (** {2 Display} *)
  
  val string_of: t -> string

  (* {2 Enumerator} *)

  val next: (value -> value) -> t -> t
  
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

let string_of a = 
  if is_empty a then "Ø"
  else List.fold_right (fun x accu -> (I.string_of x)^" "^accu) a "" 

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
          with I.Undefined -> () in
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
  let first_finished = ref false in
  let second_finished = ref false in
  let at1 = ref at1 in
  let at2 = ref at2 in
  let (it1,at3) = head_and_tail !at1 in
  let (it2,at4) = head_and_tail !at2 in
  let accu = ref (
    if it1 << it2
    then (at1 := at3; it1)
    else (at2 := at4; it2)) in
  let update it at at' =
    try accu := I.ordered_join !accu it; at := at'  
    with I.Undefined -> (
      answer := !accu :: !answer;
      accu := it;
      at := at') in
  let dummy = I.full,full in
  let () =
    try
      while true do
        let (it1,at3) = try head_and_tail !at1 
          with Undefined -> (first_finished := true; dummy) in
        let (it2,at4) = try head_and_tail !at2 
          with Undefined -> (second_finished := true; dummy) in
        if !first_finished && !second_finished then raise Exit;
        if (not !first_finished) && (not !second_finished) && it1 << it2
        then update it1 at1 at3
        else update it2 at2 at4;
        if !first_finished
        then update it2 at2 at4
        else update it1 at1 at3
      done 
    with Exit -> () in
  List.rev (!accu::!answer)


let join a1 a2 =
  if is_empty a1 then a2
    else if is_empty a2 then a1
      else join a1 a2

(* Enumerator *)

let next next_value re = 
  let next it = I.next next_value it in
(*
    let next = I.next next_value it in
    if next <> [] then next else raise Exit in
*)
  let next_with_lesser_lub it =
    let x = ref it in
    let y = ref (next it) in
    while (
      (I.is_bounded !x) && (not (I.is_bounded !y)) || 
      (I.is_bounded !x && I.is_bounded !y && I.lub !x <= I.lub !y))
    do x := !y ; y := next !y
    done;
    !y in
  let rec init k re =
    if (Pervasives.(>)) k 0 
    then 
      init (pred k) (
        match re with 
          | it :: _ -> I.nonempty_disconnected_next next_value it :: re
          | []      -> [I.singleton I.zero])
    else re in
  let init k re = List.rev (init k re) in
  let rec next_region len re =
      match re with
        (*| [it] -> assert (len = 1); [next it]*) (* petite optimisation *)
        | it :: re' -> (
            try it :: next_region (pred len) re'
            with Exit -> (
             try init (pred len) [next it]
             with Exit -> init (pred len) [next_with_lesser_lub it] ))
        | [] -> raise Exit (* assert false *) in
  try
    match re with 
      | [] -> [I.singleton I.zero]
      | _ -> next_region (List.length re) re 
  with Exit -> init (succ (List.length re)) []



end (* Raw *)

module Make(I:Interval2.S): S with type value = I.value and type interval = I.t 
  = Raw(I)
