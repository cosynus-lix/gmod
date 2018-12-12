module type S = sig
  
  (** {2 Exception} *)

  exception Undefined

  (** {2 Types} *)
  
  type value
  type interval
  type t

  (** {2 Constructor} *)

  val of_interval: interval -> t
  
  (** {2 Boolean structure} *)
  
  val empty: t
  val full: t
  val complement: t -> t
  val meet: t -> t -> t
  val difference: t -> t -> t
  val join: t -> t -> t
  
  (** {2 Tests} *)
  
  val is_empty: t -> bool
  val mem: value -> t -> bool
  val is_included: t -> t -> bool
  val is_bounded: t -> bool
  
  (** {2 Dealing with zero} *)

  val contains_zero: t -> bool
  val add_zero: t -> t
  val remove_zero: t -> t
  val is_neighbourhood_of_zero: t -> bool

  (** {2 Topology} *)

  val connected_components: t -> interval list
  val first_connected_component: t -> interval
  val last_connected_component: t -> interval

  val interior: t -> t
  (** [interior x] is the {i interior} of the set [x] with respect to the 
  topology of the half-line/circle depending on the module from which it is 
  called.*)
  
  val closure: t -> t
  (** [closure x] is the {i closure} of the set [x] with respect to the 
  topology of the half-line/circle depending on the module from which it is 
  called.*)

  (** {2 Direction} *)

  val future_extension: t -> t -> t
  (** [future_extension x y] is the set of points {i q} of the union of [x] 
  and [y] such that there exists a point {i p} of [x] such the 
  interval/anticlockwise arc from {i p} to {i q} is contained in the union of 
  [x] and [y].*)

  val past_extension: t -> t -> t
  (** [past_extension x y] is the set of points {i q} of the union of [x] and 
  [y] such that there exists a point {i p} of [x] such the 
  interval/anticlockwise arc from {i q} to {i p} is contained in the union of 
  [x] and [y].*)

  (** {2 Display} *)

  val string_of: t -> string

  (** {2 Enumerator} *)

  val next: (value -> value) -> t -> t

end (* S *)

module Raw(I:NonEmptyInterval.S) = struct

exception Undefined

type value = I.value
type interval = I.t
type t = interval list

(* Internals *)

let (<<) it1 it2 = I.compare it1 it2 <= 0

let (<|<) it1 it2 = I.ordered_disjoint it1 it2

let head_and_tail a = 
  match a with 
  | it::a -> it,a
  | [] -> raise Undefined

(* Constructor *)

let of_interval it = [it]

(* Constants *)

let empty = []
let full = [I.terminal true I.zero]

(* Dealing with zero *)

let first_connected_component at = 
  match at with 
  | it :: _ -> it
  | _ -> raise Undefined

let rec last_connected_component at = 
  match at with
  | [it] -> it
  | _::at -> last_connected_component at
  | [] -> raise Undefined

let contains_zero at = 
  try I.contains_zero (first_connected_component at)
  with Undefined -> false

let add_zero at = 
  match at with
  | it :: at' -> (
    try (I.add_zero it) :: at'  
    with I.Undefined -> I.(atom zero) :: at)
  | [] -> [I.(atom zero)]

let remove_zero at = 
  match at with
  | it :: at -> (
    try (I.remove_zero it) :: at
    with I.Undefined -> at)
  | [] -> []

let is_neighbourhood_of_zero at =
  try I.is_neighbourhood_of_zero (first_connected_component at)
  with Undefined -> false

(* Tests *)

let is_empty a = (a = [])
let is_full a = (a = full)

let mem v at =
  let at = ref at in
  let answer = ref false in
  let () =
    try
      while true do
        match !at with
        | (it::at') -> (
          if (try I.mem v (I.strict_lower_bounds it) with I.Undefined -> false)
          then (answer := false; raise Exit)
          else if I.mem v it then (answer := true; raise Exit)
            else at := at')
        | _ -> raise Exit
      done 
    with Exit -> () in
  !answer

let is_included at1 at2 =
  let answer = ref false in
  let () = 
    try
      let at1 = ref at1 in
      let at2 = ref at2 in
      while true do
        match !at1,!at2 with
        | (it1::at3),(it2::at4) -> (
          if it1 <|< it2 then raise Exit 
          else if it2 <|< it1 then at2 := at4
            else if I.is_included it1 it2 then at1 := at3
              else raise Exit)
        | (_::_),[] -> raise Exit
        | _ -> (answer := true; raise Exit)
      done
    with Exit -> () in
  !answer
  
(* Boolean structure *)

let complement at =
  let answer = ref empty in
  try
    let at = ref at in
    let last = ref I.full in (* dummy value *)
    let () = 
      match !at with 
      | last' :: at' -> (
          answer := (
            try of_interval (I.strict_lower_bounds last') 
            with I.Undefined -> empty);
          last := last';
          at := at')
      | [] -> (answer := full; raise Exit) in
    while true do
      match !at with 
      | last' :: at' -> (
          answer := (I.between !last last') :: !answer;
          at := at';
          last := last')
      | [] -> (
          try answer := I.strict_upper_bounds !last :: !answer; raise Exit
          with I.Undefined -> raise Exit) 
    done;
    assert false
  with Exit -> List.rev !answer
  
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

let difference at1 at2 = meet at1 (complement at2)

let rec finish answer accu at = 
  match at with 
  | it :: at -> (
      try finish answer (I.ordered_join accu it) at
      with I.Undefined -> (List.rev (it :: accu :: answer)) @ at)
  | [] -> List.rev (accu :: answer) 

let join at1 at2 =
  let answer = ref [] in
  try
    let at1 = ref at1 in
    let at2 = ref at2 in
    let accu = ref (
      match !at1,!at2 with
      | (it1::at3),(it2::at4) -> (
          if it1 << it2
          then (at1 := at3; it1)
          else (at2 := at4; it2))
      | (_::_),[] -> (answer := !at1; raise Exit)
      | [],(_::_) -> (answer := !at2; raise Exit)
      | _ -> raise Exit) in
    let update it at at' =
      try accu := I.ordered_join !accu it; at := at'  
      with I.Undefined -> (
        answer := !accu :: !answer;
        accu := it;
        at := at') in
    while true do
      match !at1,!at2 with
      | (it1::at3),(it2::at4) -> (
          if it1 << it2
          then update it1 at1 at3
          else update it2 at2 at4)
      | (_::_),[] -> (answer := finish !answer !accu !at1; raise Exit)
      | [],(_::_) -> (answer := finish !answer !accu !at2; raise Exit)
      | _ -> assert false
    done;
    assert false
  with Exit -> !answer

(* Topology *)

let connected_components a = a

let is_bounded at =
  try I.is_bounded (last_connected_component at)
  with Undefined -> true

let interior_is_bounded = ref false

let rec interior at =
  match at with 
  | it :: at -> (
    let () = 
      if at = [] 
      then interior_is_bounded := I.is_bounded it in
    try (I.interior it) :: interior at 
    with I.Undefined -> interior at)
  | _ -> empty

let closure_is_bounded = ref true

let rec closure at =
  match at with
  | it1 :: ((it2 :: at'') as at') -> (
    let it1 = I.closure it1 in
    try 
      let it1 = I.ordered_join it1 (I.closure it2) in 
      closure (it1 :: at'')
    with I.Undefined -> it1 :: closure at')
  | [it] -> 
    let it = I.closure it in
    let () = closure_is_bounded := I.is_bounded it in
    of_interval it
  | [] -> closure_is_bounded := true; empty

(* Direction *)

let future_extension at1 at2 =
  let answer = ref [] in
  let () = 
    try
      let first_operand = ref false in
      let at1 = ref at1 in
      let at2 = ref at2 in
      let first_it1 = ref None in
      let accu = ref (
        match !at1,!at2 with
        | (it1::at3),(it2::at4) -> (
            if it1 << it2
            then (at1 := at3; first_it1 := Some it1; it1)
            else (at2 := at4; it2))
        | (_::_),[] -> (answer := !at1; raise Exit)
        | [],(_::_) -> (answer := empty; raise Exit)
        | _ -> raise Exit) in 
      let update it at at' =
        try
          accu := I.ordered_join !accu it; at := at'; 
          if !first_operand && !first_it1 = None then first_it1 := Some it;  
        with I.Undefined ->
          (match !first_it1 with 
          | Some it -> answer := (I.meet (I.terminal_hull it) !accu) :: !answer
          | None -> ());
          accu := it;
          (if !first_operand then first_it1 := Some it else first_it1 := None);
          at := at' in
      while true do
        match !at1,!at2 with
        | (it1::at3),(it2::at4) -> (
            if it1 << it2
            then (first_operand := true; update it1 at1 at3)
            else (first_operand := false; update it2 at2 at4))
        | (it1::at3),[] -> (first_operand := true; update it1 at1 at3)
        | [],(it2::at4) -> (first_operand := false; update it2 at2 at4)
        | [],[] -> (
            match !first_it1 with 
            | Some it -> answer := List.rev ((I.meet (I.terminal_hull it) !accu) :: !answer); raise Exit
            | None -> answer := List.rev !answer; raise Exit)
      done 
    with Exit -> () in
  !answer

let past_extension at1 at2 =
  let answer = ref [] in
  let () = 
    try
      let first_operand = ref false in
      let at1 = ref at1 in
      let at2 = ref at2 in
      let last_it1 = ref None in
      let accu = ref (
        match !at1,!at2 with
        | (it1::at3),(it2::at4) -> (
            if it1 << it2
            then (at1 := at3; last_it1 := Some it1; it1)
            else (at2 := at4; it2))
        | (_::_),[] -> (answer := !at1; raise Exit)
        | [],(_::_) -> (answer := empty; raise Exit)
        | _ -> raise Exit) in 
      let update it at at' =
        try
          accu := I.ordered_join !accu it; at := at'; 
          if !first_operand then last_it1 := Some it;  
        with I.Undefined -> 
          (match !last_it1 with
          | Some last -> answer := (I.meet (I.initial_hull last) !accu) :: !answer
          | None -> ());
          accu := it;
          if !first_operand then last_it1 := Some it else last_it1 := None;
          at := at' in
      while true do
        match !at1,!at2 with
        | (it1::at3),(it2::at4) -> (
            if it1 << it2
            then (first_operand := true; update it1 at1 at3)
            else (first_operand := false; update it2 at2 at4))
        | (it1::at3),[] -> (first_operand := true; update it1 at1 at3)
        | [],(it2::at4) -> (first_operand := false; update it2 at2 at4)
        | [],[] -> (
            match !last_it1 with 
            | Some it -> answer := List.rev ((I.meet (I.initial_hull it) !accu)::!answer); raise Exit
            | None -> answer := List.rev !answer; raise Exit)
      done 
    with Exit -> () in
  !answer


(* Display *)

let string_of a = 
  if is_empty a then "Ø"
  else
    let string_of = I.string_of "[" "]" "{" "}" "+oo" in
    List.fold_right (fun x accu -> (string_of x)^" "^accu) a "" 

(* Enumerator *)

let next next_value re = 
  let next it = I.next next_value it in
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
          | []      -> [I.atom I.zero])
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
      | [] -> [I.atom I.zero]
      | _ -> next_region (List.length re) re 
  with Exit -> init (succ (List.length re)) []

end (* Raw *)

module Make(I:NonEmptyInterval.S): S 
  with type value = I.value
  and type interval = I.t 
= Raw(I)
