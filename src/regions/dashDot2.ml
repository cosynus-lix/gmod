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

  (** {2 Enumerator} *)

  val next: (value -> value) -> t -> t
  
  (** {2 Boolean operators} *)
  
  val meet: t -> t -> t
  val join: t -> t -> t
  val complement: t -> t
  val difference: t -> t -> t
  
(*
  module type DirectedTopology = sig

    val string_of: t -> string
    (** Shape dependent string converter.*)

    val interior: t -> t
    (** [interior x] is the {i interior} of the set [x] with respect to the 
    topology of the half-line/circle depending on the module from which it is 
    called.*)
    
    val closure: t -> t
    (** [closure x] is the {i closure} of the set [x] with respect to the 
    topology of the half-line/circle depending on the module from which it is 
    called.*)

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

  end
  
  module HalfLine:DirectedTopology
  
  module Circle:DirectedTopology
*)

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

let (<<) it1 it2 = I.compare it1 it2 <= 0

let rec finish answer accu at = 
  match at with 
  | it :: at -> (
      try finish answer (I.ordered_join accu it) at
      with I.Undefined -> (List.rev (it :: accu :: answer)) @ at)
  | [] -> List.rev (accu :: answer) 

let difference at1 at2 = meet at1 (complement at2)

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

let counter = ref 0

let pir msg it = 
  Printf.printf "%s = %s\n" msg (I.string_of it)

let prr msg at = 
  Printf.printf "%s = %s\n" msg (string_of at)


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

module type DirectedTopology = sig
  val string_of: t -> string
  val interior: t -> t
  val closure: t -> t
  val future_extension: t -> t -> t
  val past_extension: t -> t -> t
end


module HalfLine = struct

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

let rec last_connected_component at = 
  match at with
  | [it] -> it
  | _::at -> last_connected_component at
  | [] -> raise Undefined

let is_bounded at = 
  try I.is_bounded (last_connected_component at)
  with Undefined -> true
  
let closure at = assert false

let interior at = assert false

let string_of at = assert false

let first_connected_component at = 
  match at with 
  | it :: _ -> it
  | _ -> raise Undefined


end (* HalfLine *)


  
let contains_zero at = 
  try I.contains_zero (HalfLine.first_connected_component at)
  with Undefined -> false

module Circle = struct

let future_extension at1 at2 =
  let at3 = HalfLine.future_extension at1 at2 in
  if HalfLine.is_bounded at3 || not (contains_zero at2)
  then at3
  else join (of_interval(HalfLine.first_connected_component at2)) at3

let past_extension at1 at2 =
  let at3 = HalfLine.past_extension at1 at2 in
  if contains_zero at3 && not (HalfLine.is_bounded at2)
  then join at3 (of_interval (HalfLine.last_connected_component at2))
  else at3

let closure at = assert false

let interior at = assert false

let string_of at = assert false

end (* Circle *)


end (* Raw *)

module Make(I:Interval2.S): S with type value = I.value and type interval = I.t 
  = Raw(I)
