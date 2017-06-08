(** Common functions. These mostly consist in helper functions extending OCaml's
    standard library, which are not specific to this application.*)

let id x = x

let fst3 (x,y,z) = x
let snd3 (x,y,z) = y
let thd3 (x,y,z) = z

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)

let lcm a b = a * (b / gcd a b)

let rec pow a b =
  if b = 0 then 1 else a * (pow a (b - 1))

let rec factorial n =
  if n = 0 then 1
  else n * (factorial (n - 1))

(* TODO: improve *)
let rec binomial n k =
  if k < 0 || k > n then 0 else
    factorial n / (factorial k * factorial (n - k))

let trinomial n k =
  let ans = ref 0 in
  for j = 0 to n do
    ans := !ans + (binomial n j) * (binomial j (k - j))
  done;
  !ans

let memoize f =
  let m = Hashtbl.create 1000 in
  fun x ->
    try
      Hashtbl.find m x
    with
      | Not_found ->
        let y = f x in
        Hashtbl.add m x y;
        y

module Enum = struct
  type 'a t = unit -> 'a

  exception End

  let empty =
    fun () -> raise End

  let get e = e ()

  (** Create an enum from a function returning the next element. The function
      should raise [End] if there are no more elements. *)
  let make f = f

  let append e1 e2 =
    fun () ->
      try
        get e1
      with
        | End -> get e2

  let map f e =
    fun () -> f (get e)

  let filter p e =
    let rec aux p e =
      let x = get e in
      if p x then
        x
      else
        aux p e
    in
    fun () -> aux p e

  let iter f e =
    try
      while true do
        f (get e)
      done
    with
      | End -> ()

  let fold f init e =
    let ans = ref init in
    try
      while true do
        ans := f !ans (get e)
      done;
      assert false
    with
      | End -> !ans

  let to_list e =
    List.rev (fold (fun l x -> x::l) [] e)

  let reduce f e =
    try
      let init = get e in
      fold f init e
    with
      | End -> raise Not_found

  let indexes n =
    let k = ref (-1) in
    fun () ->
      incr k;
      if !k < n then
        !k
      else
        raise End
end

module Array = struct
  include Array

  let exists p a =
    try
      for i = 0 to Array.length a - 1 do
        if p a.(i) then raise Exit
      done;
      false
    with
      | Exit -> true

  let existsi p a =
    try
      for i = 0 to Array.length a - 1 do
        if p i a.(i) then raise Exit
      done;
      false
    with
      | Exit -> true

  let for_all p a =
    try
      for i = 0 to Array.length a - 1 do
        if not (p a.(i)) then raise Exit
      done;
      true
    with
      | Exit -> false

  let for_all2 p a1 a2 =
    let l = Array.length a1 in
    assert (l = Array.length a2);
    try
      for i = 0 to l - 1 do
        if not (p a1.(i) a2.(i)) then raise Exit
      done;
      true
    with
      | Exit -> false

  let iter2 f a1 a2 =
    let l = Array.length a1 in
    assert (l = Array.length a2);
    for i = 0 to l - 1 do
      f a1.(i) a2.(i)
    done

  let map2 f a1 a2 =
    let l = Array.length a1 in
    assert (l = Array.length a2);
    Array.init l (fun i -> f a1.(i) a2.(i))

  let reduce f a =
    if Array.length a = 0 then raise (Invalid_argument "Array.reduce");
    let ans = ref a.(0) in
    for i = 1 to Array.length a - 1 do
      ans := f !ans a.(i);
    done;
    !ans

  let remove a i =
    Array.init
      (Array.length a - 1)
      (fun k -> if k < i then a.(i) else a.(i+1))

  let enum a =
    let i = ref (-1) in
    fun () ->
      incr i;
      if !i < Array.length a then
        a.(!i)
      else
        raise Enum.End

  let findi p a =
    let ans = ref None in
    try
      for i = 0 to Array.length a - 1 do
        if p a.(i) then
          (
            ans := Some i;
            raise Exit
          )
      done;
      raise Exit
    with
      | Exit ->
        (
          match !ans with
            | Some i -> i
            | None -> raise Not_found
        )

  let index x a =
    findi (fun y -> y = x) a

  let extract inds a = Array.init (Array.length inds) (fun i -> a.(inds.(i)))

  (* Returns the next integer in the collection of integers that can be written 
  with at most Array.length a digits in base n. The stronger digit is the 
  right most one.*)

  let cartesian_next_array_consumer a n =
    let () = if a = [||] then raise Exit in
    let la = Array.length a in
    let pos = ref 0 in
    while n <= a.(!pos) do
      incr pos;
      if !pos = la then raise Enum.End
    done;
    for k = 0 to !pos-1 do
      a.(k) <- 0
    done;
    a.(!pos) <- a.(!pos)+1

  (* As cartesian_next_array_consumer except that the number or allowed digits 
  now depends on the position of the digit. Arrays a and b should be of the same 
  size.*)

  let cartesian_next_array_consumer2 a b =
    let () = if a = [||] then raise Exit in
    let la = Array.length a in
    let pos = ref 0 in
    while b.(!pos) <= a.(!pos) do
      incr pos;
      if !pos = la then raise Enum.End
    done;
    for k = 0 to !pos-1 do
      a.(k) <- 0
    done;
    a.(!pos) <- a.(!pos)+1


  let shuffle a =
    let permute i j =
      let tmp = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- tmp
    in
    let l = Array.length a in
    if l>=2 then
      for i=0 to l-2 do
        permute i (i + Random.int (l-i))
      done

  (*Find the first subarray (from left) of a which matches the array p(a)*)

  (*The –contiguous– subarrays of some array α are in one-to-one
    correspondance with the pairs (start,len) with 0 ⩽ start ⩽ λ(α)-1
    and 0 ⩽ len ⩽ λ(α)-start, where λ(α) denotes the length of
    α. Hence they are ordered by the lexicographical order over these
    pairs. Hence the first/last occurence of β in α is the least
    subarray of α that matches β according to this –total– order.*)

  (*TODO: add an option "backward"*)

  let first_occurence s a =
    let l = Array.length a in
    let len = Array.length s in
    let answer = ref (0,0,[||]) in (*dummy default*)
    try
      for start = 0 to l-len do
	let candidate = Array.sub a start len in
	if (try for i=0 to len-1 do if s.(i)<>a.(start+i) then raise Exit done;true
	  with Exit -> false)
	then (answer:=start,len,candidate;raise Exit)
      done;
      raise Not_found
    with Exit -> !answer

  let nth_occurence n s a =
    let k = ref n in
    let l = Array.length a in
    let len = Array.length s in
    let answer = ref (0,0,[||]) in (*dummy default*)
    try
      for start = 0 to l-len do
	let candidate = Array.sub a start len in
	if (try for i=0 to len-1 do if s.(i)<>a.(start+i) then raise Exit done;true
	  with Exit -> false)
	then (if !k=0 then (answer:=start,len,candidate;raise Exit) else decr k)
      done;
      raise Not_found
    with Exit -> !answer

  let first_satisfying p a =
    let l = Array.length a in
    let answer = ref (0,0,[||]) in (*dummy default*)
    try
      for start = 0 to l-1 do
	for len = 0 to l-start do
	  let candidate = Array.sub a start len in
	  if p candidate
	  then (answer:=start,len,candidate;raise Exit)
	done
      done;
      raise Not_found
    with Exit -> !answer

  (*TODO: first_shortest_satisfying, first_longest_satisfying *)

  let nth_satisfying n p a =
    let k = ref n in
    let l = Array.length a in
    let answer = ref (0,0,[||]) in (*dummy default*)
    try
      for start = 0 to l-1 do
	for len = 0 to l-start do
	  let candidate = Array.sub a start len in
	  if p candidate && !k=0
	  then (answer:=start,len,candidate;raise Exit)
	  else decr k
	done
      done;
      raise Not_found
    with Exit -> !answer

  let replace_first s t a =
    let (start,len,_) = first_occurence s a in
    let sl = start+len in
    Array.concat [Array.sub a 0 start;t;Array.sub a sl ((Array.length a)-sl)]

  let alter_first_satisfying f p a =
    let start,len,s = first_satisfying p a in
    let sl = start+len in
    Array.concat [Array.sub a 0 start;f s;Array.sub a sl ((Array.length a)-sl)]

  let replace_nth n s t a =
    let (start,len,_) = nth_occurence n s a in
    let sl = start+len in
    Array.concat [Array.sub a 0 start;t;Array.sub a sl ((Array.length a)-sl)]

  (*If all the bindings of the map satisfy the test or all of them do not satisfy
  it, then the common result of the test is returned as Some b, otherwise None is
  returned.*)

  let share test a =
    try
      Array.fold_left
	(
	  fun x accu -> match accu with
	  | Some b -> if b = test x then accu else raise Exit
	  | None -> Some (test x)
	) None a
    with Exit -> None
end

module Filename = struct
  include Filename

  let expand_tilde f =
    if String.length f > 0 then
      if f.[0] = '~' then
        Sys.getenv "HOME" ^ String.sub f 1 (String.length f - 1)
      else f
    else f

  (* proceed ?output_file:o c i opens the "input file" i, perform "things" from
     the channel associated with i. The results are printed on screen if no
     output file is provided. Otherwise they are printed in file o. *)

  let proceed ?(ovrwrt=false) ?output_file calculation input_file =
    let flags =
      let open Sys in
      if ovrwrt
      then [Open_binary;Open_trunc;Open_creat]
      else [Open_binary;Open_creat;Open_excl] in
    let input_channel =
      try open_in_bin input_file
      with Sys_error _ as err ->
        let () = Printf.printf "Unable to open file %s\n" input_file in
        raise err in
    let output_channel = match output_file with
      | Some out_f ->
        (
          try open_out_gen flags 0o640 out_f
          with Sys_error _ as err -> (Printf.printf "Unable to create file %s: overwrite not allowed\n" out_f; raise err)
        )
      | None -> stdout in
    try calculation input_channel output_channel
    with err -> close_in input_channel; close_out output_channel; raise err
end

module List = struct
  include List

  let uniq_common ?(equal=(=)) l1 l2 =
    (*
      let equal = match equal with
      | Some equal -> equal
      | None -> (=) in
    *)
    let found = ref false in
    try
      let () =
        List.iter
          (fun x1 ->
            if List.exists (equal x1) l2
            then
              if !found
              then raise Exit
              else found := true) l1 in
      !found
    with Exit -> false

  let rev_linearize l =
    let f accu x = if List.mem x accu then accu else x :: accu in
    List.fold_left f [] l

  (*let linearize l = List.rev (rev_linearize l)*)

  let linearize l =
    let rec linearize accu l = match l with
      | x :: l -> if List.mem x accu then linearize accu l else x :: linearize (x :: accu) l
      | [] -> [] in
    linearize [] l

  let rec counting_occurences ?(accu=0) x l = match l with
    | y :: l -> if x = y then counting_occurences ~accu:(accu + 1) x l else counting_occurences ~accu x l
    | [] -> accu

  let rec appears_at_least_n_times b x l = match l with
    | y :: l -> (b <= 0) ||
        if x = y then appears_at_least_n_times (b-1) x l else appears_at_least_n_times b x l
    | [] -> (b <= 0)

  let remove_elements_occuring_at_least_n_times b l =
    if b > 1
    then
      let test x = not (appears_at_least_n_times b x l) in
      List.filter test l
    else []

  let rec make n x =
    if n > 0
    then x :: (make (n-1) x)
    else []

  let rec init n f =
    if n > 0
    then (f (n-1)) :: (init (n-1) f)
    else []

(*2011/12/31 14:29:13: new version of segment_initial_croissant*)

  let rec initial_segment ?(start=0) k =
    if k = 0 then [] else start :: (initial_segment ~start:(start + 1) (k-1))

  (* TODO: improve this *)
  let init n f = List.rev (init n f)

(*

  let exists p l =
    let rec aux = function
      | h::t -> if p h then raise Exit; aux t
      | [] -> () in
    try aux l; false
    with Exit -> true

*)

  let rec last l =
    match l with
      | [x] -> x
      | _::t -> last t
      | [] -> raise Not_found

  let rev_map f l = rev (map f l)

  let pmap (f:('a -> 'b)) l =
    let result = ref [] in
    iter
      (fun x ->
        let i, o = Unix.pipe () in
        match Unix.fork () with
          | 0 ->
            (
              try
                Unix.close i;
                Marshal.to_channel (Unix.out_channel_of_descr o) (f x) [Marshal.Closures];
                exit 0
              with
                | _ -> exit 1
            )
          | pid ->
            Unix.close o;
            result := (pid, Unix.in_channel_of_descr i) :: !result
      )
      l;
    rev_map
      (fun (pid, i) ->
        match Unix.waitpid [] pid with
          | _, Unix.WEXITED 0 -> (Marshal.from_channel i: 'b)
          | _ -> failwith "List.pmap"
      )
      !result

  let iteri f l =
    let rec aux n = function
      | h::t -> f n h; aux (n+1) t
      | [] -> ()
    in
    aux 0 l

  let iter_tl f l =
    let rec aux = function
      | x::t -> f x t; aux t
      | [] -> ()
    in
    aux l

  let map_tl f l =
    let rec aux = function
      | x::t -> (f x t)::(aux t)
      | [] -> []
    in
    aux l

  let mapi f l =
    let rec aux n = function
      | h::t -> (f n h)::(aux (n+1) t)
      | [] -> []
    in
    aux 0 l

  let rec may = function
    | (Some x)::t -> x::(may t)
    | None::t -> may t
    | [] -> []

  let rec may_map f = function
    | x::t ->
      (
        match f x with
          | Some y -> y::(may_map f t)
          | None -> may_map f t
      )
    | [] -> []

  let filter_map = may_map

  let rec may_rev_map f l =
    let rec aux accu = function
      | x::t ->
	let accu =
          match f x with
            | Some y -> y::accu
            | None -> accu
	in
	aux accu t
      | [] -> accu
    in
    aux [] l

  let may_mapi f l =
    let rec aux n = function
      | x::t ->
        (
          match f n x with
            | Some y -> y::(aux (n+1) t)
            | None -> aux (n+1) t
        )
      | [] -> []
    in
    aux 0 l
  let filter_mapi = may_mapi

  let map_ctxt f l =
    let rec aux b = function
      | [] -> []
      | h::t -> (f b h t)::(aux (b@[h]) t)
    in
    aux [] l

  let iter_pairs f l =
    ignore (map_ctxt (fun _ x t -> List.iter (fun y -> f x y) t) l)

  let find_map f l =
    let ans = ref None in
      List.iter
        (fun x ->
          if !ans = None then
            ans := f x
        ) l;
    (
      match !ans with
      | Some y -> y
      | None -> raise Not_found
    )

  let enum l =
    let l = ref l in
    fun () ->
      match !l with
        | x::t ->
          l := t;
          x
        | [] -> raise Enum.End

  let rec uniq = function
    | x::t -> if mem x t then (uniq t) else x::(uniq t)
    | [] -> []

  let rec nth n l =
    match n,l with
      | _, [] -> raise Not_found
      | 0, x::_ -> x
      | n, _::t -> nth (n-1) t

  let assoc_all x l =
    may_map (fun (y,v) -> if x = y then Some v else None) l

  (*Replace all occurences of the sequence s by the sequence t in the
    sequence l. The function is actually defined by the following
    induction relation replace_all ω ω' w⋅ω⋅w' = w⋅ω'⋅(replace_all ω
    ω' w') where w does not contain ω as a subword. In particular
    replace_all raise an exception if ω is the empty word – indeed the
    algorithm does not terminate in this case.*)

  let replace_on_the_fly s t l =
    let rec replace_on_the_fly l =
      let rec replace s l = match s with
	| x::s' ->
	  (match l with
	    | y::l' -> if x<>y then None else replace s' l'
	    | []    -> None)
	| [] -> Some l
      in
      match l with
	| x::l' ->
	  (match replace s l with
	    | Some l' -> List.append t (replace_on_the_fly l')
	    | None -> x::(replace_on_the_fly l'))
	| [] -> []
    in
    if s<>[]
    then replace_on_the_fly l
    else raise (Invalid_argument "Common.List.replace_all")


  (*Replace all occurences of the shortest matching of sequence s
    satisfying p(s) by the sequence f(s) in the sequence l. The
    function is actually defined by the following induction relation
    replace_all ω ω' w⋅ω⋅w' = w⋅ω'⋅(replace_all ω ω' w') where w does
    not contain ω as a subword. In particular replace_all raise an
    exception if ω is the empty word – indeed the algorithm does not
    terminate in this case. TODO*)


  (*TODO*)

  let first_shortest_match p l = ()


  let replace_first s t l =
    let rec replace_first l =
      let rec replace s l = match s with
	| x::s' ->
	  (match l with
	    | y::l' -> if x<>y then None else replace s' l'
	    | []    -> None)
	| [] -> Some l
      in
      match l with
	| x::l' ->
	  (match replace s l with
	    | Some l' -> List.append t l'
	    | None -> x::(replace_first l'))
	| [] -> []
    in
    replace_first l

  let replace_nth ?signal n s t l =
    let () = match signal with
      | Some signal -> signal := false
      | None -> ()
    in
    let n = ref n in
    let rec replace_nth l =
      let rec replace s l = match s with
	| x::s' ->
	  (match l with
	    | y::l' -> if x<>y then None else replace s' l'
	    | []    -> None)
	| [] ->
	  if !n=0
	  then Some l
	  else (decr n;None)
      in
      match l with
	| x::l' ->
	  (match replace s l with
	    | Some l' ->
	      let () = match signal with Some signal -> signal := true | None -> ()
	      in List.append t l'
	    | None -> x::(replace_nth l'))
	| [] -> if s <> [] || (!n > List.length l) then [] else (let () = match signal with Some signal -> signal := true | None -> ()
							     in t)
    in
    replace_nth l

  let replace_all s t l =
    let signal = ref true in
    let answer = ref [] in
    let n = ref 0 in
    while !signal
    do
      signal:=false;
      let x = replace_nth ~signal !n s t l in
      incr n;
      if !signal
      then answer := x :: !answer
    done;
    !answer

  (*If s is a prefix of l, i.e. l = s @ l' then l' is returned*)
  let rec remaining s l = match s with
    | x :: s ->
      (match l with
      | y :: l -> if x = y then remaining s l else None
      | [] -> None)
    | [] -> Some l

  let replace_all ?(init=[]) s t l =
    let rec replace_all found head tail =
      if tail <> []
      then
        let next_head , next_tail = ((List.hd tail) :: head) , (List.tl tail) in
        match remaining s tail with
        | Some tail' ->
          let replaced = List.rev_append head (t @ tail') in
          if List.mem replaced found
          then replace_all found next_head next_tail
          else replace_all (replaced :: found) next_head next_tail
        | None -> replace_all found next_head next_tail
      else
        if s <> []
        then found
        else ((List.rev head) @ t) :: found in
    replace_all init [] l

  let symmetrize lop = linearize (lop @ (List.map (fun (x,y) -> (y,x)) lop))

  let saturate ?depth relations terms =
    let counter = ref 0 in
    let keep_going = match depth with
    | Some n -> (fun k -> k < n)
    | None -> (fun k -> true) in
    let answer = ref terms in
    let current_length = ref (-1) in
    let () = while keep_going !counter && !current_length < (List.length !answer)
      do
        let () = current_length := List.length !answer in
        let () =
          List.iter
            (fun (s,t) ->
              (List.iter
                (fun a_term -> answer := replace_all ~init:(!answer) s t a_term)
                !answer))
          relations in
        incr counter
      done in
    !answer

  (* This function may raise the exception Failure "hd" *)

  let rec next_in_list x l =
    if x = List.hd l
    then List.hd (List.tl l)
    else next_in_list x (List.tl l)

  let disjoint l1 l2 =
    let rec is_meeting l = match l with
    | x :: l -> if List.mem x l2 then raise Exit else is_meeting l
    | [] -> true in
    try is_meeting l1 with Exit -> false

  (*br is a (symmetric) binary relation over the elements of the list, the
    function returns a list of list each element of which being the connected
    component. Prerequites: ll is a partition and x does not appear in any block of
    the partition.*)

  let gather_one x br ll =
    let (to_be_gathered,others) = List.partition (fun l -> List.exists (fun y -> br x y || br y x) l) ll in
    (x :: (List.concat to_be_gathered)) :: others

  let gather br l = List.fold_left (fun accu x -> gather_one x br accu) l []

  let distribute p l1 l2 = List.fold_left (fun accu x1 -> List.fold_left (fun accu x2 -> p x1 x2 :: accu) accu l2) [] l1

end(*List*)

module Set = struct
  module type OrderedType = Set.OrderedType

  module type S = sig
     include Set.S

     val map : (elt -> elt) -> t -> t

     val enum : t -> elt Enum.t

     val of_enum : elt Enum.t -> t
  end

  module Make (O : OrderedType) = struct
    include (Set.Make(O))

    let of_list l =
      List.fold_left (fun s x -> add x s) empty l

    let enum s =
      List.enum (elements s)

    let of_enum e =
      let ans = ref empty in
      try
        while true do
          ans := add (Enum.get e) !ans
        done;
        assert false
      with
        | Enum.End -> !ans

    let map f s =
      fold (fun x s -> add (f x) s) s empty
  end
end

module String = struct
  include String

  let concat_enum sep e =
    let ans = ref "" in
    let first = ref true in
    try
      while true do
        if not !first then
          ans := !ans ^ sep
        else
          first := false;
        ans := !ans ^ Enum.get e
      done;
      assert false
    with
      | Enum.End -> !ans

  let concat_map s f l =
    concat s (List.map f l)

  let is_int s =
    try
      for i = 0 to String.length s - 1 do
        if int_of_char s.[i] < int_of_char '0' || int_of_char s.[i] > int_of_char '9' then
          raise Exit
      done;
      true
    with
      | Exit -> false

  let remove_final_endline s =
    let n = String.length s in
    if s<>"" && s.[n-1] = '\n'
    then String.sub s 0 (n-1)
    else s

  let longest_length sl =
    let f accu s = max accu (String.length s) in
    List.fold_left f 0 sl

  let string_as_columns ?(shift=0) ?row_separator ?(column_separator="") ?column_width terminal_width sl =
    let column_separator_width = String.length column_separator in
    let left_margin = String.make shift ' ' in
    let position = ref 0 in
    match column_width with
      | Some column_width ->
          let row_separator = match row_separator with
            | None -> ""
            | Some s -> s ^ "\n" in
                let string_format s = s ^ (String.make (max 0 (column_width - (String.length s))) ' ') in
                List.fold_left
                  (
                    fun accu s ->
                      if !position <> 0
                      then
                        let k = !position + column_width + column_separator_width in
                        if k <= terminal_width
                        then (position := k ; accu ^ column_separator ^ (string_format s))
                        else (position := column_width + shift; accu ^ "\n" ^ row_separator ^ left_margin ^ (string_format s))
                      else (position := column_width + shift; accu ^ left_margin ^ (string_format s))
                  ) "" sl
      | None ->
          let row_separator = match row_separator with
            | None -> ""
            | Some s -> s ^ "\n" in
                let string_format s = s(*^(String.make (max 0 (column_width-(String.length s))) ' ')*) in
                List.fold_left
                  (
                    fun accu s ->
                      if !position <> 0
                      then
                        let k = !position + (String.length s) + column_separator_width in
                        if k <= terminal_width
                        then (position := k; accu ^ column_separator ^ (string_format s))
                        else (position := (String.length s) + shift; accu ^ "\n" ^ row_separator ^ left_margin ^ (string_format s))
                      else (position := (String.length s) + shift; accu ^ left_margin ^ (string_format s))
                  ) "" sl


let string_as_columns_list ?(shift=0) ?(column_separator="") ?(align=false) ?column_width terminal_width sl =
  let column_separator_width = String.length column_separator in
  let position = ref 0 in
  let column_width = match column_width with
    | Some x -> (fun s -> max x (String.length s))
    | None ->
        if align
        then (let lmax = longest_length sl in (fun s -> lmax))
        else String.length in
  let string_format s = s ^ (String.make (max 0 ((column_width s) - (String.length s))) ' ') in
  let f (line,accu) s =
    if !position <> 0
      then
        let k = !position + (column_width s) + column_separator_width in
        if k <= terminal_width
        then (position := k; ((string_format s) :: line, accu))
        else (position := shift; ([], ((List.rev ([string_format s]@line)) :: accu)))
      else (position := (column_width s) + shift; ([string_format s] @ line, accu)) in
  let aux = List.fold_left f ([],[]) sl in
  List.rev ((List.rev (fst aux))::(snd aux))

let superscript_digit d = match d with
  | '0' -> "⁰" | '1' -> "¹" | '2' -> "²" | '3' -> "³" | '4' -> "⁴"
  | '5' -> "⁵" | '6' -> "⁶" | '7' -> "⁷" | '8' -> "⁸" | '9' -> "⁹"
  |  _  -> ""

let lowerscript_digit d = match d with
  | '0' -> "₀" | '1' -> "₁" | '2' -> "₂" | '3' -> "₃" | '4' -> "₄"
  | '5' -> "₅" | '6' -> "₆" | '7' -> "₇" | '8' -> "₈" | '9' -> "₉"
  |  _  -> ""

let exponent_string ?(utf8=true) n =
  let pattern = string_of_int n in
  if utf8
  then
    let l = String.length pattern in
    Array.fold_left (fun accu d -> accu^d) ""
      (Array.init l (fun k -> superscript_digit pattern.[k]))
  else "^"^pattern

let index_string ?(utf8=true) n =
  let pattern = string_of_int n in
  if utf8
  then
    let l = String.length pattern in
    Array.fold_left (fun accu d -> accu^d) ""
      (Array.init l (fun k -> lowerscript_digit pattern.[k]))
  else "^"^pattern

end

module Matrix = struct
  type 'a t = 'a array array

  let make = Array.make_matrix

  let height m = Array.length m

  let width m = Array.length m.(0)

  module Int = struct
    type t = int array array

    let mult a b =
      assert (width a = height b);
      let ans = make (height a) (width b) 0 in
      for i = 0 to height a - 1 do
        for j = 0 to width b - 1 do
          for k = 0 to width a - 1 do
            ans.(i).(j) <- ans.(i).(j) + a.(i).(k) * b.(k).(j)
          done
        done
      done;
      ans

    let is_zero m =
      try
        for i = 0 to height m - 1 do
          let mi = m.(i) in
          for j = 0 to width m - 1 do
            if mi.(j) <> 0 then raise Exit
          done
        done;
        true
      with
        | Exit -> false

    let incr m i j = m.(i).(j) <- m.(i).(j) + 1

    let decr m i j = m.(i).(j) <- m.(i).(j) - 1

  (*
    let gauss m =
  *)

  (*
  (** Compute the Smith normal form of the matrix. *)
    let smith m =
    assert false;
    m
  *)
  end
end

let duration f =
  let start = Unix.gettimeofday () in
  let output = f () in
  let t = Unix.gettimeofday () -. start in
  (output, t)

let memory ?(full_major=true) f =
  let mem () =
    let s = Gc.stat () in
    let m = s.Gc.live_words * (Sys.word_size / 8) in
    (* Printf.printf "mem: %d\n%!" m; *)
    m
  in
  if full_major then
    Gc.full_major ();
  let before = mem () in
  let output = f () in
  let after = mem () in
  output, after - before, max before after

let print_duration s f =
  Printf.printf "Computing %s... %!" s;
  let x, t = duration f in
  Printf.printf "done in %F s.\n%!" t;
  x

let print_stats s f =
  Gc.full_major ();
  Printf.printf "Computing %s... %!" s;
  let (x, t), m, mm = memory ~full_major:false (fun () -> duration f) in
  Printf.printf "done in %F s using %d bytes (total %d bytes).\n%!" t m mm;
  x

(* TODO: audit uses of this function in the code. Actually occurs in several pieces of code written by Emmanuel.*)

let get_some ?d x = match x with
	| Some x -> x
	| None -> (match d with Some d -> d | None -> raise Not_found)

let bool_of_option x = match x with Some _ -> true | None -> false

(** Returns the content of textfile whose name is given as an argument*)

let file_content p =
  try
    let c = open_in p in
    let file_content = ref "" in
    let () =
      try
				while true
				do file_content := !file_content^(input_line c)^"\n" done
      with End_of_file -> seek_in c 0 (*set the cursor position at the beginning of the file*)
    in close_in c; !file_content
  with Sys_error _ -> failwith (Printf.sprintf "unable to open/close the file %s." p)

let lines_of_file p =
  try
    let c = open_in p in
    let file_content = ref [] in
    let () =
      try
				while true
				do file_content := (input_line c) :: !file_content done
      with End_of_file -> seek_in c 0 (*set the cursor position at the beginning of the file*)
    in close_in c; List.rev !file_content
  with Sys_error _ -> failwith (Printf.sprintf "unable to open/close the file %s." p)

let fill_file ?(ovrwrt=false) lines filename =
if not ovrwrt && Sys.file_exists filename
then failwith "File %S already exists [Common.fill_file]"
else
  let c = open_out filename in
  let () = List.iter (fun line -> Printf.fprintf c "%s\n" line) lines in
  close_out c


(** Some useful tricks to customize terminal outputs *)

module Terminal =

struct

  (* Clear screen and set the cursor position of the top left corner when print *)

  let width = ref 120

  let clear = "\027[2J\027[H"

  let set_cursor_position row column = Printf.sprintf "\027[%i;%if" row column

let deco ?(active=true) ?(start=0) ?length ?(dim=false) ?(bold=false) ?(crossed_out=false) ?(underline=false) str =
  let max_length = (String.length str) - start in
  let length = match length with
    | Some x -> if x <= max_length then x else max_length
    | None -> max_length in
  if active
  then
    (let param = ref [] in
    let () = if bold        then param := (1::!param) in
    let () = if dim         then param := (2::!param) in
    let () = if underline   then param := (4::!param) in
    let () = if crossed_out then param := (9::!param) in
    if !param = []
    then str
    else
      let param = String.concat ";" (List.map string_of_int !param) in
      let pre = String.sub str 0 start in
      let highlighten_substring = String.sub str start length in
      let post =
        let k = length + start in
        String.sub str k (String.length str - k) in
      pre^"\027["^param^"m"^highlighten_substring^"\027[0m"^post)
  else str

type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

let color ?(active=true) ?(start=0) ?length ?(dim=false) ?(bold=false) ?(crossed_out=false) ?(underline=false) c str =
  let color = match c with
    | Black -> 30
    | Red -> 31
    | Green -> 32
    | Yellow -> 33
    | Blue -> 34
    | Magenta -> 35
    | Cyan -> 36
    | White -> 37 in
  let max_length = (String.length str) - start in
  let length = match length with
    | Some x -> if x <= max_length then x else max_length
    | None -> max_length in
  if active
  then
    (let param = ref [color] in
    let () = if bold        then param := (1::!param) in
    let () = if dim         then param := (2::!param) in
    let () = if underline   then param := (4::!param) in
    let () = if crossed_out then param := (9::!param) in
    let param = String.concat ";" (List.map string_of_int !param) in
    let pre = String.sub str 0 start in
    let highlighten_substring = String.sub str start length in
    let post =
      let k = length + start in
      String.sub str k (String.length str - k) in
    pre^"\027["^param^"m"^highlighten_substring^"\027[0m"^post)
  else str

(* Color values actually range from 0 to 255 according to the
   following ANSI norm:

   0   - 15  : 16-colors mode
   16  - 231 : 216 = 6³ colors in RGB mode
   232 - 255 : grey scale from black to white. *)

(* If none of r, g, b is provided then no highlight is made. Moreover
   we have 0 ⩽ r, g, b ⩽ 5. If r, g, or b is below 0 (resp. above 5)
   then it is set to 0 (resp. 5). *)

  let rgb ?(active=true) ?(start=0) ?length ?(bold=false) ?(dim=false) ?(crossed_out=false) ?(underline=false) r g b str =
    let color_component x =
      if x < 0
      then 0
      else
        if x > 5
        then 5
        else x in
    let color = 16 (*due to ANSI norm*)
      + (color_component r) * 36 (*red*)
      + (color_component g) * 6  (*green*)
      + (color_component b)      (*blue*) in
    let max_length = (String.length str) - start in
    let length = match length with
      | Some x -> if x <= max_length then x else max_length
      | None -> max_length in
    if active
    then
      (let param = ref [38;5;color] in
      let () = if bold        then param := (1::!param) in
      let () = if dim         then param := (2::!param) in
      let () = if underline   then param := (4::!param) in
      let () = if crossed_out then param := (9::!param) in
      let param = String.concat ";" (List.map string_of_int !param) in
      let pre = String.sub str 0 start in
      let highlighten_substring = String.sub str start length in
      let post =
        let k = length + start in
        String.sub str k (String.length str - k) in
      pre^"\027["^param^"m"^highlighten_substring^"\027[0m"^post)
    else str

(* One should respect 0 ⩽ light ⩽ 23. Moreover if light is below 0
   (resp. above 23) then it is set to 0 (resp. 23). *)

let grey ?(active=true) ?(start=0) ?length ?(bold=false) ?(dim=false) ?(crossed_out=false) ?(underline=false) light str =
  let grey_scale_component x = (*due to ANSI norm*)
    if x < 0
    then 232
    else
      if x > 23
      then 255
      else x + 232 in
  let color = grey_scale_component light in
  let max_length = (String.length str) - start in
  let length = match length with
    | Some x -> if x <= max_length then x else max_length
    | None   -> max_length in
  if active
  then
    (let param = ref [38;5;color] in
    let () = if bold        then param := (1::!param) in
    let () = if dim         then param := (2::!param) in
    let () = if underline   then param := (4::!param) in
    let () = if crossed_out then param := (9::!param) in
    let param = String.concat ";" (List.map string_of_int !param) in
    let pre = String.sub str 0 start in
    let post = let k = length + start in String.sub str k (String.length str - k) in
    let highlighten_substring = String.sub str start length in
    pre^("\027["^param^"m"^highlighten_substring^"\027[0;39m")^post)
  else str

let untag s =
  let tag = Str.regexp "\027\\[\\([0-9]+;\\)*[0-9]*[f m J H]" in
  Str.global_substitute tag (fun s -> "") s

(*The function is based on the following hack: the accented letters are represented by two characters the first of
which being '\195'. The string s must not be tagged.*)

let length s =
  let length = ref 0 in
  let incr_if_not_char_195 c = if c <> '\195' then incr length in
  let () = String.iter incr_if_not_char_195 s in
  !length

(*The argument mode must be some anagram of "rgb" – uppercases are allowed*)

let color_shade mode =
  let mode = String.uppercase_ascii mode in
  let () =
    if not (List.mem mode ["RGB";"RBG";"BRG";"BGR";"GRB";"GBR"])
    then raise (Invalid_argument "color_shade") in
  let component c x y z =
    let n = String.index mode c in
      match n with
        | 0 -> x | 1 -> y | 2 -> z
        | _ -> failwith "Should not happen [color_shade]" in
  let () = Printf.printf "Available colors with their %s%s%s coordinates\n"
    (rgb 5 0 0 "R") (rgb 0 5 0 "G") (rgb 0 0 5 "B") in
  for x = 0 to 5 do
    for y = 0 to 5 do
      for z = 0 to 5 do
        let r = component 'R' x y z in
        let g = component 'G' x y z in
        let b = component 'B' x y z in
        Printf.printf "%s (%i,%i,%i)  " (rgb r g b "Color") r g b
        done; print_endline ""
    done
  done

let grey_scale () =
  let () = print_endline "Available grey scale" in
  for light = 0 to 23 do
    Printf.printf "%s (%i)\n" (grey light "Grey") light
  done

let error e = Printf.sprintf "%s %s" (color ~bold:true Red "Error:") e

let warning w = Printf.sprintf "%s %s" (color ~bold:true Yellow "Warning:") w

let success s = Printf.sprintf "%s: %s" s (color ~bold:true Green "done")

let information i = Printf.sprintf "%s %s" (color ~bold:true Blue "Info:") i

let scream s = Printf.printf "%s%s%s\n%!" (color ~bold:true Magenta "<") s (color ~bold:true Magenta ">")

let spy ?(verbose=false) b msg_true msg_false =
  let display msg = if msg <> "" then print_string msg in
  let () =
    if verbose
    then
    (if b then (display msg_true) else (display msg_false)) in
  b

end (*Terminal*)

(* IntSet : Set of integers *)
module IntSet =
struct

  include Set.Make(struct type t = int let compare = Pervasives.compare end)

  let from_array a = Array.fold_right (fun term accu -> (add term accu)) a empty

  let from_list l = List.fold_right (fun term accu -> (add term accu)) l empty

  let rec segment_initial ?(start=empty) k =
    if k > 0 then segment_initial ~start:(add (k-1) start) (k-1) else start

  let rec image_segment_initial f ?(start=empty) k =
    if k > 0 then image_segment_initial f ~start:(add (f (k-1)) start) (k-1) else start

end

(* StringSet: Set of strings *)
module StringSet =
struct
  include Set.Make(struct type t = string let compare = Pervasives.compare end)
  let longest s = fold (fun s accu -> max accu (String.length s)) s 0
end

module StringMap = Map.Make(struct type t = string let compare = Pervasives.compare end)

(* Moi : Maps whose keys are integers *)
module Moi = struct
  include Map.Make (struct type t = int let compare = Pervasives.compare let default = 0 end)
  let make converter list_of_bindings =
    List.fold_left (fun accu (i,s) -> add i (converter s) accu) empty list_of_bindings
  let dod m =
    fold (fun key x accu -> IntSet.add key accu) m IntSet.empty
  let to_array neutral m =
    let aux = dod m in
    let least = IntSet.min_elt aux and greatest = IntSet.max_elt aux in
    (least,(Array.init (greatest-least+1) (fun k -> find (k-least) m)))
end
