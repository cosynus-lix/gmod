open Common

(*
module Vector = struct
  (* TODO: add length? *)
  (* TODO: handle big BVecs! *)
  type t = int

  let create n =
    if n >= Sys.word_size - 1 then
      (
        Printf.eprintf "n: %d\n%!" n;
        assert (n < Sys.word_size - 1)
      );
    0

  let empty n = create n

  let singleton _ i = 1 lsl i

  (* Vector with [0,n[ as elements. *)
  let initial_segment n =
    (1 lsl n) - 1

  let full n = initial_segment n

  (** Total order on boolean vectors. *)
  let compare v1 v2 = v1 - v2

  let cardinal v =
    let rec aux n =
      if n = 0 then
        0
      else
        (n land 1) + aux (n lsr 1)
    in
    aux v

  let is_empty v = v = 0

  let add i v =
    v lor (singleton 0 i)

  let union v1 v2 =
    v1 lor v2

  let mem i v =
    v land (singleton 0 i) <> 0

  let diff v1 v2 =
    v1 land (lnot v2)

  let included v1 v2 =
    diff v1 v2 = 0

  let inter v1 v2 =
    v1 land v2

  let remove i v =
    diff v (singleton 0 i)

  (* TODO: add length *)
  let of_list l =
    List.fold_left (fun v n -> add n v) (create 1) l

  let enum v =
    let v = ref v in
    let n = ref 0 in
    let shift () =
      incr n;
      v := !v lsr 1
    in
    let e () =
      while !v land 1 = 0 do
        if !v = 0 then raise Enum.End;
        shift ()
      done;
      let ans = !n in
      shift ();
      ans
    in
    Enum.make e

  let to_string len v =
    let v = ref v in
    let n = ref 0 in
    let ans = ref "" in
    while !v <> 0 do
      ans := !ans ^ (if !v land 1 <> 0 then "1" else "0");
      incr n;
      v := !v lsr 1
    done;
    !ans ^ (String.make (len - String.length !ans) '0')

  (* TODO: faster by not looking a zeros *)
  let iter p v =
    let v = ref v in
    let n = ref 0 in
      while !v <> 0 do
        if !v land 1 <> 0 then
          p !n;
        incr n;
        v := !v lsr 1
      done

  let filter p v =
    let ans = ref 0 in
    iter (fun n -> if p n then ans := add n !ans) v;
    !ans

  let for_all p v =
    try
      iter (fun n -> if not (p n) then raise Exit) v;
      true
    with
      | Exit -> false

  let fold f v i =
    let ans = ref i in
    iter (fun n -> ans := f n !ans) v;
    !ans

  (** Pick an element from v. *)
  let pick v =
    let ans = ref None in
    try
      iter (fun n -> ans := Some n; raise Exit) v;
      raise Not_found
    with
      | Exit -> get_some !ans
end
  *)

module Vector = struct
  type t =
      {
        n : int;
        v : string;
      }

  (* Bytes necessary to store things up to n-1. *)
  let bytes n =
    if n = 0 then 0 else (n - 1) / 8 + 1

  let create n =
    {
      n = n;
      v = String.make (bytes n) '\000';
    }

  let empty n = create n

  let copy v =
    {
      n = v.n;
      v = String.copy v.v;
    }

  let set i v =
    let b = i / 8 in
    let o = i - b * 8 in
    let v = v.v in
    v.[b] <- char_of_int (int_of_char v.[b] lor (1 lsl o))

  let unset i v =
    let b = i / 8 in
    let o = i - b * 8 in
    let v = v.v in
    v.[b] <- char_of_int (int_of_char v.[b] land (lnot (1 lsl o)))

  let mem i v =
    let b = i / 8 in
    let o = i - b * 8 in
    let v = v.v in
    int_of_char v.[b] land (1 lsl o) <> 0

  let singleton n i =
    let ans = create n in
    set i ans;
    ans

  let last_mask n =
    let b = bytes n in
    let last = n - (b - 1) * 8 in
    1 lsl last - 1

  let last_mask_char n c =
    let m = last_mask n in
    let c = int_of_char c in
    char_of_int (c land m)

  let initial_segment n =
    let b = bytes n in
    let v = String.make b '\255' in
    let ans = { n = n; v = v; } in
    v.[b-1] <- char_of_int (last_mask n);
    ans

  let full n = initial_segment n

  (** Total order on boolean vectors. *)
  let compare v1 v2 =
    let b = String.length v1.v in
    let n = v1.n in
    let v1 = v1.v in
    let v2 = v2.v in
    let ans = ref (compare (last_mask_char n v1.[b-1]) (last_mask_char n v2.[b-1])) in
    for i = b - 2 downto 0 do
      if !ans = 0 then
        ans := compare v1.[i] v2.[i]
    done;
    !ans

  let is_empty v =
    let n = v.n in
    let v = v.v in
    let b = String.length v in
    let ans = ref true in
    for i = 0 to b - 2 do
      if v.[i] <> '\000' then ans := false
    done;
    if last_mask_char n v.[b-1] <> '\000' then ans := false;
    !ans

  let add i v =
    let ans = copy v in
    set i ans;
    ans

  let init n p =
    let ans = ref (create n) in
    for i = 0 to n - 1 do
      if p i then ans := add i !ans
    done;
    !ans

  let union v1 v2 =
    let ans = create v1.n in
    let v = ans.v in
    let v1 = v1.v in
    let v2 = v2.v in
    let b = String.length v1 in
    for i = 0 to b - 1 do
      v.[i] <- char_of_int ((int_of_char v1.[i]) lor (int_of_char v2.[i]))
    done;
    ans

  let diff v1 v2 =
    let ans = create v1.n in
    let v = ans.v in
    let v1 = v1.v in
    let v2 = v2.v in
    let b = String.length v1 in
    for i = 0 to b - 1 do
      v.[i] <- char_of_int ((int_of_char v1.[i]) land (lnot (int_of_char v2.[i])))
    done;
    ans

  let included v1 v2 =
    is_empty (diff v1 v2)

  let inter v1 v2 =
    let ans = create v1.n in
    let v = ans.v in
    let v1 = v1.v in
    let v2 = v2.v in
    let b = String.length v1 in
    for i = 0 to b - 1 do
      v.[i] <- char_of_int ((int_of_char v1.[i]) land (int_of_char v2.[i]))
    done;
    ans

  let remove i v =
    (* diff v (singleton v.n i) *)
    let v = copy v in
    let b = i / 8 in
    let o = i - b * 8 in
    let vv = v.v in
    vv.[b] <- char_of_int (int_of_char vv.[b] land (lnot (1 lsl o)));
    v

  let of_list len l =
    List.fold_left (fun v n -> add n v) (create len) l

  (* TODO: this improved below *)
  let enum v =
    let n_max = v.n in
    let n = ref 0 in
    let rec e () =
      let i = !n in
      incr n;
      if i >= n_max then raise Enum.End;
      if mem i v then i else e ()
    in
    Enum.make e

  let enum v =
    let n_max = v.n in
    let v = v.v in
    let b = ref 0 in
    let n = ref 0 in
    let x = ref (int_of_char v.[0]) in
    let rec e () =
      if !n >= n_max then raise Enum.End;
      if !x land 1 <> 0 then
        (
          let ans = !n in
          incr n;
          x := !x lsr 1;
          ans
        )
      else if !x = 0 then
        (
          incr b;
          n := !b * 8;
          if !n >= n_max then raise Enum.End;
          x := int_of_char v.[!b];
          e ()
        )
      (* Special case for speeding up sparse vectors. *)
      else if !x land 7 = 0 then
        (
          n := !n + 3;
          x := !x lsr 3;
          e ()
        )
      else
        (
          incr n;
          x := !x lsr 1;
          e ()
        )
    in
    Enum.make e

  let iter p v =
    Enum.iter p (enum v)

  let to_string v =
    let byte_to_string len v =
      let v = ref (int_of_char v) in
      let n = ref 0 in
      let ans = ref "" in
      while !v <> 0 do
        ans := !ans ^ (if !v land 1 <> 0 then "1" else "0");
        incr n;
        v := !v lsr 1
      done;
      let rem = len - String.length !ans in
      if rem = 0 then
        !ans
      else
        !ans ^ (String.make rem '0')
    in
    let len = v.n in
    let v = v.v in
    let ans = ref "" in
    let b = (len - 1) / 8 in
    for i = 0 to b - 1 do
      ans := !ans ^ byte_to_string 8 v.[i]
    done;
    let l = len - b * 8 in
    ans := !ans ^ byte_to_string l v.[b];
    !ans

  let cardinal v =
    let ans = ref 0 in
    iter (fun _ -> incr ans) v;
    !ans

  let filter p v =
    let ans = ref (create v.n) in
    iter (fun n -> if p n then ans := add n !ans) v;
    !ans

  let for_all p v =
    try
      iter (fun n -> if not (p n) then raise Exit) v;
      true
    with
      | Exit -> false

  let fold f v i =
    let ans = ref i in
    iter (fun n -> ans := f n !ans) v;
    !ans

  (** Pick an element from v. *)
  let pick v =
    let ans = ref None in
    try
      iter (fun n -> ans := Some n; raise Exit) v;
      raise Not_found
    with
      | Exit -> get_some !ans
end

module Matrix = struct
  type t = Vector.t array

  let compare m1 m2 =
    let ans = ref 0 in
    (
      try
        Array.iter2
          (fun v1 v2 ->
            let d = compare v1 v2 in
            if d <> 0 then
              (
                ans := d;
                raise Exit
              )
          ) m1 m2
      with
        | Exit -> ()
    );
    !ans

  let create n m =
    Array.init n (fun _ -> Vector.create m)

  let dim m =
    let v = m.(0) in
    Array.length m, v.Vector.n

  let full n m =
    Array.init n (fun _ -> Vector.full m)

  let add (i,j) m =
    Array.init
      (Array.length m)
      (fun k ->
        if k = i then
          Vector.add j m.(i)
        else
          m.(k)
      )

  let mem (i,j) m =
    Vector.mem j m.(i)

  let union m1 m2 =
    Array.map2 Vector.union m1 m2

  let diff m1 m2 =
    Array.map2 Vector.diff m1 m2

  let included m1 m2 =
    Array.for_all2 Vector.included m1 m2

  let is_empty m =
    Array.for_all Vector.is_empty m

  let inter m1 m2 =
    Array.map2 Vector.inter m1 m2

  let remove (i,j) m =
    Array.mapi
      (fun i' v ->
        if i' = i then
          Vector.remove j v
        else
          v
      ) m

  let for_all p m =
    try
      for i = 0 to Array.length m - 1 do
        if not (Vector.for_all (fun j -> p (i,j)) m.(i)) then
          raise Exit
      done;
      true
    with
      | Exit -> false

  let cardinal m =
    Array.fold_left (fun c v -> c + Vector.cardinal v) 0 m

  let fold f m init =
    let ans = ref init in
    for i = 0 to Array.length m - 1 do
      ans := Vector.fold (fun j -> f (i,j)) m.(i) !ans
    done;
    !ans

  let enum m =
    let m = Array.map Vector.enum m in
    let i = ref 0 in
    let rec e () =
      try
        !i, m.(!i) ()
      with
        | Enum.End ->
          incr i;
          if !i = Array.length m then
            raise Enum.End
          else
            e ()
    in
    Enum.make e

  let of_enum m n e =
    Enum.fold (fun m xy -> add xy m) (create m n) e

  let iter f m =
    Enum.iter f (enum m)

  let map f m =
    let ans = ref [] in
    iter (fun p -> ans := f p :: !ans) m;
    List.rev !ans

  let row m i = m.(i)

  let iter_rows f m =
    Array.iter f m

  let map_rows f m =
    Array.map f m

  let mapi_rows f m =
    Array.mapi f m

  let transpose mat =
    let m = Array.length mat in
    let n = mat.(0).Vector.n in
    of_enum m n (Enum.map (fun (i,j) -> (j,i)) (enum mat))

  let of_list m n l =
    List.fold_left (fun m xy -> add xy m) (create m n) l

  let to_string mat =
    let ans = ref "" in
    Array.iter (fun v -> ans := !ans ^ Vector.to_string v ^ "\n") mat;
    !ans

  let append_columns m n =
    let _, x = dim m in
    let _, y = dim n in
    assert (x = y);
    Array.append m n
end
