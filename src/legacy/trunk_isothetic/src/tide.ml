open Type

(** Iterate a function over a list of lists, choosing an element in each list. *)
let list_iter_all f l =
  let rec aux hh = function
    | [] -> f (List.rev hh)
    | h::t -> List.iter (fun x -> aux (x::hh) t) h
  in
    aux [] l

(** Same as [list_iter_all] but returns the list of results. *)
let list_map_all f l =
  let ans = ref [] in
    list_iter_all (fun l -> ans := (f l) :: !ans) l;
    List.rev !ans

let list_iter_context f l =
  let rec aux h = function
    | [] -> ()
    | x::t -> f (List.rev h) x t; aux (x::h) t
  in
    aux [] l

let rec list_filter_context f l =
  let rec aux h = function
    | [] -> []
    | x::t ->
        if f (List.rev h) x t then
          x::(aux (x::h) t)
        else
          aux (x::h) t
  in
    aux [] l

let compute area =
  let dim = AC.dimension area in
  let cubes = AC.fold (fun c l -> c::l) area [] in
  let tides =
    let v =
      let v = ref [] in
        (* TODO: do something which is not arch dependent... *)
        assert (dim <= Sys.word_size - 2);
        for i = 1 to (1 lsl dim) - 2 do
          v := (Array.init dim (fun n -> i lsr n mod 2 = 1)) :: !v
        done;
        !v
    in
      List.map
        (fun c ->
           List.map
             (fun v ->
                let tide = ref [] in
                  for i = 0 to dim - 1 do
                    tide := ((if v.(i) then C.before else C.after) i c) :: !tide
                  done;
                  AC.make !tide
             ) v
        ) cubes
  in
  let tides =
      list_map_all
        (fun l ->
           List.fold_left AC.meet (AC.full ~d:dim ()) l
        ) tides
  in
  Printf.printf "Tides: %d\n%!" (List.length tides);
  List.iter (fun t -> Printf.printf "-\n%!"; AC.print t) tides;
  (*
  let components =
    let rec ans = ref [] in
    let rec aux a tides =
      list_iter_context
        (fun h x t ->
           let ctxt = h@t in
           let a' = AC.meet a x in
           let ctxt = List.fold_left (fun cc c -> AC.join cc (AC.meet a' c)) (AC.empty ~d:dim ()) ctxt in
             aux a' t;
             aux a t
        ) tides
    in
      aux (AC.full ~d:dim ()) tides;
      !ans
  in
  *)
  let components =
    let ans = Array.make (List.length tides + 1) [] in
    let rec aux n a = function
      | [] -> ()
      | x::t ->
          let m = AC.meet a x in
            ans.(n+1) <- m :: ans.(n+1);
            aux (n+1) m t;
            aux n a t
    in
      aux 0 (AC.full ~d:dim ()) tides;
      for i = 0 to Array.length ans - 1 do
        Printf.printf "*** T%d:\n%!" i;
        List.iter (fun c -> Printf.printf "-\n%!"; AC.print c) ans.(i)
      done;
      for i = 1 to Array.length ans - 1 do
        let b = List.fold_left AC.join (AC.empty ~d:dim ()) ans.(i) in
        let b = AC.normalize b in
          ans.(i-1) <- List.map (fun a -> AC.difference a b) ans.(i-1)
      done;
      Printf.printf "after removal\n%!";
      for i = 0 to Array.length ans - 1 do
        Printf.printf "*** T%d:\n%!" i;
        List.iter (fun c -> Printf.printf "-\n%!"; AC.print c) ans.(i)
      done;
      List.flatten (Array.to_list ans)
  in
  (* Remove empty components *)
  let components = List.filter (fun c -> not (AC.is_empty c)) components in
  (* Remove duplicate components *)
  let components = list_filter_context (fun _ c t -> List.for_all (fun c' -> AC.compare c c' <> 0) t) components in
  (*
  (* Remove sub-components *)
  let components =
    list_filter_context
      (fun h c t -> not (List.exists (fun c' -> AC.is_included c c') (h@t))) components
  in
  *)
    components

let display area =
  let components = compute area in
  let nb = List.fold_left (fun n c -> n + (Co.cardinal (Co.connected c))) 0 components in
    Printf.printf "Components: %d\n%!" nb;
    List.iter (fun c -> Printf.printf "-\n%!"; AC.print c) components

let to_string area =
  let answer = ref "" in
  let add_line s = answer := !answer ^ s in
  let components = compute area in
  let nb = List.fold_left (fun n c -> n + (Co.cardinal (Co.connected c))) 0 components in
    add_line (Printf.sprintf "Components: %d\n%!" nb);
    List.iter (fun c -> add_line (Printf.sprintf "%s\n-\n%!" (AC.string_of c))) components;
    !answer
