let normalize_string s =
  let s = Str.global_replace (Str.regexp "[\n\t]") "" s in
  Str.global_replace (Str.regexp "\\([ ]\\)+") " " s

let semicolon_split s = Str.split (Str.regexp ";") s

let colon_split s = Str.split (Str.regexp ":") s

let words s = Str.split (Str.regexp "[ ]") s

module G = GraphOverInteger.G

module I = HalfLineOverInteger.I

module HL = HalfLineOverInteger.HL

module R = OnGraph.Raw(G)(HL) 

module GnGR = struct 
  
  let add_arrow src arrow tgt dd (g,r) =
    let g = G.add_arrow src arrow tgt g in
    let r = R.add_arrow arrow dd r in
    (g,r)

  let add_vertex b v (g,r) = 
    let g = G.add_vertex v g in
    let r = if b then R.add_vertex v r else r in
    (g,r)

(*
  A half-line is described as a sequence on intervals
  
  Intervals are 
  
  [a b] [a b[ ]a b[ ]a b] (bounded with a < b)

  [a oo[ ]a oo[ (unbounded)

  {a} (singleton)

  A vertex is added to the underlying graph by 
    <vertex> ;
  
  or 
  
    <vertex>: ;
    
  A vertex is added to the underlying graph and to the region by 
    <vertex>:* ;
    
  An arrow is added to the underlying graph and to the region by
    <source> <target> : <description of a region on half-line> ;
  
  Different arrows may have the same source and the same target.
*)

  let of_string s =
    let s = normalize_string s in
    let l = semicolon_split s in
    let counter = ref 0 in
    let of_string (g,r) s =
      let l = colon_split s in
      match l with
      | [src_tgt ; dd] -> (
        let arrow = !counter in
        let () = incr counter in
        match (words src_tgt) with
        | [src;tgt] ->
          let src = int_of_string src in
          let tgt = int_of_string tgt in
          let dd = HalfLineOverInteger.of_string dd in
          add_arrow src arrow tgt dd (g,r)
        | [v] -> (
            let v = int_of_string v in
            let dd = Str.global_replace (Str.regexp "[ ]+") "" dd in
            let b = dd = "*" in
            let () = assert (b || dd = "") in
            add_vertex (dd="*") v (g,r))
        | _ -> assert false) 
      | [s] -> (
          let l = words s in
          match l with 
          | [src;tgt] -> (
            let arrow = !counter in
            let () = incr counter in
            let src = int_of_string src in
            let tgt = int_of_string tgt in
            add_arrow src arrow tgt HL.empty (g,r))
          | [v] -> (
            let v = int_of_string v in
            add_vertex false v (g,r)) 
          | _ -> assert false)
      | _ -> assert false
    in
    List.fold_left of_string (G.empty,R.empty) l

let print (g,r) =
  print_endline "endpoints";
  G.print_arrows g;
  print_endline "";
  print_endline "neighbors";
  G.print_neighbors g;
  print_endline "";
  print_endline "vertices";
  R.VSet.iter (fun v -> Printf.printf "%i " v) r.R.vertices;
  print_endline "";
  print_endline "";
  print_endline "arrows";
  R.AMap.iter (fun a dd -> Printf.printf "%i: %i –> %i: %s\n" 
    a (G.src a g) (G.tgt a g) 
    (HL.string_of dd)) 
    r.arrows;
  print_endline ""

let zero_normalize (g,r) = g , R.zero_normalize g r

end (* GR *)
