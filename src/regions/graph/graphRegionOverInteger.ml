let normalize_string s =
  let s = Str.global_replace (Str.regexp "[\n\t]") "" s in
  Str.global_replace (Str.regexp "\\([ ]\\)+") " " s

let semicolon_split s = Str.split (Str.regexp ";") s

let colon_split s = Str.split (Str.regexp ":") s

let words s = Str.split (Str.regexp "[ ]") s

module G = struct

  exception Undefined

  type vertex = int
  type arrow = int
  let string_of_arrow = string_of_int
  let string_of_vertex = string_of_int
  module S = Set.Make(struct type t = vertex let compare = compare end)
  type cone = { past:S.t ; future:S.t }
  
  let empty_cone = { past = S.empty ; future = S.empty }
  
  let is_empty_cone {past;future} = 
    S.is_empty past && S.is_empty future
  
  module M = Map.Make(struct type t = vertex let compare = compare end)
  
  type neighbors = cone M.t
  
  type endpoints = (vertex * vertex) M.t
  
  type t = { endpoints:endpoints ; neighbors:neighbors }
  
  let empty = { endpoints = M.empty ; neighbors = M.empty }
  
  let get_neighbors v neighbors =
    try M.find v neighbors
    with Not_found -> 
      let past = S.empty in
      let future = S.empty in
      { past ; future }
  
  let get_future_neighbors v neighbors =
    (get_neighbors v neighbors).future
  
  let get_past_neighbors v neighbors =
    (get_neighbors v neighbors).past

  let add_vertex v {endpoints;neighbors} =
    let neighbors = 
      if M.mem v neighbors
      then neighbors
      else M.add v empty_cone neighbors in
    { endpoints ; neighbors } 

  (* Warning: the add_arrow function requires that arrow does not appear in the 
  graph in which it has to be added. *)

  let add_arrow_to_neighbors src arrow tgt neighbors =
    let future = get_future_neighbors src neighbors in
    let future = S.add arrow future in
    let past = get_past_neighbors src neighbors in
    let neighbors = M.add src {past;future} neighbors in
    let past = get_past_neighbors tgt neighbors in 
    let past = S.add arrow past in
    let future = get_future_neighbors tgt neighbors in
    let neighbors = M.add tgt {past;future} neighbors in
    neighbors

  let add_arrow src arrow tgt {endpoints;neighbors} =
    let endpoints = M.add arrow (src,tgt) endpoints in
    let neighbors = add_arrow_to_neighbors src arrow tgt neighbors in
    { endpoints ; neighbors }
  
  let neighbors_from_endpoints endpoints =
    let update arrow (src,tgt) accu =
      add_arrow_to_neighbors src arrow tgt accu in
    M.fold update endpoints M.empty 

  let compare_vertex = compare

  let compare_arrow = compare

  let get_endpoints arrow g = 
    try M.find arrow g.endpoints
    with Not_found -> raise Undefined 
    
  let src arrow g = fst (get_endpoints arrow g)

  let tgt arrow g = snd (get_endpoints arrow g)

  let fold_vertex f g a = 
    M.fold (fun k _ a -> f k a) g.neighbors a

  let fold_out v f g a = 
    let outgoing_arrows = get_future_neighbors v g.neighbors in
  S.fold f outgoing_arrows a

  let fold_in v f g a = 
    let ingoing_arrows = get_future_neighbors v g.neighbors in
  S.fold f ingoing_arrows a

  let iter_vertex f g = 
    M.iter (fun k _ -> f k) g.neighbors

  let iter_out v f g = 
    let outgoing_arrows = get_future_neighbors v g.neighbors in
  S.iter f outgoing_arrows

  let iter_in v f g = 
    let ingoing_arrows = get_past_neighbors v g.neighbors in
  S.iter f ingoing_arrows

(*
  let fold_arrow f g a = M.fold (fun k _ a -> f k a) g.endpoints a
  let iter_arrow f g = M.iter (fun k _ -> f k) g.endpoints
*)

  let of_list l =
    let counter = ref 0 in
    let add_an_arrow a (src,tgt) = 
      let arrow = !counter in
      let () = incr counter in
      M.add arrow (src,tgt) a in
    let endpoints = List.fold_left add_an_arrow M.empty l in
    let neighbors = neighbors_from_endpoints endpoints in
    {endpoints;neighbors}

  let of_string s =
    let s = normalize_string s in
    let l = semicolon_split s in
    let l = List.map (fun a -> 
      let a = Str.split (Str.regexp " ") a in
      match a with
      | [src;tgt] -> (int_of_string src,int_of_string tgt)
      | _ -> assert false)
      l in
    of_list l

  let print_arrows g =
    let print_arrow a (src,tgt) = Printf.printf "%i: %i –> %i\n" a src tgt in
    M.iter print_arrow g.endpoints 

  let print_neighbors g =
    let print_neighbor v {past;future} =
      if not(S.is_empty future) then (
       Printf.printf "future of %i:\n" v ;
       S.iter (fun a -> Printf.printf "  %i: %i –> %i\n" a v (tgt a g)) future) ;
       if not(S.is_empty past) then (
       Printf.printf "past of %i:\n" v ;
       S.iter (fun a -> Printf.printf "  %i: %i –> %i\n" a (src a g) v) past) in
    M.iter print_neighbor g.neighbors

  let print_vertices g =
    M.iter (fun v _ -> Printf.printf "%i " v) g.neighbors;
    print_endline ""

  let is_isolated v g = 
    try is_empty_cone (M.find v g.neighbors)
    with Not_found -> false

  let print_isolated_vertices g =
    let something_was_printed = ref false in
    M.iter (fun v _ -> 
      if is_isolated v g
      then (something_was_printed := true; Printf.printf "%i " v)) g.neighbors;
    if !something_was_printed then print_endline ""

end (* G *)

module I = HalfLineRegionOverInteger.I

module HL = HalfLineRegionOverInteger.HL

module R = GraphRegion.Raw(G)(HL) 

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
          let dd = HalfLineRegionOverInteger.of_string dd in
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
