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
  
  module M = Map.Make(struct type t = arrow let compare = compare end)
  
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
      if not( S.is_empty future) then (
       Printf.printf "future of %i:\n" v ;
       S.iter (fun a -> Printf.printf "  %i: %i –> %i\n" a v (tgt a g)) future) ;
       if not( S.is_empty past) then (
       Printf.printf "past of %i:\n" v ;
       S.iter (fun a -> Printf.printf "  %i: %i –> %i\n" a (src a g) v) past) in
    M.iter print_neighbor g.neighbors

end (* G *)

module DDI = DashDotOverInteger

module I = DDI.I

module DD = DDI.DD

module GR = struct 
  
  include GraphRegion.Raw(G)(DD)
  
  let add_arrow src arrow tgt dd (g,r) =
    let g = G.add_arrow src arrow tgt g in
    let r = add_arrow arrow dd r in
    (g,r)

  let add_vertex b v (g,r) = 
    let g = G.add_vertex v g in
    let r = if b then add_vertex v r else r in
    (g,r)

(*
  A vertex is added by 
    <vertex> ;
  An arrow is added by
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
          let dd = DDI.of_string dd in
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
            add_arrow src arrow tgt DD.empty (g,r))
          | [v] -> (
            let v = int_of_string v in
            add_vertex false v (g,r)) 
          | _ -> assert false)
      | _ -> assert false
    in
    List.fold_left of_string (G.empty,empty) l

end (* GR *)

let print (g,r) =
  print_endline "endpoints";
  G.print_arrows g;
  print_endline "";
  print_endline "neighbors";
  G.print_neighbors g;
  print_endline "";
  print_endline "vertices";
  GR.VSet.iter (fun v -> Printf.printf "%i " v) r.GR.vertices;
  print_endline "";
  print_endline "";
  print_endline "arrows";
  GR.AMap.iter (fun a dd -> Printf.printf "%i: %i –> %i: %s\n" 
    a (G.src a g) (G.tgt a g) 
    (DD.HalfLine.string_of dd)) 
    r.arrows;
  print_endline ""

let zero_normalize (g,r) = g , GR.zero_normalize g r

(* Tests *)

let x1 = 
"
0 1:;
1 2:;
1:*
"

let x1 = GR.of_string x1

let x1 = zero_normalize x1

let () = print_endline "x1"; print x1

let x2 = 
"
0 1:[0 oo[;
1 2:;
1:
"

let x2 = GR.of_string x2

let x2 = zero_normalize x2

let () = print_endline "x2"; print x2

let () = print_endline "calcul de x3"

let x3 = 
  let g = fst x1 in
  let r = GR.past_extension g (snd x1) (snd x2) in 
  (g,r)

let () =
  print_endline "x3";
  print x3