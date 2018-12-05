module G = struct

  exception Undefined

  type vertex = int
  type arrow = int
  module S = Set.Make(struct type t = int let compare = compare end)
  type cone = { past:S.t ; future:S.t }
  module M = Map.Make(struct type t = int let compare = compare end)
  
  type neighbors = cone M.t
  
  type endpoints = (vertex * vertex) M.t
  
  type t = { endpoints:endpoints ; neighbors:neighbors }
  
  let get_neighbors v neighbors =
    try M.find v neighbors
    with Not_found -> 
      let past = S.empty in
      let future = S.empty in
      { past ; future }
  
  let get_future_neighbors v neigbors =
    (get_neighbors v neigbors).future
  
  let get_past_neighbors v neigbors =
    (get_neighbors v neigbors).past
  
  let from_endpoints endpoints =
    let update arrow (src,tgt) accu =
      let future = get_future_neighbors src accu in
      let future = S.add arrow future in
      let past = get_past_neighbors src accu in
      let accu = M.add src {past;future} accu in
      let past = get_past_neighbors tgt accu in 
      let past = S.add arrow past in
      let future = get_future_neighbors tgt accu in
      let accu = M.add tgt {past;future} accu in
      accu in 
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
    let ingoing_arrows = get_future_neighbors v g.neighbors in
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
    let neighbors = from_endpoints endpoints in
    {endpoints;neighbors}

  let of_string s =
    let s = Str.global_replace (Str.regexp "[\n\t]") "" s in
    let s = Str.global_replace (Str.regexp "\\([ ]\\)+") " " s in
    let () = print_endline s in
    let l = Str.split (Str.regexp ";") s in
    let l = List.map (fun a -> 
      let a = Str.split (Str.regexp " ") a in
      match a with
      | [src;tgt] -> (int_of_string src,int_of_string tgt)
      | _ -> assert false)
      l in
    of_list l

  let print_arrows g =
    let print_arrow a (src,tgt) = Printf.printf "%i >– %i –> %i\n" src a tgt in
    M.iter print_arrow g.endpoints 

  let print_neighbors g =
    let print_neighbor v {past;future} =
      if not( S.is_empty future) then (
       Printf.printf "%i >–\n" v ;
       S.iter (fun a -> Printf.printf "  %i –> %i\n" a (tgt a g)) future) ;
       if not( S.is_empty past) then (
       Printf.printf "%i <–\n" v ;
       S.iter (fun a -> Printf.printf "  %i –< %i\n" a (src a g)) past) in
    M.iter print_neighbor g.neighbors

end

module I = NonEmptyInterval.Raw(Integer)

module DD = DashDot.Raw(I)

module GR = GraphRegion.Raw(G)(DD)



(*
let g = G.of_string "1 2;
1 2"

let () =
  print_endline "endpoints";
  G.print_arrows g;
  print_endline "";
  print_endline "neighbors";
  G.print_neighbors g
*)
