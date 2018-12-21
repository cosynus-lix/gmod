module type Data = sig
  type t
  val string_of: t -> string
  val compare: t -> t -> int
end (* Data *)

module type S = sig
  exception Undefined
  type vertex
  type arrow
  type t
  val string_of_arrow: arrow -> string
  val string_of_vertex: vertex -> string
  val compare_vertex: vertex -> vertex -> int
  val compare_arrow: arrow -> arrow -> int
  val src: arrow -> t -> vertex
  val tgt: arrow -> t -> vertex
  val fold_vertex: (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_out: vertex -> (arrow -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_in: vertex -> (arrow -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_vertex: (vertex -> unit) -> t -> unit
  val iter_out: vertex -> (arrow -> unit) -> t -> unit
  val iter_in: vertex -> (arrow -> unit) -> t -> unit
end (* S *)

module Raw(V:Data)(A:Data) = struct

  exception Undefined

  type vertex = V.t
  type arrow = A.t
  let string_of_arrow = A.string_of
  let string_of_vertex = V.string_of
  let compare_arrow = A.compare
  let compare_vertex=  V.compare
  
  module S = Set.Make(struct type t = arrow let compare = A.compare end)
  
  type cone = { past:S.t ; future:S.t }
  
  let empty_cone = { past = S.empty ; future = S.empty }
  
  let is_empty_cone {past;future} = 
    S.is_empty past && S.is_empty future
  
  module AM = Map.Make(struct type t = arrow let compare = A.compare end)
  module VM = Map.Make(struct type t = vertex let compare = V.compare end)
  
  type neighbors = cone VM.t
  
  type endpoints = (vertex * vertex) AM.t
  
  type t = { endpoints:endpoints ; neighbors:neighbors }
  
  let empty = { endpoints = AM.empty ; neighbors = VM.empty }
  
  let get_neighbors v neighbors =
    try VM.find v neighbors
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
      if VM.mem v neighbors
      then neighbors
      else VM.add v empty_cone neighbors in
    { endpoints ; neighbors } 

  (* Warning: the add_arrow function requires that arrow does not appear in the 
  graph in which it has to be added. *)

  let add_arrow_to_neighbors src arrow tgt neighbors =
    let future = get_future_neighbors src neighbors in
    let future = S.add arrow future in
    let past = get_past_neighbors src neighbors in
    let neighbors = VM.add src {past;future} neighbors in
    let past = get_past_neighbors tgt neighbors in 
    let past = S.add arrow past in
    let future = get_future_neighbors tgt neighbors in
    let neighbors = VM.add tgt {past;future} neighbors in
    neighbors

  let add_arrow src arrow tgt {endpoints;neighbors} =
    let endpoints = AM.add arrow (src,tgt) endpoints in
    let neighbors = add_arrow_to_neighbors src arrow tgt neighbors in
    { endpoints ; neighbors }
  
  let neighbors_from_endpoints endpoints =
    let update arrow (src,tgt) accu =
      add_arrow_to_neighbors src arrow tgt accu in
    AM.fold update endpoints VM.empty 

  let get_endpoints arrow g = 
    try AM.find arrow g.endpoints
    with Not_found -> raise Undefined 
    
  let src arrow g = fst (get_endpoints arrow g)

  let tgt arrow g = snd (get_endpoints arrow g)

  let fold_vertex f g a = 
    VM.fold (fun k _ a -> f k a) g.neighbors a

  let fold_out v f g a = 
    let outgoing_arrows = get_future_neighbors v g.neighbors in
  S.fold f outgoing_arrows a

  let fold_in v f g a = 
    let ingoing_arrows = get_future_neighbors v g.neighbors in
  S.fold f ingoing_arrows a

  let iter_vertex f g = 
    VM.iter (fun k _ -> f k) g.neighbors

  let iter_out v f g = 
    let outgoing_arrows = get_future_neighbors v g.neighbors in
  S.iter f outgoing_arrows

  let iter_in v f g = 
    let ingoing_arrows = get_past_neighbors v g.neighbors in
  S.iter f ingoing_arrows

  let print_arrows g =
    let print_arrow a (src,tgt) = Printf.printf "%s: %s –> %s\n" 
      (A.string_of a) (V.string_of src) (V.string_of tgt) in
    AM.iter print_arrow g.endpoints 

  let print_neighbors g =
    let print_neighbor v {past;future} =
      if not(S.is_empty future) then (
       Printf.printf "future of %s:\n" (V.string_of v) ;
       S.iter (fun a -> Printf.printf "  %s: %s –> %s\n" (A.string_of a) (V.string_of v) (V.string_of (tgt a g))) future) ;
       if not(S.is_empty past) then (
       Printf.printf "past of %s:\n" (V.string_of v) ;
       S.iter (fun a -> Printf.printf "  %s: %s –> %s\n" (A.string_of a) (V.string_of (src a g)) (V.string_of v)) past) in
    VM.iter print_neighbor g.neighbors

  let print_vertices g =
    VM.iter (fun v _ -> Printf.printf "%s " (V.string_of v)) g.neighbors;
    print_endline ""

  let is_isolated v g = 
    try is_empty_cone (VM.find v g.neighbors)
    with Not_found -> false

  let print_isolated_vertices g =
    let something_was_printed = ref false in
    VM.iter (fun v _ -> 
      if is_isolated v g
      then (something_was_printed := true; Printf.printf "%s " (V.string_of v))) g.neighbors;
    if !something_was_printed then print_endline ""

end (* G *)

module Make(V:Data)(A:Data): S with type vertex = V.t and type arrow = A.t 
  = Raw(V:Data)(A:Data)
