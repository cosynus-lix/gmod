module type Graph = sig
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
end (* Graph *)

module type S = sig
  type vertex
  type arrow
  type graph
  type t
end (* S *)

module Raw(G:Graph)(DD:HalfLineRegion.S) = struct

  type arrow = G.arrow
  type vertex = G.vertex
  type graph = G.t
  
  module VSet = Set.Make(struct type t = vertex let compare = G.compare_vertex end)
  module AMap = Map.Make(struct type t = arrow let compare = G.compare_arrow end)

  type t = { vertices:VSet.t ; arrows: DD.t AMap.t }
  
  (* Invariant: if v belongs to «vertices» then zero belongs to every half-line 
  region carried by an arrow outgoing from v; if v does not belong to  
  «vertices» then none of the half-line regions carried by the arrows outgoing 
  from v contains zero. An arrow is an entry of the dictionnary «arrows» if and 
  only if the half-line region it carries is not empty. *)

  let debug = true

  let print_arrows msg arrows = if debug then (
    print_endline msg;
    AMap.iter (fun a dd -> 
      Printf.printf "%s : %s\n"
        (G.string_of_arrow a)
        (DD.string_of dd)
    ) arrows)

  let print_vertices msg vertices =
    if debug then (
      print_endline msg;
      print_string "{ ";
      VSet.iter (fun v -> print_string ((G.string_of_vertex v)^" ")) vertices;
      print_endline "}")
  
  let empty = { vertices = VSet.empty ; arrows = AMap.empty }

  let is_empty {vertices;arrows} =
    VSet.is_empty vertices && AMap.is_empty arrows 

  let full graph = 
    let update v (vertices,arrows) = 
      let vertices = VSet.add v vertices in
      let f = fun a arrows -> AMap.add a DD.full arrows in
      let arrows = G.fold_out v f graph arrows in
      (vertices,arrows) in
    let empty = (VSet.empty,AMap.empty) in
    let (vertices,arrows) = 
      G.fold_vertex update graph empty in
    { vertices ; arrows }  

  let add_vertex v {vertices;arrows} =
    let vertices = VSet.add v vertices in
    { vertices ; arrows }

  let add_arrow a dd {vertices;arrows} =
    let arrows = 
      if DD.is_empty dd 
      then AMap.remove a arrows
      else AMap.add a dd arrows in
    { vertices ; arrows }

  let get_dd a arrows = 
    try AMap.find a arrows 
    with Not_found -> DD.empty
  
  let binary_boolean_operator graph v_op dd_op =
    fun r1 r2 -> 
      let vertices = v_op r1.vertices r2.vertices in
      let add v arrows =
        let add a arrows =
          let dd1 = get_dd a r1.arrows in
          let dd2 = get_dd a r2.arrows in
          let result = dd_op dd1 dd2 in
          let result = 
            if VSet.mem v vertices 
            then DD.add_zero result
            else DD.remove_zero result in
          if DD.is_empty result 
          then AMap.remove a arrows
          else AMap.add a result arrows in 
        G.fold_out v add graph arrows in
      let arrows = G.fold_vertex add graph AMap.empty in
      { vertices ; arrows }

  let difference graph = binary_boolean_operator graph VSet.diff DD.difference
  let meet graph = binary_boolean_operator graph VSet.inter DD.meet
  let join graph = binary_boolean_operator graph VSet.union DD.join
  let complement graph = difference graph (full graph)
  
  let add_zeroes_vertex graph v arrows = 
    let add_zero a arrows =
      AMap.add a (DD.add_zero (get_dd a arrows)) arrows in
    G.fold_out v add_zero graph arrows  

  let add_zeroes graph v_set a_map =
    VSet.fold (add_zeroes_vertex graph) v_set a_map

  let remove_zeroes_vertex graph v arrows = 
    let remove_zero a arrows =
      let dd = DD.remove_zero (get_dd a arrows) in
      if DD.is_empty dd
      then AMap.remove a arrows
      else AMap.add a dd arrows in
    G.fold_out v remove_zero graph arrows  

  let remove_zeroes graph v_set a_map =
    VSet.fold (remove_zeroes_vertex graph) v_set a_map

  let zero_normalize_vertex graph v {vertices;arrows} =
    let arrows = 
      if VSet.mem v vertices
      then add_zeroes_vertex graph v arrows
      else remove_zeroes_vertex graph v arrows in
    {vertices;arrows}

  let zero_normalize graph ({vertices;arrows} as x) = 
    G.fold_vertex (fun v a -> zero_normalize_vertex graph v a) 
    graph x

  let future_extension graph r1 r2 =
    let arrows_1 = ref r1.arrows in
    let arrows_2 = ref r2.arrows in
    let vertices = ref VSet.empty in
    let arrows = ref AMap.empty in
    let current = ref (full graph).vertices in
    let next = ref VSet.empty in
    let future_extension a =
      let dd1 = get_dd a !arrows_1 in
      let dd2 = get_dd a !arrows_2 in
      let dd3 = DD.future_extension dd1 dd2 in
      let tgt_a = G.tgt a graph in
      let in_r1 = VSet.mem tgt_a r1.vertices in
      let in_r2 = VSet.mem tgt_a r2.vertices in
      let dd3_is_unbounded = not (DD.is_bounded dd3) in
      let tgt_to_be_added = (not (VSet.mem tgt_a !vertices))
        && (in_r1 || (in_r2 && dd3_is_unbounded)) in 
      let () =
        if tgt_to_be_added
        then (
          (if not in_r1 then arrows_1 := add_zeroes_vertex graph tgt_a !arrows_1);
          (if not in_r2 then arrows_2 := add_zeroes_vertex graph tgt_a !arrows_2);
          vertices := VSet.add tgt_a !vertices;
          next := VSet.add tgt_a !next) in 
      arrows := 
        if DD.is_empty dd3 
        then AMap.remove a !arrows 
        else AMap.add a dd3 !arrows in
    let future_cone v = G.iter_out v future_extension graph in
    let () = 
      while not (VSet.is_empty !current) do
        VSet.iter future_cone !current;
        current := !next;
        next := VSet.empty;
      done in
    let vertices = !vertices in
    let arrows = !arrows in
    { vertices ; arrows }

  (* To be tested *)
  (* BUG: the case where the target belongs to the model and the arrow carries 
  an unbounded region is not taken into account. *)
  (* TODO: check that it has been done *)

  let past_extension graph r1 r2 =
    let arrows_1 = ref r1.arrows in
    let arrows_2 = ref r2.arrows in
    let vertices = ref VSet.empty in
    let arrows = ref AMap.empty in
    let current = ref (full graph).vertices in
    let next = ref VSet.empty in
    let past_extension add_unbounded_component a =
      let dd1 = get_dd a !arrows_1 in
      let dd2 = get_dd a !arrows_2 in
      let dd1 = 
        if add_unbounded_component
        then (
          let lcc_dd2 = 
            try DD.(of_interval (last_connected_component dd2)) 
            with DD.Undefined -> DD.empty in
          if not (DD.is_bounded lcc_dd2) 
          then DD.join dd1 lcc_dd2
          else dd1)
        else dd1 in
      let dd3 = DD.past_extension dd1 dd2 in
      let src_a = G.src a graph in
      let in_r1 = VSet.mem src_a r1.vertices in
      let in_r2 = VSet.mem src_a r2.vertices in
      let dd3_contains_zero = DD.contains_zero dd3 in
      let src_to_be_added = (not (VSet.mem src_a !vertices))
            && (in_r1 || (in_r2 && dd3_contains_zero)) in 
      let () =
        if src_to_be_added
        then (
          (if not in_r1 then arrows_1 := add_zeroes_vertex graph src_a !arrows_1);
          (if not in_r2 then arrows_2 := add_zeroes_vertex graph src_a !arrows_2);
          vertices := VSet.add src_a !vertices;
          next := VSet.add src_a !next) in 
      arrows := 
        if DD.is_empty dd3 
        then AMap.remove a !arrows 
        else AMap.add a dd3 !arrows in
    let past_cone v = G.iter_in v 
      (past_extension VSet.(mem v !vertices || mem v r1.vertices)) graph in
    let () =
      while not (VSet.is_empty !current) do
        VSet.iter past_cone !current;
        current := !next;
        next := VSet.empty;
      done in
    let vertices = !vertices in
    let arrows = !arrows in
    { vertices ; arrows }

(* TODO: test closure and interior *)

let vertex_belongs_to_interior g v arrows =
  (try 
    G.iter_in v
      (fun a ->
        if DD.is_bounded (get_dd a arrows) 
        then raise Exit) g;
    true
  with Exit -> false)
  &&
  (try
    G.iter_out v 
      (fun a -> 
        if not (DD.contains_zero (get_dd a arrows))
        then raise Exit) g;
    true
  with Exit -> false)

let interior g r =
  let arrows = ref (AMap.map DD.interior r.arrows) in
  let f = fun v accu ->
    if vertex_belongs_to_interior g v !arrows 
    then VSet.add v accu 
    else (arrows := remove_zeroes_vertex g v !arrows; accu) in 
  let vertices = VSet.fold f r.vertices VSet.empty in
  let arrows = !arrows in
  { vertices ; arrows }

let vertex_belongs_to_closure g v arrows =
  (try 
    G.iter_in v
      (fun a ->
        if not (DD.is_bounded (get_dd a arrows)) 
        then raise Exit) g;
    false
  with Exit -> true)
  || 
  (try
    G.iter_out v 
      (fun a ->
        if DD.(contains_zero (get_dd a arrows))
        then raise Exit) g;
    true
  with Exit -> true)

let closure g r =
  let arrows = ref (AMap.map DD.closure r.arrows) in
  let f = fun v accu ->
    if vertex_belongs_to_closure g v !arrows
    then (arrows := add_zeroes_vertex g v !arrows; VSet.add v accu) 
    else accu in
  let vertices = G.fold_vertex f g VSet.empty in
  let arrows = !arrows in
  { vertices ; arrows }

let future_closure g r = future_extension g r (closure g r)
let past_closure g r   = past_extension   g r (closure g r)

end (* Raw *)

module Make(G:Graph)(DD:HalfLineRegion.S):S with type arrow = G.arrow and type vertex = G.vertex
  = Raw(G:Graph)(DD:HalfLineRegion.S)
