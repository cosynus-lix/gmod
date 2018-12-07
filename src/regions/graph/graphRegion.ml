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

module Raw(G:Graph)(DD:DashDot.S) = struct

  type arrow = G.arrow
  type vertex = G.vertex
  type graph = G.t
  
  module VSet = Set.Make(struct type t = vertex let compare = G.compare_vertex end)
  module AMap = Map.Make(struct type t = arrow let compare = G.compare_arrow end)

  type t = { vertices:VSet.t ; arrows: DD.t AMap.t }
  
  let empty = { vertices = VSet.empty ; arrows = AMap.empty }

  let full graph = 
    let update v (vertices,arrows) = 
      let vertices = VSet.add v vertices in
      let arrows = G.fold_out v (fun a arrows -> AMap.add a DD.full arrows) graph arrows in
      (vertices,arrows) in
    let empty = (VSet.empty,AMap.empty) in
    let (vertices,arrows) = 
      G.fold_vertex update graph empty in
    { vertices ; arrows }  

  let add_vertex v {vertices;arrows} =
    let vertices = VSet.add v vertices in
    { vertices ; arrows }

  let add_arrow a dd {vertices;arrows} =
    let arrows = AMap.add a dd arrows in
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
          AMap.add a result arrows in 
        G.fold_out v add graph arrows in
      let arrows = G.fold_vertex add graph AMap.empty in
      { vertices ; arrows }

  let difference graph = binary_boolean_operator graph VSet.diff DD.difference
  let meet graph = binary_boolean_operator graph VSet.inter DD.meet
  let join graph = binary_boolean_operator graph VSet.union DD.join
  let complement graph = difference graph (full graph)

  let add_zeroes_vertex graph v arrows = 
    let add_zero a arrows = AMap.add a (DD.add_zero (get_dd a arrows)) arrows in
    G.fold_out v add_zero graph arrows  
  
  let add_zeroes graph v_set a_map =
    VSet.fold (add_zeroes_vertex graph) v_set a_map
  
  let debug = false
  
  let print_arrows arrows = if debug then
    AMap.iter (fun a dd -> 
      Printf.printf "%s : %s\n"
        (G.string_of_arrow a)
        (DD.HalfLine.string_of dd)
    ) arrows
  
  let print_vertices vertices =
    if debug then VSet.iter (fun v -> print_string ((G.string_of_vertex v)^" ")) vertices
  
  let future_extension graph r1 r2 =
    let () = if debug then print_endline "r1.arrows"; print_arrows r1.arrows in
    let arrows_1 = ref (add_zeroes graph r1.vertices r1.arrows) in
    let () = if debug then print_endline "arrows_1"; print_arrows !arrows_1 in
    let () = if debug then print_endline "r2.arrows"; print_arrows r2.arrows in
    let arrows_2 = ref (add_zeroes graph r2.vertices r2.arrows) in
    let () = if debug then print_endline "arrows_2"; print_arrows !arrows_2 in
    let vertices = ref VSet.empty in
    let () = if debug then (print_string "vertices = { "; print_vertices !vertices; print_endline "}") in
    let arrows = ref AMap.empty in
    let () = if debug then print_endline "arrows"; print_arrows !arrows in
    let current = ref (full graph).vertices in
    let () = if debug then (print_string "current = { "; print_vertices !current; print_endline "}") in
    let next = ref VSet.empty in
    let () = if debug then (print_string "next = { "; print_vertices !next; print_endline "}") in
    let future_extension a =
      let () = if debug then Printf.printf "FUTURE EXTENSION %s\n" (G.string_of_arrow a) in
      let dd1 = get_dd a !arrows_1 in
      let () = if debug then Printf.printf "dd1 = %s\n" (DD.HalfLine.string_of dd1) in
      let dd2 = get_dd a !arrows_2 in
      let () = if debug then Printf.printf "dd2 = %s\n" (DD.HalfLine.string_of dd2) in
      let dd3 = DD.HalfLine.future_extension dd1 dd2 in
      let () = if debug then Printf.printf "dd3 = %s\n" (DD.HalfLine.string_of dd3) in
      let w = G.tgt a graph in
      let in_r1 = VSet.mem w r1.vertices in (*false*) 
      let () = if debug then Printf.printf "  in_r1 = %b\n" in_r1 in
      let in_r2 = VSet.mem w r2.vertices in (*true*) 
      let () = if debug then Printf.printf "  in_r2 = %b\n" in_r2 in
      let dd3_is_unbounded = not (DD.HalfLine.is_bounded dd3) in (*true*) 
      let () = if debug then  Printf.printf "  dd3_is_unbounded = %b\n" dd3_is_unbounded in
      let condition = (not (VSet.mem w !vertices))
            && (in_r1 || (in_r2 && dd3_is_unbounded)) in 
      let () = if debug then Printf.printf "  condition = %b\n" condition in
      let () =
        if condition
        then (
          (if not in_r1 then arrows_1 := add_zeroes_vertex graph w !arrows_1); (* ajouter les zéros dans les flèches qui sortent de w dans r1 *)
          let () = if debug then print_endline "arrows_1"; print_arrows !arrows_1 in
          (if not in_r2 then arrows_2 := add_zeroes_vertex graph w !arrows_2); (* idem pour r2 *)
          let () = if debug then print_endline "arrows_2"; print_arrows !arrows_2 in
          vertices := VSet.add w !vertices;
          let () = if debug then (print_string "  vertices = { "; print_vertices !vertices; print_endline "}") in
          next := VSet.add w !next;
          if debug then (print_string "  next = { "; print_vertices !next; print_endline "}")) in 
      arrows := AMap.add a dd3 !arrows; 
      if debug then (print_endline "  arrows"; print_arrows !arrows; print_endline "––") in
    let future_cone v = G.iter_out v future_extension graph in
    let () = 
      while not (VSet.is_empty !current) do
        VSet.iter future_cone !current;
        current := !next;
        next := VSet.empty;
        if debug then print_string "vertices = { ";
        if debug then print_vertices !vertices;
        if debug then print_endline "}"
      done in
    let vertices = !vertices in
    let arrows = !arrows in
    { vertices ; arrows }

  let past_extension graph r1 r2 = failwith "NIY"
  
end (* Raw *)

module Make(G:Graph)(DD:DashDot.S):S with type arrow = G.arrow and type vertex = G.vertex
  = Raw(G:Graph)(DD:DashDot.S) 
