module type Graph = sig
  exception Undefined
  type vertex
  type arrow
  type t
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

  type t = { graph:graph ; vertices:VSet.t ; arrows: DD.t AMap.t }
  
  let empty graph = { graph ; vertices = VSet.empty ; arrows = AMap.empty }

  let full graph = 
    let update v (vertices,arrows) = 
      let vertices = VSet.add v vertices in
      let arrows = G.fold_out v (fun a arrows -> AMap.add a DD.full arrows) graph arrows in
      (vertices,arrows) in
    let empty = (VSet.empty,AMap.empty) in
    let (vertices,arrows) = 
      G.fold_vertex update graph empty in
    { graph ; vertices ; arrows }  

  let add_vertex v {graph;vertices;arrows} =
    let vertices = VSet.add v vertices in
    {graph;vertices;arrows}

  let add_arrow a dd {graph;vertices;arrows} =
    let arrows = AMap.add a dd arrows in
    {graph;vertices;arrows}

  let get_dd a arrows = 
    try AMap.find a arrows 
    with Not_found -> DD.empty
  
  let binary_boolean_operator v_op dd_op =
    fun r1 r2 -> 
      let graph = r1.graph in
      let vertices = v_op r1.vertices r2.vertices in
      let add v arrows =
        let add a arrows =
          let dd1 = get_dd a r1.arrows in
          let dd2 = get_dd a r2.arrows in
          let result = dd_op dd1 dd2 in 
          AMap.add a result arrows in 
        G.fold_out v add graph arrows in
      let arrows = G.fold_vertex add graph AMap.empty in
      { graph ; vertices ; arrows}

  let difference r1 r2 = binary_boolean_operator VSet.diff DD.difference
  let meet r1 r2 = binary_boolean_operator VSet.union DD.join
  let join r1 r2 = binary_boolean_operator VSet.inter DD.meet
  let complement r = difference (full r.graph) r

  let add_zeroes_vertex graph v arrows = 
    let add_zero a arrows = AMap.add a (DD.add_zero (get_dd a arrows)) arrows in
    G.fold_out v add_zero graph arrows  
  
  let add_zeroes graph v_set a_map =
    VSet.fold (add_zeroes_vertex graph) v_set a_map
  
  let future_extension r1 r2 =
    let graph = r1.graph in
    let arrows_1 = ref (add_zeroes graph r1.vertices r1.arrows) in
    let arrows_2 = ref (add_zeroes graph r2.vertices r2.arrows) in
    let vertices = ref VSet.empty in
    let arrows = ref AMap.empty in
    let current = ref (full graph).vertices in
    let next = ref VSet.empty in
    let future_extension a =
      let dd1 = get_dd a r1.arrows in
      let dd2 = get_dd a r2.arrows in
      let dd3 = DD.HalfLine.future_extension dd1 dd2 in
      let w = G.tgt a graph in
      let in_r1 = VSet.mem w r1.vertices in
      let in_r2 = VSet.mem w r2.vertices in
      let dd3_is_unbounded = not (DD.HalfLine.is_bounded dd3) in
      let () =
        if (not (VSet.mem w !vertices))
            && (in_r1 || (in_r2 && dd3_is_unbounded))
        then (
          if not in_r1 then arrows_1 := add_zeroes_vertex graph w !arrows_1; (* ajouter les zéros dans les flèches qui sortent de w dans r1 *)
          if not in_r2 then arrows_2 := add_zeroes_vertex graph w !arrows_2; (* idem pour r2 *)
          vertices := VSet.add w !vertices;
          next := VSet.add w !next) in 
      arrows := AMap.add a dd3 !arrows in
    let future_cone v = G.iter_out v future_extension graph in
    let () = 
      while not (VSet.is_empty !current) do
        VSet.iter future_cone !current;
        current := !next;
        next := VSet.empty
      done in
    let vertices = !vertices in
    let arrows = !arrows in
    { graph ; vertices ; arrows}

  let past_extension r1 r2 = failwith "NIY"
  
end (* Raw *)

module Make(G:Graph)(DD:DashDot.S):S with type arrow = G.arrow and type vertex = G.vertex
  = Raw(G:Graph)(DD:DashDot.S) 
