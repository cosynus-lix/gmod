module type Graph = sig
  type vertex
  type arrow
  type t
  val compare_vertex: vertex -> vertex -> int
  val compare_arrow: arrow -> arrow -> int
  val fold_vertex: (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_out: vertex -> (arrow -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_in: vertex -> (arrow -> 'a -> 'a) -> t -> 'a -> 'a
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
  
  let add_zeroes v_set r =
    let add_zero a arrows = AMap.add a (DD.add_zero (get_dd a r)) arrows in
    let add_zeroes v arrows = G.fold_out v add_zero graph arrows in
    let arrows = VSet.fold add_zeroes  r.arrows in
    { r.graph ; r. vertices ; arrows}
  
  let future_extension r1 r2 = failwith "NIY"
  
  
  
  let past_extension r1 r2 = failwith "NIY"
  
end (* Raw *)

module Make(G:Graph)(DD:DashDot.S):S with type arrow = G.arrow and type vertex = G.vertex
  = Raw(G:Graph)(DD:DashDot.S) 
