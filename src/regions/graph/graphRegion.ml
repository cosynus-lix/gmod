module type OrderedType = sig
  type t
  val compare: t -> t -> int
end


module type S = sig
  type vertex
  type arrow
  type graph
  type covering
  
  val graph_to_covering: graph -> covering
  
end (* S *)

module Raw(A:OrderedType)(V:OrderedType) = struct

  type arrow = A.t
  type vertex = V.t

  module VA = struct
  
    type t = vertex * arrow
    
    let compare (v1,a1) (v2,a2) =
      let delta = V.compare v1 v2 in
      if delta <> 0 then delta
        else A.compare a1 a2
  
  end (* VA *)

  module Star = Set.Make(VA)

  type graph = Set.Make(V).t * ((vertex * vertex) Map.Make(A).t)
  
  type neighbor = Star.t * Star.t
  
  type covering = neighbor Map.Make(V).t

  let graph_to_covering = failwith "NIY"

end (* Raw *)

module Make(A:OrderedType)(V:OrderedType):S with type arrow = A.t and type vertex = V.t
  = Raw(A:OrderedType)(V:OrderedType) 
