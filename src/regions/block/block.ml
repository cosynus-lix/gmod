module type S = sig
  exception Undefined
  type graph
  type region
  type t
end (* S *)

module Raw (G:Graph.Graph)(R:Graph.Region) = struct

  exception Undefined

  type graph = G.t
  type region = R.t
  type t = region array
  
  let nonempty r = if R.is_empty r then raise Undefined else r
  
  let meet ga b1 b2 =
    let meet i g = nonempty (R.meet g b1.(i) b2.(i)) in
    Array.mapi meet ga
  
  let interior ga b =
    let interior g r = nonempty (R.interior g r) in 
    Array.map2 interior ga b

  let closure ga b = Array.map2 R.closure ga b
  let future_closure ga b = Array.map2 R.future_closure ga b
  let past_closure ga b = Array.map2 R.past_closure ga b
  
end (* Raw *)

module Make (G:Graph.Graph)(R:Graph.Region): S with 
  type graph = G.t and type region = R.t 
  = Raw(G:Graph.Graph)(R:Graph.Region)
