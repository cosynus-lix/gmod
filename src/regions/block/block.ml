module Make(G:Graph.S)(R:OnGraph.Region) = struct

  exception Undefined

  type t = R.t array
  
  let compare = compare
  
  let nonempty r = if R.is_empty r then raise Undefined else r
  
  let full ga = 
    let d = Array.length ga in
    let full i = R.full ga.(i) in
    Array.init d full
  
  let meet ga b1 b2 =
    let meet i g = nonempty (R.meet g b1.(i) b2.(i)) in
    Array.mapi meet ga
  
  let interior ga b =
    let interior g r = nonempty (R.interior g r) in 
    Array.map2 interior ga b

  let closure ga b = Array.map2 R.closure ga b
  let future_closure ga b = Array.map2 R.future_closure ga b
  let past_closure ga b = Array.map2 R.past_closure ga b
  
  let complement i ga b =
    let d = Array.length ga in
    let complement j = 
      if i = j
      then nonempty (R.complement ga.(i) b.(i))
      else R.full ga.(i) in 
    Array.init d complement
  
end (* Make *)
