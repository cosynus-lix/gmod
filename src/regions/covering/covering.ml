module type S = sig
  type graph
  type region_on_graph
  type t
  val empty: t
  val full: graph array -> t
  val of_array: region_on_graph array -> t 
  val join: t -> t -> t
  val meet: graph array -> t -> t -> t
  val complement: graph array -> t -> t
  val interior: graph array -> t -> t
  val closure: graph array -> t -> t
  val future_extension: graph array -> t -> t -> t
  val past_extension: graph array -> t -> t -> t
end (* S *)

module Raw(R:OnGraph.Region) = struct

  module B = Block.Make(R:OnGraph.Region)
  module C = Set.Make(B)

  type graph = R.graph

  type region_on_graph = R.t

  type t = C.t
  
  let empty = C.empty
  
  let full ga = C.singleton (B.full ga) 
  
  let of_array ar = C.singleton ar
  
  let join c1 c2 = C.union c1 c2
  
  let meet ga c1 c2 =
    let meet b1 b2 a = 
      try C.add (B.meet ga b1 b2) a 
      with B.Undefined -> a in
    let meet b1 a = C.fold (meet b1) c2 a in
    C.fold meet c1 C.empty

  let complement ga b =
    let d = Array.length ga in
    let result = ref C.empty in
    let () = 
      for i = 0 to pred d do
        try result := C.add (B.complement i ga b) !result 
        with B.Undefined -> ()
      done in
    !result

  let complement ga c =
    let complement b a = meet ga (complement ga b) a in
    C.fold complement c (full ga)

  let unary op ga c =
    let op b a = C.add (op ga b) a in
    C.fold op c C.empty

  (* Warning: the function interior requires that each point covered by 
  c be contained in the interior of some block of c. It suffices that c 
  be a normal form, but it is not necessary. *)
  
  let interior = unary B.interior
  let closure  = unary B.closure
  
  let future_extension ga c1 c2 = failwith "Covering.future_extension NIY"
  let past_extension ga c1 c2 = failwith "Covering.past_extension NIY"
  
end (* Raw *)

module Make(R:OnGraph.Region): S 
  with type graph = R.graph and type region_on_graph = R.t
  = Raw(R)
