module type S = sig
  type graph
  type block
  type t
end (* S *)

module Raw(G:GraphRegion.Graph)(B:Block) = struct
end (* Raw *)

module Make(G:GraphRegion.Graph)(B:Block): S with 
  type graph = G.t and type block = Block.t
  = Raw(G:GraphRegion.Graph)(B:Block)
