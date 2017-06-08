open Common

module Make
  (V:sig
    include Set.OrderedType
    val to_string : t -> string
  end):
sig
  (** Edge of an hypergraph. *)
  type edge

  module E : Set.S with type elt = V.t and type t = edge

  (** Collection of edges, which forms a hypergraph *)
  type t

  module ES : Set.S with type elt = E.t and type t = t

  (** Vertex of an hypergraph. *)
  type vertex

  val empty : t

  (** Add an edge to a hypergraph. *)
  val add_edge : edge -> t -> t

  (** Union of two hypergraphs (this is the union of the edge sets.) *)
  val union : t -> t -> t

  (** "join" of two hypergraphs (this is the union of pairs of edges, one from each argument). *)
  val join : t -> t -> t

  (** Set of edges of a graph such that no other edge is a (strict) subset of this edge. *)
  val minimal : t -> t

  (*
  (** Set of edges of a graph such that no other edge is a superset of this edge. *)
  val maximal : t -> t
  *)

  (** Checks if an edge is transversal to a hypergraph. *)
  val is_transversal : t -> edge -> bool

  (** Compute the transversal hypergraph of a hypergraph. *)
  val transversal : t -> t

  (** String representation of an hypergraph. *)
  val to_string : t -> string
end
  with type vertex = V.t
