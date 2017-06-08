open Common

(** Operations on simplicial sets. *)
module Simplicial :
  sig
    (** Simplices. *)
    module rec S :
    sig
      (** A simplex. *)
      type t

      (** Comparison function on simplicial sets. *)
      val compare : t -> t -> int

      (** Construct a simplex of given dimension and faces. *)
      val construct : int -> t array -> t
      val set_faces : t -> t array -> unit
      val get_faces : t -> t array
      val get_dim : t -> int

      (** Get the set of simplicies below a simplex. *)
      val get_below : t -> SS.t
      val set_below : t -> SS.t -> unit
    end
    (** A set of simplicies. *)
    and SS : Set.S with type elt = S.t

    (** A simplicial set: i-th element is the set of i-dimensional simplices. *)
    type t = SS.t array

    (** Construct the standard simploidal set of given dimension. *)
    val standard : int -> S.t * t

    (** [d i s] gives the [i]-th face of a simplex [s]. *)
    val d : int -> S.t -> S.t

    (** Get an arbitrary simplex of given dimension. *)
    val choose : t -> int -> S.t

    val union : t -> t -> t

    (** Number of connected components. *)
    val components : t -> int
  end

(** Operations on simploidal sets. *)
module Simploidal :
sig
  (** Operations on simploids. *)
  module rec S :
  sig
    type t
    val compare : t -> t -> int
    val get_dim : t -> int
    val construct : int array -> t array array -> t
    val get_type : t -> int array
    val get_faces : t -> t array array
    val set_faces : t -> t array array -> unit
    val get_faces_as_enum : t -> t Enum.t
    val is_empty : t -> bool
    val set_below : t -> SS.t -> unit
    val get_below : t -> SS.t
  end

  and SS : Set.S with type elt = S.t

  (** A simploidal set. *)
  type t = SS.t

  val get_count : t -> int list
  val refresh_below : t -> unit
  val product : t -> t -> t

  (** [d i j s] gives the [j]-th face in direction [i] of a simplex [s]. *)
  val d : int -> int -> S.t -> S.t

  val maximal : t -> t
  val of_simplicial : Simplicial.SS.t array -> SS.t
  val union : t -> t -> t
  val euler : t -> int
  (*
    val copy :
    SS.t ->
    SM.key Batteries.Array.mappable -> SS.t * S.t Batteries.Array.mappable *)
  val components : t -> int
  val type_leq : 'a array -> 'a array -> bool
(* val compose_map : SM.key SM.t -> SM.key SM.t -> SM.key SM.t *)
(*
  val identify :
  SS.t -> SM.key -> SM.key -> SS.elt SM.t option -> SS.t * SM.key SM.t
*)
end
