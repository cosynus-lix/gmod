module type Requirements =
sig
  module IntSet:Set.S with type elt = int
  module Array: sig val for_all: ('a -> bool) -> ('a array) -> bool end
  module List: sig val initial_segment: ?start:int -> int -> int list end
end with type IntSet.elt = int

module type S =
sig
  type brick
  type area
  type t
  val yccc                         : brick -> t
  (** Partition of the complement of a cube following Yoneda components *)
  val connected                    : area -> t
  (** Partition following connected components *)
  val lattice                      : area -> t
  (** Covering following maximal sub-lattices *)
  val yoneda                       : area -> t
  (** Partition following Yoneda components *)
  val ginzu                        : area -> t
  (** The coarsest parition whose elements are cubes *)
  val mutex_program_model_yoneda   : (area ref) -> unit
  (** partition of a mutex program model following Yoneda components *)
  val choose                       : t -> area
  val add                          :  area -> t -> t
  val fold                         : (area -> 'a -> 'a) -> t -> 'a -> 'a
  val iter                         : (area -> unit) -> t -> unit
  val cardinal                     : t -> int (* Renvoie le nombre d'élément de la partition. *)
  val remove                       : area -> t -> t
  val empty                        : t
  val dimension                    : t -> int (* renvoie la dimension de l'espace ambiant. *)
  val string_of                    : t -> string (* pour affichage *)
  val string_list_of               : t -> string list (* pour affichage *)
  val find                         : (area -> bool) -> t -> area
  val print                        : t -> unit
  val clean                        : t -> t
  val dihomotopy_classes           : area -> t
  (* val dihomotopy_classes_version_3 : area -> t (*brick list list*) *)
  val exact_dihomotopy_classes     : ?a:area -> t -> t
  val dihomotopy_classes_based_on_ginzu : ?norm:unit -> area -> t
  val print_dihomotopy_classes     : area -> unit
  val dihomotopy_classes_iter_path : (brick list -> unit) -> area -> unit
  val dihomotopy_classes_iter_wave : (area       -> unit) -> area -> unit
    val dihomotopy_classes_fold_wave : (area -> 'a -> 'a  ) -> area -> 'a -> 'a
end


module Make(Overall:Requirements):
sig

module OverCube
(C:Cube.S with type ints = Overall.IntSet.t)
(A:AreaOverCube.S 
  with type regular_value = C.regular_value
  and type point = C.point
  and type generator = C.interval
  and type brick = C.t
  and type ints = Overall.IntSet.t): (S with type area = A.t)

  module OverTorus(T:Torus.S):sig end

  module OverCylinder(X:Cylinder.S):sig end

end

