(** Implementation of Martin Raussen's algorithm. *)

val analyze : ?components:bool -> ?euler:bool -> ?covering:bool-> Semantics.t -> Type.Co.t

(*
val sublists : 'a list -> 'a list list

val list_product : 'a list list -> 'a list list

val list_exponent : 'a list -> int -> 'a list list

val extend_brick : Type.C.t -> int -> Type.C.t

val extend_cube_list_in_multiple_dimensions :
  Type.C.t list -> IntPairSet.t -> Type.C.t list

val isDead : Type.AC.brick list -> IntPairSet.t -> bool

val area_to_list : Type.AC.t -> Type.AC.brick list

val copy_matrix : 'a array array -> 'a array array

val boolMatrixSet_concat : BoolMatrixSet.t Batteries.Enum.t -> BoolMatrixSet.t

val intPairSetSet_concat : IntPairSetSet.t Batteries.Enum.t -> IntPairSetSet.t


val matrix_means_empty : IntPairSet.t -> int -> bool

val ips_components : IntPairSetSet.t -> int -> IntPairSetSet.t

val ipss_print : 'a BatInnerIO.output -> IntPairSetSet.t -> unit

val boundary_coordinates : Type.C.t list -> IntPairSet.t

val analyze1_part1_version1 : Type.AC.brick list -> IntPairSet.t -> int -> int -> IntPairSetSet.t

val analyze1_part1_version2 : Type.C.t list -> IntPairSet.t -> int -> int -> IntPairSetSet.t

val analyze1_part2_version1 : IntPairSetSet.t -> 'a -> 'b -> 'c -> IntPairSetSet.t

val analyze1_part2_version2 : IntPairSetSet.t -> IntPairSet.t -> int -> int -> IntPairSetSet.t

val analyze_part3 : IntPairSetSet.t -> int -> int -> Simploid.Simploidal.t

val euler : IntPairSetSet.t -> int -> int

val time : (unit -> 'a) -> 'a * float

val analyze1 : Semantics.t -> Type.Co.t
*)
