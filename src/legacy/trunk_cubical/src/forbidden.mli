module type S =
sig
  type area
  val recursive_mutex        : ?runpro:Semantics.process array -> Semantics.t -> area
  val normal_mutex           : ?runpro:Semantics.process array -> Semantics.t -> area
  val switch_mutex           : ?runpro:Semantics.process array -> Semantics.t -> area
  val switch_semaphore       : ?runpro:Semantics.process array -> Semantics.t -> area
  val quantitative_semaphore : ?runpro:Semantics.process array -> Semantics.t -> area
  val synchronization        : ?runpro:Semantics.process array -> Semantics.t -> area
  val monitor                : ?runpro:Semantics.process array -> Semantics.t -> area
  val pile_starvation        : ?runpro:Semantics.process array -> Semantics.t -> area
  val pile_overflow          : ?runpro:Semantics.process array -> Semantics.t -> area
  val file_starvation        : ?runpro:Semantics.process array -> Semantics.t -> area
  val file_overflow          : ?runpro:Semantics.process array -> Semantics.t -> area
  val conflict               : ?runpro:Semantics.process array -> ?out:bool -> ?forbidden:area -> Semantics.t -> (area option * string list)
end

module Make
  (I:sig include Interval.S
    val start:regular_value
    val step: regular_value -> regular_value
    val regular_value_to_string: regular_value -> string end)
  (C:Cube.S with type regular_value = I.regular_value and type interval = I.t)
  (AC:AreaOverCube.S with type regular_value = I.regular_value and type brick = C.t)
  (BuzySection:Buzy_section.S with type regular_value = I.regular_value and type area = AC.t)
  (Buzy_matrix:Buzy_matrix.S with type regular_value = I.regular_value and type area = AC.t) 
: (S with type area = AC.t)
