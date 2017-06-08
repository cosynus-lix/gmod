module type S =
sig
  type area
  val recursive_mutex        : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val normal_mutex           : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val switch_mutex           : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val switch_semaphore       : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val quantitative_semaphore : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val synchronization        : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val monitor                : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val pile_starvation        : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val pile_overflow          : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val file_starvation        : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val file_overflow          : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val conflict               : ?runpro:AbstractSyntax.process array -> ?out:bool -> ?forbidden:area -> AbstractSyntax.t -> (area option * string list)
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
