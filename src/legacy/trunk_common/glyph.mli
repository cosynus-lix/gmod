
val enabled: bool ref

val init: bool -> unit
val enable: unit -> unit
val disable: unit -> unit

val negation: unit -> string
val empty_set: unit -> string
val delta: unit -> string
val nabla: unit -> string
val subset_or_equal: unit -> string
val supset_or_equal: unit -> string
val strict_subset: unit -> string
val strict_supset: unit -> string
val subset: unit -> string
val supset: unit -> string
val belongs_to: unit -> string
val big_belongs_to: unit -> string
val infinity: unit -> string
val direct_sum: unit -> string
val tensor_product: unit -> string
val big_direct_sum: unit -> string
val big_tensor_product: unit -> string
val cartesian_product: unit -> string
val big_sigma: unit -> string
val cap: unit -> string
val cup: unit -> string
val big_cap: unit -> string
val big_cup: unit -> string
val wedge: unit -> string
val vee: unit -> string
val big_wedge: unit -> string
val big_vee: unit -> string
val precedes: unit -> string
val succeeds: unit -> string
val precedes_or_equal: unit -> string
val succeeds_or_equal: unit -> string
val less_than_or_slanted_equal_to: unit -> string
val greater_than_or_slanted_equal_to: unit -> string
val forces: unit -> string
val way_below: unit -> string
val way_above: unit -> string
val for_all: unit -> string
val exists: unit -> string
val partial_differencial: unit -> string
val big_square_cap: unit -> string
val big_square_cup: unit -> string
val square_cap: unit -> string
val square_cup: unit -> string
val square_subset: unit -> string
val square_supset: unit -> string
val square_subset_or_equal: unit -> string
val square_supset_or_equal: unit -> string
val normal_subgroup: unit -> string
val normal_supgroup: unit -> string
val bottom: unit -> string
val top: unit -> string
val bowtie: unit -> string
val end_of_proof: unit -> string
val assertion: unit -> string
val models: unit -> string
val true_: unit -> string
val forces: unit -> string
val hermitian_conjugate_matrix: unit -> string
val multimap: unit -> string
val left_normal_factor_semidirect_product: unit -> string
val right_normal_factor_semidirect_product: unit -> string


module Triangle :
sig
  val up: unit -> string
  val down: unit -> string
  val left: unit -> string
  val right: unit -> string
  val big_up: unit -> string
  val big_down: unit -> string
  val big_left: unit -> string
  val big_right: unit -> string
end

module Greek :
sig
  val alpha: unit -> string
  val beta: unit -> string
  val gamma: unit -> string
  val delta: unit -> string
  val epsilon: unit -> string
  val zeta: unit -> string
  val eta: unit -> string
  val theta: unit -> string
  val iota: unit -> string
  val kappa: unit -> string
  val lambda: unit -> string
  val mu: unit -> string
  val nu: unit -> string
  val xi: unit -> string
  val omicron: unit -> string
  val pi: unit -> string
  val rho: unit -> string
  val final_sigma: unit -> string
  val sigma: unit -> string
  val tau: unit -> string
  val upsilon: unit -> string
  val phi: unit -> string
  val chi: unit -> string
  val psi: unit -> string
  val omega: unit -> string
end
