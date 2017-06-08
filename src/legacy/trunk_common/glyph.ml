(*Glyphs used on displaying devices*)

let enabled = ref true

let init b = enabled := b

let enable () = enabled := true
let disable () = enabled := false

let negation () = if !enabled then "¬" else "#void"
let empty_set () = if !enabled then "∅" else "#void"
let delta () = if !enabled then "∆" else "#Laplace"
let nabla () = if !enabled then "∇" else "#nabla"
let subset_or_equal () = if !enabled then "⊆" else "subset of or equal to"
let supset_or_equal () = if !enabled then "⊇" else "supset of or equal to"
let strict_subset () = if !enabled then "⊊" else "belongs to"
let strict_supset () = if !enabled then "⊋" else "belongs to"
let subset () = if !enabled then "⊂" else "belongs to"
let supset () = if !enabled then "⊃" else "belongs to"
let belongs_to () = if !enabled then "∊" else "belongs to"
let big_belongs_to () = if !enabled then "∈" else "belongs to"
let infinity () = if !enabled then "∞" else "oo"
let direct_sum () = if !enabled then "⊕" else "(+)"
let tensor_product () = if !enabled then "⊗" else "(x)"
let big_direct_sum () = if !enabled then "⨁" else "(+)"
let big_tensor_product () = if !enabled then "⨂" else "(x)"
let cartesian_product () = if !enabled then "⨯" else "x"
let big_sigma () = if !enabled then "∑" else "sum"
let cap () = if !enabled then "∩" else "intersection"
let cup () = if !enabled then "∪" else "union"
let big_cap () = if !enabled then "⋂" else "intersection"
let big_cup () = if !enabled then "⋃" else "union"
let wedge () = if !enabled then "∧" else "&"
let vee () = if !enabled then "∨" else "|"
let big_wedge () = if !enabled then "⋀" else "&"
let big_vee () = if !enabled then "⋁" else "|"
let precedes () = if !enabled then "≺" else "<"
let succeeds () = if !enabled then "≻" else ">"
let precedes_or_equal () = if !enabled then "≼" else "≤"
let succeeds_or_equal () = if !enabled then "≽" else "≥"
let less_than_or_slanted_equal_to () = if !enabled then "⩽" else "<="
let greater_than_or_slanted_equal_to () = if !enabled then "⩾" else ">="
let forces () = if !enabled then "⊩" else ""
let way_below () = if !enabled then "≪" else "<<"
let way_above () = if !enabled then "≫" else ">>"
let for_all () = if !enabled then "∀" else "for all"
let exists () = if !enabled then "∃" else "there exists"
let partial_differencial () = if !enabled then "∂" else ""
let big_square_cap () = if !enabled then "⨅" else ""
let big_square_cup () = if !enabled then "⨆" else ""
let square_cap () = if !enabled then "⊓" else ""
let square_cup () = if !enabled then "⊔" else ""
let square_subset () = if !enabled then "⊏" else ""
let square_supset () = if !enabled then "⊐" else ""
let square_subset_or_equal () = if !enabled then "⊑" else ""
let square_supset_or_equal () = if !enabled then "⊒" else ""
let normal_subgroup () = if !enabled then "⊲" else ""
let normal_supgroup () = if !enabled then "⊳" else ""
let bottom () = if !enabled then "⊥" else ""
let top () = if !enabled then "⊤" else ""
let bowtie () = if !enabled then "⋈" else ""
let end_of_proof () = if !enabled then "∎" else ""
let assertion () = if !enabled then "⊦" else "|-"
let models () = if !enabled then "⊧" else "|:"
let true_ () = if !enabled then "⊨" else "|="
let forces () = if !enabled then "⊩" else "||–"
let hermitian_conjugate_matrix () = if !enabled then "⊹" else ""
let multimap () = if !enabled then "⊸" else ""
let left_normal_factor_semidirect_product () = if !enabled then "⋉" else ""
let right_normal_factor_semidirect_product () = if !enabled then "⋊" else ""


module Triangle =
struct

  let up () = if !enabled then "▵" else ""
  let down () = if !enabled then "▿" else ""
  let left () = if !enabled then "◃" else ""
  let right () = if !enabled then "▹" else ""
  let big_up () = if !enabled then "△" else ""
  let big_down () = if !enabled then "▽" else ""
  let big_left () = if !enabled then "◁" else ""
  let big_right () = if !enabled then "▷" else ""

end

module Greek =
struct

  let alpha () = if !enabled then "α" else ""
  let beta () = if !enabled then "β" else ""
  let gamma () = if !enabled then "γ" else ""
  let delta () = if !enabled then "δ" else ""
  let epsilon () = if !enabled then "ε" else ""
  let zeta () = if !enabled then "ζ" else ""
  let eta () = if !enabled then "η" else ""
  let theta () = if !enabled then "θ" else ""
  let iota () = if !enabled then "ι" else ""
  let kappa () = if !enabled then "κ" else ""
  let lambda () = if !enabled then "λ" else ""
  let mu () = if !enabled then "μ" else ""
  let nu () = if !enabled then "ν" else ""
  let xi () = if !enabled then "ξ" else ""
  let omicron () = if !enabled then "ο" else ""
  let pi () = if !enabled then "π" else ""
  let rho () = if !enabled then "ρ" else ""
  let final_sigma () = if !enabled then "ς" else ""
  let sigma () = if !enabled then "σ" else ""
  let tau () = if !enabled then "τ" else ""
  let upsilon () = if !enabled then "υ" else ""
  let phi () = if !enabled then "φ" else ""
  let chi () = if !enabled then "χ" else ""
  let psi () = if !enabled then "ψ" else ""
  let omega () = if !enabled then "ω" else ""

end

