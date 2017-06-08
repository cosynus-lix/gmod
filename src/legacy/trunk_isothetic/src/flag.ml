(** Settings which meaningful only for an execution. Long-term flags are stored
    in [Settings]. *)

let verbose  = ref false      (* give pieces of information about the execution of oplate *)
let full_verbose  = ref false (* give pieces of information about the execution of oplate *)
let enl = ref false
let eqn_to_display = ref ""
let ask_which_equation_should_be_displayed = ref false
let mode_options:((string list) ref) = ref []
let depth = ref 0
let branching_mode = ref "semaphore-seeker-branching"
let explore = ref 1
let gui = ref false
let verbosity_during_calculation    = ref false
let verbosity_during_model_building = ref false
let verbosity_during_configuration  = ref false

