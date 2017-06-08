val schedule_algorithm : string ref
val max_execution_steps : int ref
val engine : Semantics.t -> Semantics.ex_aribo Common.StringMap.t ref -> unit -> unit
val depth : int ref
val scenari : (int array array) ref
val current_position : (int array) ref
val active_processes : (bool array) ref
val init_runpro : Semantics.t -> Semantics.process array
