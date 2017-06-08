module type Empty = sig end

module Empty = struct end

module Mos = Map.Make(String)

module UniqueProcessIdentifier = struct
  (*The first component provides the identifier of the process, and the second
  one provides the serial number of the clone*)
  type t = string * int
  let compare = Pervasives.compare
end (* UniqueProcessIdentifier *)

module Mor = Map.Make(AbstractSyntax.Resource)

(*block of parallel instructions as multisets*)
module MapOfUPIs = Map.Make(UniqueProcessIdentifier)

module SetOfUPIs = Set.Make(UniqueProcessIdentifier)

module SetOfRUPIs = Set.Make(struct
  type t = AbstractSyntax.Resource.t * UniqueProcessIdentifier.t
  let compare = Pervasives.compare
end)

module MapOfRUPIs = Map.Make(struct
  type t = AbstractSyntax.Resource.t * UniqueProcessIdentifier.t
  let compare = Pervasives.compare
end)

module Text(E:Empty) = struct

  let text = ref []

  let store msg = text := msg :: !text

  let flush () = List.iter print_string (List.rev !text) ; text := []

end (* Text *)

module MonoContainer(S:Set.S)(M:Map.S with type key = S.elt) =
struct

  let container = ref None

  let reset () = container := None

  let store x =
    container := Some x

  let added_to container' =
    match !container with
    | Some x -> container' := S.add x !container'
    | None -> ()

  let incr container' =
    match !container with
    | Some key -> (
        let x = try succ (M.find key !container') with Not_found -> 1 in
        container' := M.add key x !container')
    | None -> ()

end (*MonoContainer*)

module SetContainer(S:Set.S) = struct

  let container = ref S.empty

  let get () = !container

  let reset () = container := S.empty

  let store x = container := S.add x (get ())

  let mem x = S.mem x (get ())

  let joined_to container' = container' := S.union (get ()) !container'

end (*SetContainer*)

module Counters(M:Map.S) = struct

  let container = ref M.empty

  let reset () = container := M.empty

  let get () = !container

  let is_initialized key = M.mem key (get ()) 

  let set_counter key x = container := M.add key x (get ())

  let update key x = 
    if is_initialized key 
    then set_counter key x
    else failwith (Printf.sprintf "Counter is not initialized")

  let incr key =
    let x =
      try succ @@ M.find key (get ())
      with Not_found -> 1 in
    set_counter key x

  let decr key =
    let x =
      try pred @@ M.find key (get ())
      with Not_found -> 0 in
    set_counter key x

  let get_counter key = M.find key (get ())

end (* Counters *)

module SerialNumber(M:Map.S) = struct

  let counters = ref M.empty

  let fresh identifier =
    let fresh =
      try succ @@ M.find identifier !counters
      with Not_found -> 0 in
    let () = counters := M.add identifier fresh !counters in
    fresh

end (* SerialNumber *)

let draw_line () = if false then print_endline ((String.make 120 '_')^"\n")

(* Make *)

module Make(Source:InterpreterSignature.Parameter) : InterpreterSignature.S =
struct

  open AbstractSyntax

  type environment = int Mod.t

  type machine_state = ( (instruction list) * environment * bool * string ) array

  module Preface = Text(Empty)

  module Principal = Text(Empty)

  module Postface = Text(Empty)

  module Previously = struct

    module Read = SetContainer(SetOfRUPIs)

    module Written = SetContainer(SetOfRUPIs)

    module P = Counters(MapOfRUPIs)

    module V = Counters(MapOfRUPIs)

    module Freed = SetContainer(SetOfUPIs)

    module Created = SetContainer(SetOfUPIs)
    
    let reset () = 
      Read.reset () ; 
      Written.reset () ; 
      P.reset () ; 
      V.reset () ; 
      Freed.reset () ; 
      Created.reset

  end(*Previously*)

  module Currently = struct

    module Read = SetContainer(SetOfRUPIs)

    module Written = MonoContainer(SetOfRUPIs)(MapOfRUPIs)

    module P = MonoContainer(SetOfRUPIs)(MapOfRUPIs)

    module V = MonoContainer(SetOfRUPIs)(MapOfRUPIs)

    let reset () = 
      Read.reset () ; 
      Written.reset () ; 
      P.reset () ; 
      V.reset () 

  end(*Currently*)

  module Clone = SerialNumber(Mos)

  module ParallelBlock = struct

    include Counters(MapOfUPIs)

    let flush () =
      let not_first = ref false in
      let () = MapOfUPIs.iter
        (fun key value ->
          let coefficient = if value > 1 then Printf.sprintf "%i*" value else ""  in
          Preface.store (
            if !not_first
            then Printf.sprintf " + %s%s.%i" coefficient (fst key) (snd key)
            else (not_first :=  true ; Printf.sprintf "  %s%s.%i" coefficient (fst key) (snd key) ) ) )
        (get ()) in
      let () =
        if not @@ MapOfUPIs.is_empty (get ())
        then Preface.store "\n" in
      reset ()

  end (* ParallelBlock *)

  let current_round = ref 0

  let nb_of_process = ref (List.length Source.program.initials)

  let delayed_print_nb_of_running_process () =
    Postface.store @@
    Printf.sprintf "  there %s %i process%s running\n"
      (if !nb_of_process > 1 then "are" else "is")
      !nb_of_process
      (if !nb_of_process > 1 then "es" else "")

  let name_to_body identifier = name_to_body identifier Source.program.equations

  let initial_environment =
    let resources =
      (AbstractSyntax.all_mutex Source.program) @
      (AbstractSyntax.all_semaphores Source.program) @
      (AbstractSyntax.all_synchronizations Source.program) in
    List.fold_left
      (fun accu res -> AbstractSyntax.Mod.add res 0 accu)
        AbstractSyntax.Mod.empty
        resources

  let current_environment = ref initial_environment

(*
  let number_of_P = ref 0

  let number_of_V = ref 0

  let is_parallelizable res =
    let arity =
      if is_semaphore (snd res)
      then semaphore_arity Source.program (fst res)
      else 2 in
    (Mod.find res !current_environment) + !number_of_P < arity &&
      (Mod.find res !current_environment) - !number_of_V >= 0
*)

  let value_of var = Mod.find var !current_environment

  let reset res =
    current_environment := Mod.add res 0 !current_environment

  let increase res =
    current_environment := Mod.add res (succ @@ (try Mod.find res !current_environment with Not_found -> 0)) !current_environment

  let decrease res =
    current_environment := Mod.add res (pred @@ (Mod.find res !current_environment)) !current_environment

  let initial_state =
    Array.of_list
      (List.map
        (fun name -> (name_to_body name , Mod.empty , false , name , Clone.fresh name))
        Source.program.initials)

  let current_state = ref initial_state

  let unstuck () =
    current_state := Array.map (fun (a,b,c,d,e) -> a,b,false,d,e) !current_state

  let remove pid = Printf.printf "remove %i\n" pid ;
    decr nb_of_process ;
    current_state :=
      Array.init !nb_of_process
        (fun pid' -> !current_state.(if pid' < pid then pid' else succ pid'))

  let state_of_pid pid = !current_state.(pid)

  let fst_of_pid pid = let proc,_,_,_,_ = state_of_pid pid in proc

  let snd_of_pid pid = let _,held,_,_,_ = state_of_pid pid in held

  let thd_of_pid pid = let _,_,stuck,_,_ = state_of_pid pid in stuck

  let name_of_pid pid = let _,_,_,name,_ = state_of_pid pid in name

  let rank_of_pid pid = let _,_,_,_,rank = state_of_pid pid in rank

  let uid_of pid = let _,_,_,name,rank = state_of_pid pid in (name , rank)

  let update_state pid proc held stuck name rank =
    !current_state.(pid) <- (proc , held , stuck , name , rank)

  let not_parallel = ref false

  let is_not_parallel () = !not_parallel

  let start_parallel_block () =
    not_parallel := false ;
    Previously.(
      Freed.reset () ;
      Created.reset () ;
      P.reset () ;
      V.reset () ;
      Written.reset () ;
      Read.reset ())

  let not_recently_created uid = not @@ Previously.Created.mem uid

  let recently_created uid =
    Previously.Created.store uid

  let not_just_freed uid = not @@ Previously.Freed.mem uid

  let just_freed uid = Previously.Freed.store uid

  let in_conflict x uid set =
    SetOfRUPIs.exists (fun (x' , uid') -> x = x' && uid <> uid') set

(* IL EST LÀ LE GROS BUG: addition de toutes les ressources alors que seules 
celles corresondant à res doivent être comptées...et en plus il compte dans P 
et pas dans V...mais apparemment il en reste ailleurs!!! *)

  let token_freed_by_other_processes_so_far res uid = 
    MapOfRUPIs.fold 
      (fun (x , uid') v accu -> if x = res && uid' <> uid then accu + v else accu) 
      (Previously.V.get ())
      0

(*
  a: arité de x.
  c: le nombre d'occurrences de x prises jusqu'au tour précédent.
  v: le nombre d'occurrences de x libérées depuis le début du bloc parallèle par des processus autres que uid.
  en parallèle = c + v < a - 1
*)

  let in_conflict_P x uid =
    let resource_kind = Resource.kind x in
    let arity =
      if is_semaphore resource_kind
      then semaphore_arity Source.program (Resource.id x)
      else 2 in
    let current_resource_state = Mod.find x !current_environment in
    let number_of_tokens_freed_by_other_processes_so_far = token_freed_by_other_processes_so_far x uid in
    let answer = current_resource_state + number_of_tokens_freed_by_other_processes_so_far >= pred arity in (*C'est le contraire: corrigé*)
    let () = if answer then (
      Principal.store (Printf.sprintf "  current_resource_state %s = %i\n" (fst x) current_resource_state) ;
      Principal.store (Printf.sprintf "  number_of_tokens_freed_by_other_processes_so_far %s = %i\n" (fst x) number_of_tokens_freed_by_other_processes_so_far) ;
      Principal.store (Printf.sprintf "  pred arity of %s = %i\n" (fst x) (pred arity))  ) in
    answer

(*
  a: arité de x.
  c: le nombre d'occurrences de x prises jusqu'au tour précédent.
  v: le nombre d'occurrences de x libérées depuis le début du bloc parallèle par des processus autres que uid.
  en parallèle = c + v < a
*)

  let in_conflict_V x uid =
    let resource_kind = Resource.kind x in
    let arity =
      if is_semaphore resource_kind
      then semaphore_arity Source.program (Resource.id x)
      else 2 in
    let current_resource_state = Mod.find x !current_environment in
    let number_of_tokens_freed_by_other_processes_so_far = token_freed_by_other_processes_so_far x uid in
    let answer = current_resource_state + number_of_tokens_freed_by_other_processes_so_far >= arity in (*idem*)
    let () = if answer then (
      Principal.store (Printf.sprintf "  current_resource_state %s = %i\n" (fst x) current_resource_state) ;
      Principal.store (Printf.sprintf "  number_of_tokens_freed_by_other_processes_so_far %s = %i\n" (fst x) number_of_tokens_freed_by_other_processes_so_far) ;
      Principal.store (Printf.sprintf "  arity of %s = %i\n" (fst x) (arity))  ) in
    answer



  let preliminary uid = 
    let answer = not Previously.(Created.mem uid || Freed.mem uid) in
    let () = if not answer then Principal.store (Printf.sprintf "  %s.%i has been created or freed before\n" (fst uid) (snd uid)) in
    answer

  let altered_by_P res pid =
    let uid = uid_of pid in
    Currently.P.store (res , uid) ;
    not_parallel := not (preliminary uid && not @@ in_conflict_P res uid)

  let altered_by_V res pid =
    let uid = uid_of pid in
    Currently.V.store (res , uid) ;
    not_parallel := not (preliminary uid && not @@ in_conflict_V res uid)

  let set_variable identifier value pid =
    let uid = uid_of pid in
    let var = identifier , Variable `Write in
    let () = Currently.Written.store (var,uid) in
    let () = not_parallel := in_conflict var uid (Previously.Written.get ()) || in_conflict var uid (Previously.Read.get ()) in
      current_environment := Mod.add var value !current_environment

  let content_of_variable identifier pid =
    try
      let var = identifier , Variable `Write in
      let content = Mod.find var !current_environment in
      let uid = uid_of pid in
      let () = Currently.Read.store (var , uid) in
      let () = not_parallel := in_conflict var uid (Previously.Written.get ()) in
      content
    with Not_found -> failwith @@ Printf.sprintf "Undefined variable %s" identifier

  let evaluate exp pid =  
    let rec evaluate exp =
      match exp with
        | Va identifier -> content_of_variable identifier pid
        | Ac a -> a
        | Bc b -> if b then 1 else 0
        | Ne exp -> - (evaluate exp)
        | Ab exp -> let x = evaluate exp in if x < 0 then -x else x
        | Pl (exp1 , exp2) -> (evaluate exp1) + (evaluate exp2)
        | Mi (exp1 , exp2) -> (evaluate exp1) - (evaluate exp2)
        | Ti (exp1 , exp2) -> (evaluate exp1) * (evaluate exp2)
        | Di (exp1 , exp2) -> (evaluate exp1) / (evaluate exp2)
        | Mo (exp1 , exp2) -> (evaluate exp1) mod (evaluate exp2)
        | An (exp1 , exp2) -> if (evaluate exp1 <> 0) && (evaluate exp2 <> 0) then 1 else 0
        | Or (exp1 , exp2) -> if (evaluate exp1 <> 0) || (evaluate exp2 <> 0) then 1 else 0
        | Lq (exp1 , exp2) -> if evaluate exp1 <= evaluate exp2 then 1 else 0
        | Ls (exp1 , exp2) -> if evaluate exp1 <  evaluate exp2 then 1 else 0
        | Gq (exp1 , exp2) -> if evaluate exp1 >= evaluate exp2 then 1 else 0
        | Gs (exp1 , exp2) -> if evaluate exp1 >  evaluate exp2 then 1 else 0
        | Nq (exp1 , exp2) -> if evaluate exp1 <> evaluate exp2 then 1 else 0
        | Eq (exp1 , exp2) -> if evaluate exp1 = evaluate exp2 then 1 else 0
        | No exp       -> if (evaluate exp <> 0) then 1 else 0
        | _ -> failwith "expression cannot be evaluated" in
      evaluate exp

let action_P res pid =
  let x = try Mod.find res !current_environment with Not_found -> 0 in
  let proc , held , stuck , name , rank = state_of_pid pid in
  let h = try Mod.find res held with Not_found -> 0 in
  match res with
    | (mtx , Mutex mtx_var) ->
      (match mtx_var with
        | `Default -> (
          if h < 1 (*has the process already held the mutex?*)
          then ( (*no*)
            if x < 1 (*Is the mutex already held by another process?*)
            then (*no – otherwise the process is stuck*)
              let proc' = List.tl proc in
              let held' = Mod.add res 1 held in (
              altered_by_P res pid ; (*cette ligne contribue on contrôle du parallélisme*)
              update_state pid proc' held' false name rank ;
              increase res)
            else update_state pid proc held true name rank)
          else (*yes: unspecified behavior*) failwith "Warning: undefined behavior Default mutex")
        | `Normal -> (
          if h < 1 (*does the process already held the mutex?*)
          then ( (*no*)
            if x < 1 (*Is the mutex already held by another process?*)
            then (*no – otherwise the process is stuck*)
              let proc' = List.tl proc in
              let held' = Mod.add res 1 held in (
              altered_by_P res pid ;
              update_state pid proc' held' false name rank ;
              increase res)
            else (*yes: the process is stuck*)
              update_state pid proc held true name rank)
          else ( (*yes*)
              let proc' = List.tl proc in
              update_state pid proc' held false name rank) )
        | `Recursive -> (
            if x < 1 (*Is the mutex already held by some process?*)
            then (*no – otherwise the process is stuck*)
              let proc' = List.tl proc in
              let held' = Mod.add res 1 held in (
              altered_by_P res pid ;
              update_state pid proc' held' false name rank ;
              increase res)
            else (*yes: the process is stuck*)
              update_state pid proc held true name rank)
        | `Error_check -> (
            if h < 1 (*does the process already held the mutex?*)
            then ( (*no*)
              if x < 1 (*Is the mutex already held by another process?*)
              then (*no – otherwise the process is stuck*)
                let proc' = List.tl proc in
                let held' = Mod.add res 1 held in (
                altered_by_P res pid ;
                update_state pid proc' held' false name rank ;
                increase res)
              else (*yes: the process is stuck*)
                update_state pid proc held true name rank)
            else (*yes: error*) failwith "Error: Error_check mutex does not allow instruction P on held mutex")
      )
    | (sem , Semaphore semaphore_var) ->
      (match semaphore_var with
        | `Default , arity -> (
          if h < 1 (*does the process already held the mutex?*)
          then ( (*no*)
            if x < pred arity (*How many processes have already held the semaphore?*)
            then (*not too much – otherwise the process is stuck*)
              let proc' = List.tl proc in
              let held' = Mod.add res 1 held in (
              altered_by_P res pid ;
              update_state pid proc' held' false name rank ;
              increase res)
            else ( (*yes: the process is stuck*)
              update_state pid proc held true name rank)
            )
          else (*yes: unspecified behavior*) failwith "Warning: undefined behavior Default semaphore")
        | `Quantitative , arity ->
            if x < pred arity (*How many processes have already held the semaphore?*)
            then (*not too much – otherwise the process is stuck*)
              let proc' = List.tl proc in
              let held' = Mod.add res (succ h) held in (
              altered_by_P res pid ;
              update_state pid proc' held' false name rank ;
              increase res)
            else (*too much: the process is stuck*)
              update_state pid proc held true name rank)
    | _ -> failwith "Error: Instruction P only applies to mutex or semaphore"

let action_V res pid =
  let () = altered_by_V res pid in
  let proc , held , stuck , name , rank = state_of_pid pid in
  let h = try Mod.find res held with Not_found -> 0 in
  match res with
    | (mtx , Mutex mtx_var) ->
      (match mtx_var with
        | _ -> (
          if h > 0 (*does the process already held the mutex?*)
          then ( (*yes*)
            let proc' = List.tl proc in
            let held' = Mod.add res 0 held in (
            update_state pid proc' held' false name rank ;
            unstuck () ;
            decrease res) )
          else (*no: unspecified behavior*) failwith "Warning: V undefined behavior Default mutex"))
    | (sem , Semaphore semaphore_var) ->
      (match semaphore_var with
        | `Default , arity -> (
          if h > 0 (*does the process already held the mutex?*)
          then ( (*yes*)
            let proc' = List.tl proc in
            let held' = Mod.add res 0 held in (
            update_state pid proc' held' false name rank ;
            unstuck () ;
            decrease res) )
          else (*yes: unspecified behavior*) failwith "Warning: undefined behavior Default semaphore")
        | `Quantitative , arity ->
          if h > 0 (*does the process already held the mutex?*)
          then ( (*yes*)
            let proc' = List.tl proc in
            let held' = Mod.add res (pred h) held in (
            update_state pid proc' held' false name rank ;
            unstuck () ;
            decrease res) ) )
    | _ -> failwith "Error: Instruction V only applies to mutex or semaphore"

let ready_to_raise res =
  match res with
  | (_ , Synchronization arity) ->
    (try Mod.find res !current_environment = arity with Not_found -> false)
  | _ -> failwith "Error: ready_to_raise only applies to synchronization"

let not_behind_the_gate res pid = not @@ Mod.mem res (snd_of_pid pid)

let try_to_raise_the_gate res =
  if ready_to_raise res
  then (
    reset res ;
    current_state :=
      Array.mapi
        (fun i (proc , held , stuck , name , rank) ->
          if Mod.mem res held
          then (
            just_freed (name , rank) ;
            Principal.store (Printf.sprintf "%s%s.%i" (if i > 0 then " | " else "") name rank) ;
            (List.tl proc , Mod.remove res held , false , name , rank) )
          else (proc , held , stuck , name , rank))
        !current_state ;
    Principal .store (Printf.sprintf ": %s\n" (string_of_instruction  (W res , (0,0))) ) )

let action_W res pid =
  let proc , held , stuck , name , rank = !current_state.(pid) in
  match res with
    | (syn , Synchronization arity) -> (
        if not_behind_the_gate res pid
        then (
          let held' = Mod.add res 1 held in
          update_state pid proc held' true name rank ;
          increase res) ;
        try_to_raise_the_gate res)
    | _ -> failwith "Error: Instruction W only applies to synchronization"

let action_ITE alternative pid =
  let (_ , held , _ , name , rank) = !current_state.(pid) in
  let selected_proc = ref [] in
  let () =
    try
      List.iter
        (fun (expression , proc) -> if (evaluate expression pid) <> 0 then (selected_proc := proc ; raise Exit))
        alternative
    with Exit -> () in
  update_state pid !selected_proc held false name rank

let action_evaluate_and_set identifier expression pid =
  let proc , held , stuck , name , rank = state_of_pid pid in
  let value = evaluate expression pid in
  let () = set_variable identifier value pid in
  update_state pid (List.tl proc) held false name rank

let action_C identifier pid =
  let proc , held , stuck , name , rank = state_of_pid pid in
  let proc' = name_to_body identifier in
  let proc'' = proc' @ (List.tl proc) in
  update_state pid proc'' held false name rank

let action_F identifier pid =
  let proc , held , stuck , name , rank = state_of_pid pid in
  update_state pid (List.tl @@ proc) held stuck name rank ;
  incr nb_of_process ;
  let rank = Clone.fresh identifier in
  let uid = identifier , rank in
  recently_created uid ;
  current_state := Array.append !current_state [|name_to_body identifier , Mod.empty , false , identifier , rank|]

let action_Nop pid = ()

let is_not_stuck pid =
  match !current_state.(pid) with
  | (_,_,stuck,_,_) -> not stuck

let action instruction pid =
  match instruction with
    | P res -> action_P res pid
    | V res -> action_V res pid
    | S _ -> print_endline "Warning: Instruction 'Send' is ignored"
    | R _ -> print_endline "Warning: Instruction 'Receive' is ignored"
    | M _ -> print_endline "Warning: Instruction 'Monitor' is ignored"
    | N _ -> print_endline "Warning: Instruction 'Notify' is ignored"
    | A _ -> print_endline "Warning: Instruction 'notify All' is ignored"
    | F identifier -> action_F identifier pid
    | W res -> action_W res pid
    | C identifier -> action_C identifier pid
    | E _ -> print_endline "Warning: Instruction 'E' is ignored"
    | D _ -> print_endline "Warning: Type declaration is ignored"
    | T (identifier , expression) -> action_evaluate_and_set identifier expression pid
    | ITE alternative -> action_ITE alternative pid
    | Nop -> action_Nop pid

let instruction_to_perform pid = List.hd (fst_of_pid pid)

let program_is_not_done () = !current_state <> [||]

let program_is_stuck () =
  !current_state <> [||] &&
  try Array.iter (fun (proc,_,stuck,_,_) -> if not stuck && proc <> [] then raise Exit) !current_state ; true
  with Exit -> false

let remove_finished_processes () =
  let some_process_were_removed = ref false in
  current_state :=
    Array.of_list
    @@ (List.filter
      (fun (proc,_,_,name,rank) ->
        let b = proc <> [] in
        if not b
        then (
          decr nb_of_process ;
          (if not !some_process_were_removed
          then (some_process_were_removed := true ; Preface.store "  ")) ;
          Preface.store @@ Printf.sprintf "%s.%i " name rank) ;
          b))
    @@ Array.to_list
    @@ !current_state ;
    if !some_process_were_removed then Preface.store "done\n"

(*Scenario is supposed to be an "oracle" which returns either an integer or
raise Exit –if it is over– each time it is called.*)

exception Deadlock

let not_first_instruction = ref false

let break_parallelism () =
  start_parallel_block () ;
  ParallelBlock.flush () ;
  Preface.store "  break parallelism\n"

let play scenario =
  try
    (* Preamble should be put here *)
    while
      (let nb_of_process_before_clean_up = !nb_of_process in
        remove_finished_processes () ;
        (if nb_of_process_before_clean_up <> !nb_of_process
        then (
          if !nb_of_process > 0
          then
            Preface.store @@ Printf.sprintf "  %i process%s alive\n"
              !nb_of_process (if !nb_of_process > 1 then "es" else "") )) ;
        program_is_not_done ()) do
      if program_is_stuck () then raise Deadlock ;
      let pid = scenario !nb_of_process in
      if is_not_stuck pid
      then (
        let () = Currently.reset () in
        let alive_before_action = !nb_of_process in
        let instruction = instruction_to_perform pid in
        let () = action (fst instruction) pid in
        let action_has_been_performed = is_not_stuck pid in
        let () =
          if is_not_parallel ()
          then break_parallelism () in
        let () =
          Currently.P.incr Previously.P.container ;
          Currently.V.incr Previously.V.container ;
          Currently.Read.joined_to Previously.Read.container ;
          Currently.Written.added_to Previously.Written.container in
        let () =
          if action_has_been_performed
          then (
            let uid = uid_of pid in
            let () = ParallelBlock.incr uid in
            match instruction with
            | W _ , _-> ()
            | T (identifier,_) , _ ->
                let var = identifier,Variable `Write in
                let value = value_of var in
                let instruction = string_of_instruction instruction in
                Principal.store @@ Printf.sprintf "%s.%i: %s\n  %s = %i\n"
                  (fst uid) (snd uid) instruction identifier value
            | _ ->
                let instruction = string_of_instruction instruction in
                Principal.store @@ Printf.sprintf "%s.%i: %s\n"
                  (fst uid) (snd uid) instruction) in
          let () =
            if alive_before_action <> !nb_of_process
            then delayed_print_nb_of_running_process () in
          let () =
            if !not_first_instruction && action_has_been_performed
            then draw_line () in
          not_first_instruction := true ;
          Preface.flush () ;
          Principal.flush () ;
          Postface.flush () )
    done ;
    print_endline "End of program"
  with
    | Exit -> print_endline "End of scenario"
    | Deadlock -> print_endline "Deadlock"
    | Failure err_msg -> print_endline err_msg

end (* Make *)

(* TODO:

Raffiner le critère de mise en parallèle pour les instructions P et V :
pour une ressource x dont nx jetons sont pris, on teste si nx + #P < arity(x)
et nx - #V >= 0, si c'est le cas, alors on peut en fait exécuter toutes les
instructions P et V en parallèle.

Lorsqu'un 'Break Parallelism' survient, donner un message qui explique pourquoi.
En particulier, il faut numéroter les cycles de la boucle de l'interpréteur.

*)
