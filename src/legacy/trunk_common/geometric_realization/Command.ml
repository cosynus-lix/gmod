module A = PV2CFG.C.Area

module B = PV2CFG.C.Brick

let all = [ "-all" ; "-a" ; "-" ] (*aliases for the parameter "-all"*)

let get_some = Common.get_some

let actions_to_perform: (unit -> unit) list ref = ref []

let warnings_to_perform: (unit -> unit) list ref = ref []

let errors_to_perform: (unit -> unit) list ref = ref []

let results_to_display: (unit -> unit) list ref = ref []

let add_action a = actions_to_perform := a :: !actions_to_perform

let remove_all_actions () = actions_to_perform := []

let add_warning w = warnings_to_perform := w :: !warnings_to_perform

let add_error e = errors_to_perform := e :: !errors_to_perform

let add_result r = results_to_display := r :: !results_to_display

 (*The actions should not output anything. The displaying functions are explicitly postponed*)

let go () =
  let run f = f () in
  let actions_to_perform = List.rev !actions_to_perform in
  List.iter run actions_to_perform ;
  let results_to_display = List.rev !results_to_display in
  let warnings_to_perform = List.rev !warnings_to_perform in
  let errors_to_perform = List.rev !errors_to_perform in
  List.iter run results_to_display ;
  List.iter run warnings_to_perform ;
  List.iter run errors_to_perform

let specified_keys = ref None

let alphabetical = ref false

let pretty = ref true

let verbose_display = ref false

let verbose_parser = ref false

let verbose_lexer = ref false

let split = Str.split (Str.regexp_string ",")

let predicate_all x = List.mem x all

type parsing_mode = Oda | Dgrog | Cpodgrog | Pv

let sos_from_los l =
  let f accu x = Common.StringSet.add x accu in
  List.fold_left f Common.StringSet.empty l

let valid_resource res_kind_name res sos =
  if Common.StringSet.mem res sos
  then true
  else
  let w = fun () -> Printf.printf "%s The %s %s was ignored because it is not declared in the program.\n" Message.warning res_kind_name (Message.orange res) in
    let () = add_warning w in
    false

module Flag =
struct
(*The following fields should be filled by the function which parses the command line*)
(*filename is the only flag which is initialized from outside – i.e. from Main.ml*)
  let filename: string option ref = ref None
  let parsing_mode = ref None
  let cfg_reduction = ref true
  let selected_procs = ref None
  let mutex_busy_section = ref None
  let semaphore_busy_section = ref None
  let synchronization_busy_section = ref None
  let write_busy_section = ref None
  let read_busy_section = ref None
  let variable_busy_section = ref None
  let channel_busy_section = ref None
  let notify_busy_section = ref None
  let mutex_forbidden_area = ref None
  let semaphore_forbidden_area = ref None
  let hide_segments_carrying_empty_area = ref false
  let cfg_required = ref false
  let verbosity = ref 1
end (*Flag*)

let verbose_output ?(startline=false) ?(endline=false) ?(shift=false) threshold message =
  if !Flag.verbosity > threshold
  then Printf.printf "%s%s%s%s"
    (if startline then "\n" else "")
    (if shift then String.make (2 * threshold) ' ' else "")
    message
    (if endline then "\n" else "")

let get_filename () =
  try get_some !Flag.filename
  with Not_found -> add_error (fun () -> Printf.printf "%s you MUST provide a file!" Message.error); failwith "Missing filename"

let is_pv_parsing_mode () = !Flag.parsing_mode = Some Pv

module Data =
struct
(*The following fields should be filled by the results of the computations*)
  let source = ref None
  let abstract_syntax = ref None
  let semantics : (module Semantics.S) option ref = ref None
  let proc_names = ref None
  let mutex = ref None
  let synchronization = ref None
  let semaphore = ref None
  let variable = ref None
end (*Data*)

module Proceed =
struct

let rec generic_get data set () = match !data with
  | Some x -> x
  | None -> set () ; generic_get data set ()

(*semantics*)

let set_abstract_syntax () =
  let () = verbose_output ~shift:true 1 (Printf.sprintf "Extracting and parsing the content of file %S:" (try get_filename ()  with _ -> failwith "Flag.filename not initialized yet")) in
  let filename = get_filename () in
  let () = Data.abstract_syntax := Some (AbstractSyntax.from_filename filename) in
  verbose_output ~endline:true 1 " done"

let get_abstract_syntax () = generic_get Data.abstract_syntax set_abstract_syntax ()

let set_semantics () =
  let module AS = struct
    let program = get_abstract_syntax ()
    let reduce_cfg = true
    let verbosity = 0 end (*AS*) in
  let module Semantics = Semantics.Make(AS) in
  Data.semantics := Some (module Semantics)

let get_semantics () = generic_get Data.semantics set_semantics ()

let set_proc_names () =
  let proc_names = List.map AbstractSyntax.name_from_process_class (get_abstract_syntax ()).AbstractSyntax.equations in
  Data.proc_names := Some proc_names

let rec get_proc_names () = generic_get Data.proc_names set_proc_names ()

(*selected processes*)

let set_selected_procs () = Flag.selected_procs := Some (get_proc_names ())

let rec get_selected_procs () = match !Flag.selected_procs with
  | Some x -> if x = [List.hd all] then get_proc_names () else x
  | None -> set_selected_procs (); get_selected_procs ()

(*source code*)

let set_source () = match !Data.source with
  | Some _ -> ()
  | None -> Data.source := Some (Common.file_content (get_filename ()))

let rec get_source () = generic_get Data.source set_source ()

let default_value_for_Held cfg =
  let original = fst cfg in
  let open PV2CFG in
  let copy = copy_without_code original in
  Held (D.empty_from_skeleton copy)

let default_value_for_Level cfg =
  let original = fst cfg in
  let open PV2CFG in
  let copy = PV2CFG.copy_without_code original in
  Level [| D.full_from_skeleton copy |]

let default_value_for_section_of_nonsemaphore = default_value_for_Held

let default_value_for_section_of_semaphore sem cfg =
  match AbstractSyntax.semvar_of_kind (snd sem) with
  | `Quantitative -> default_value_for_Level cfg
  | `Default -> default_value_for_Held cfg

(*For any kind of resource the corresonding default value*)

let control_flow_graph name =
  let module X = (val (get_semantics ())) in
  X.control_flow_graph name

let busy_sections ~resource ~procname =
  let module X = (val (get_semantics ())) in
  X.busy_section ~resource ~procname

let default_value_for_section_of_nonsemaphore name = default_value_for_section_of_nonsemaphore (control_flow_graph name)

let default_value_for_section_of_semaphore sem name = default_value_for_section_of_semaphore sem (control_flow_graph name)

(*miscellaneous*)

let set_mutex () = Data.mutex := Some (AbstractSyntax.all_mutex (get_abstract_syntax ()))

let get_mutex () = generic_get Data.mutex set_mutex ()

let set_semaphore () =
  let declarations = (get_abstract_syntax ()).AbstractSyntax.declarations in
  let content = List.filter (fun (_,kind) -> AbstractSyntax.is_semaphore kind) declarations in
  Data.semaphore := Some content

let get_semaphore () = generic_get Data.semaphore set_semaphore ()

let arity_of_semaphore name =
  let f (x,_) = x = name in
  let _,kind = List.find f (get_semaphore ()) in
  let arity = get_some (AbstractSyntax.arity_of_kind kind) in
  if arity >= 0 then arity else failwith "arity_of_semaphore"

let set_synchronization () =
  let declarations = (get_abstract_syntax ()).AbstractSyntax.declarations in
  let f (_,kind) = AbstractSyntax.is_synchronization kind in
  let content = List.filter f declarations in
  Data.synchronization := Some content

let get_synchronization () = generic_get Data.synchronization set_synchronization ()

let arity_of_synchronization name =
  let f (x,_) = x = name in
  let _,kind = List.find f (get_synchronization ()) in
  let arity = get_some (AbstractSyntax.arity_of_kind kind) in
  if arity >= 0 then arity else failwith "arity_of_synchronization"

let set_variable () =
  let variables = AbstractSyntax.all_variables in
  Data.variable := Some variables

let get_variable () = generic_get Data.variable set_variable ()

end(*Proceed*)

module LocalDisplay =
struct

(*Control Flow Graph*)

let cfg () = if is_pv_parsing_mode () then
  let chosen_names = Proceed.get_selected_procs () in
  let chosen_names =
    if List.exists predicate_all chosen_names
    then Proceed.get_proc_names ()
    else chosen_names in
  let f () =
    List.iter
      (fun name ->
        let (cfg , endpoints) = Proceed.control_flow_graph name in
        let name = Common.Terminal.rgb 5 2 0 name in
        let endpoints = if endpoints <> []
            then List.fold_left
              (fun accu ep -> Printf.sprintf "%s %i" accu ep.PV2CFG.V.id)
              (match endpoints
                with [] -> "No endpoint"
                | [_] -> "The unique endpoint is"
                | _   -> "The endpoints are")
            endpoints
            else "No endpoint" in
        Printf.printf "Process name: %s\n" name ;
        PV2CFG.display_cfg cfg ;
        PV2CFG.display_forks cfg ;
        print_endline endpoints)
    chosen_names in
  add_result f

(*Sections*)

let generic_busy_section busy_section flag () =
  let list_of_mutex = Common.List.linearize (get_some !flag) in
  if list_of_mutex <> []
  then add_result
    (fun () -> List.iter
      (fun name ->
        Printf.printf "\nProcess name: %s\n" (Common.Terminal.rgb 5 2 0 name) ;
        List.iter
          (fun res ->
            let vfbs = busy_section (fst res) name in
            let empty = PV2CFG.is_empty_vfbs vfbs in
            Printf.printf "The busy section of %s is%s\n" (Common.Terminal.rgb 5 2 0 (AbstractSyntax.string_of_resource res)) (if empty then " empty" else "") ;
            if not empty then PV2CFG.display_value_for_busy_section vfbs)
          list_of_mutex)
      (Proceed.get_selected_procs ()) )

let mutex_busy_section () =
  let module X = (val (Proceed.get_semantics ())) in
  generic_busy_section X.busy_section_of_mutex Flag.mutex_busy_section ()

let channel_busy_section () =
  let module X = (val (Proceed.get_semantics ())) in
  generic_busy_section X.busy_section_of_channel Flag.channel_busy_section ()

let semaphore_busy_section () =
  let module X = (val (Proceed.get_semantics ())) in
  generic_busy_section X.busy_section_of_semaphore Flag.semaphore_busy_section ()

let synchronization_busy_section () =
  let module X = (val (Proceed.get_semantics ())) in
  generic_busy_section X.busy_section_of_synchronization Flag.synchronization_busy_section ()

let write_busy_section () =
  let module X = (val (Proceed.get_semantics ())) in
  generic_busy_section X.busy_section_of_write Flag.write_busy_section ()

let read_busy_section () =
  let module X = (val (Proceed.get_semantics ())) in
  generic_busy_section X.busy_section_of_read Flag.read_busy_section ()

let variable_busy_section () =
  let module X = (val (Proceed.get_semantics ())) in
  generic_busy_section X.busy_section_of_variable Flag.variable_busy_section ()

let notify_busy_section () =
  let module X = (val (Proceed.get_semantics ())) in
  generic_busy_section X.busy_section_of_notify Flag.notify_busy_section ()

(*Areas*)

let generic_forbidden_area flag =
  if is_pv_parsing_mode ()
  then (
    let list_of_resources = Common.List.linearize (get_some !flag) in
    if list_of_resources <> []
    then (
      let f () =
        let () = PV2CFG.E.active_string_of := false in
        let module X = (val (Proceed.get_semantics ())) in
        let fa = List.fold_left (fun accu r -> (X.forbidden_area_of_the_resource r)) X.empty_area list_of_resources in
        let () = Printf.printf "The forbidden area generated by the %s %sis "
          (if list_of_resources = [] then "resource" else "resources")
          (String.concat ", " (List.map fst (get_some !Flag.mutex_forbidden_area ) ) ) in
        let () = if PV2CFG.C.Area.is_not_empty fa
        then Printf.printf "\n%s\n" (PV2CFG.C.Area.string_of fa)
        else print_endline "empty" in
        PV2CFG.E.active_string_of := true in
      add_result f )
    else print_endline "No resource hence empty forbidden area" )

let mutex_forbidden_area () = generic_forbidden_area Flag.mutex_forbidden_area

let semaphore_forbidden_area () = generic_forbidden_area Flag.semaphore_forbidden_area

let forbidden_area () =
  let f () =
    let () = PV2CFG.E.active_string_of := false in
    let module X = (val (Proceed.get_semantics ())) in
    let fa = X.forbidden_area () in
    let () =
    if PV2CFG.C.Area.is_not_empty fa
    then Printf.printf "The forbidden area is:\n%s\n" (PV2CFG.C.Area.string_of fa)
    else print_endline "The forbidden area is empty\n" in
    PV2CFG.E.active_string_of := true in
  add_result f

let swept_forbidden_area () =
  let f () =
    let () = PV2CFG.E.active_string_of := false in
    let module X = (val (Proceed.get_semantics ())) in
    let fa = X.swept_forbidden_area () in
    let () =
    if PV2CFG.C.Area.is_not_empty fa
    then Printf.printf "The swept forbidden area is:\n%s\n" (PV2CFG.C.Area.string_of fa)
    else print_endline "The swept forbidden area is empty\n" in
    PV2CFG.E.active_string_of := true in
  add_result f

let normalized_forbidden_area () =
  let f () =
    let () = PV2CFG.E.active_string_of := false in
    let module X = (val (Proceed.get_semantics ())) in
    let fa = X.normalized_forbidden_area () in
    let () =
    if PV2CFG.C.Area.is_not_empty fa
    then Printf.printf "The normalized forbidden area is:\n%s\n" (PV2CFG.C.Area.string_of fa)
    else print_endline "The normalized forbidden area is empty\n" in
    PV2CFG.E.active_string_of := true in
  add_result f

let states_space () =
  let f () =
    let () = PV2CFG.E.active_string_of := false in
    let module X = (val (Proceed.get_semantics ())) in
    let fa = X.states_space () in
    let () = Printf.printf "The states space is:\n%s\n" (PV2CFG.C.Area.string_of fa) in
    PV2CFG.E.active_string_of := true in
  add_result f

(*Factorization*)

(*The factorization algorithm does not accept the empty area – the Invalid_argument is raised otherwise. However the state
space of a program is not empty so we do not need to check.*)

let factorization () =
  if is_pv_parsing_mode () then
  let module X = (val (Proceed.get_semantics ())) in
  let factors = verbose_output ~shift:true 2 "Factorizing: " ; X.factorization () in
  let () = verbose_output ~endline:true 2 "done" in
  let number_of_factors = List.length factors in
  let prelude =
    if number_of_factors = 1
    then "There is 1 group\n"
    else Printf.sprintf "There are %i independent groups:\n" number_of_factors in
  let human_readable ba = Algebra.BooleanArray.string_of_support ba in
  let f () = Printf.printf "%s%s\n"
    prelude
    (String.concat " " (List.rev_map human_readable factors)) in
  add_result f

let fast_factorization () =
  if is_pv_parsing_mode () then
  let module X = (val (Proceed.get_semantics ())) in
  let factors = verbose_output ~shift:true 2 "Factorizing: " ; X.fast_factorization () in
  let () = verbose_output ~endline:true 2 "done" in
  let number_of_factors = List.length factors in
  let prelude =
    if number_of_factors = 1
    then "There is 1 group\n"
    else Printf.sprintf "There are %i independent groups:\n" number_of_factors in
  let human_readable ba = Algebra.BooleanArray.string_of_support ba in
  let f () = Printf.printf "%s%s\n"
    prelude
    (String.concat " " (List.rev_map human_readable factors)) in
  add_result f

let unsafe_fast_factorization () =
  if is_pv_parsing_mode () then
  let module X = (val (Proceed.get_semantics ())) in
  let factors = verbose_output ~shift:true 2 "Factorizing: " ; X.unsafe_fast_factorization () in
  let () = verbose_output ~endline:true 2 "done" in
  let number_of_factors = List.length factors in
  let prelude =
    if number_of_factors = 1
    then "There is 1 group\n"
    else Printf.sprintf "There are %i independent groups:\n" number_of_factors in
  let human_readable ba = Algebra.BooleanArray.string_of_support ba in
  let f () = Printf.printf "%s%s\n"
    prelude
    (String.concat " " (List.rev_map human_readable factors)) in
  add_result f

let safe_fast_factorization () =
  if is_pv_parsing_mode () then
  let module X = (val (Proceed.get_semantics ())) in
  let factors = verbose_output ~shift:true 2 "Factorizing: " ; X.safe_fast_factorization () in
  let () = verbose_output ~endline:true 2 "done" in
  let number_of_factors = List.length factors in
  let prelude =
    if number_of_factors = 1
    then "There is 1 group\n"
    else Printf.sprintf "There are %i independent groups:\n" number_of_factors in
  let human_readable ba = Algebra.BooleanArray.string_of_support ba in
  let f () = Printf.printf "%s%s\n"
    prelude
    (String.concat " " (List.rev_map human_readable factors)) in
  add_result f

let weak_factorization () =
  if is_pv_parsing_mode () then
  let module X = (val (Proceed.get_semantics ())) in
  let factors = verbose_output ~shift:true 2 "Factorizing weakly: " ; X.weak_factorization () in
  let () = verbose_output ~endline:true 2 "done" in
  let number_of_factors = List.length factors in
  let prelude =
    if number_of_factors = 1
    then "There is 1 group\n"
    else Printf.sprintf "There are %i independent groups:\n" number_of_factors in
  let human_readable ba = Algebra.BooleanArray.string_of_support ba in
  let f () = Printf.printf "%s%s\n"
    prelude
    (String.concat " " (List.rev_map human_readable factors)) in
  add_result f



end(*LocalDisplay*)

module Action =
struct
(*The triggered action depends on the kind of action one has to deal with.*)

let oda () =
  let () = Proceed.set_source () in
  let abstract = Sheet_solver_oda.from_string (get_some !Data.source) in
  let moea = Sheet_solver_oda.solve abstract in
  let () = Printf.printf "%s\nOutput\n\n" (get_some !Data.source)
  in Sheet_solver_oda.print moea

let dgrog () =
  let filename = get_filename () in
  Sheet_solver_dgrog.print
    ?only:(!specified_keys)
    ~alphabetical:(!alphabetical)
    ~pretty:(!pretty)
    ~verbose:(!verbose_display)
    (Sheet_solver_dgrog.from_filename filename)

let cpodgrog () =
  let filename = get_filename () in
  Sheet_solver_cpodgrog.print
    ?only:(!specified_keys)
    ~alphabetical:(!alphabetical)
    ~pretty:(!pretty)
    ~verbose:(!verbose_display) (Sheet_solver_cpodgrog.from_filename filename)

let pv () = print_endline "The source file contains a PV program"

end (*Action*)

module SetFlag =
struct

let switch_flag flag s =
  let s = String.lowercase s in
    match s with
      | "on" -> flag := true
      | "off" -> flag := false
      | _ -> Printf.printf "%s The argument should be %s/%s – case unsensitive\n"
        Message.warning (Message.hl "on") (Message.hl "off")

(*These functions should be called from the module Main only. Each of them fills some field and stack up some
calculation action then some displaying action*)

let arg_as_list s =
  let aux = split s in
  if List.exists predicate_all aux then [List.hd all] else aux

let arg_as_set s = List.fold_left (fun accu x -> Common.StringSet.add x accu) Common.StringSet.empty (arg_as_list s)

let selected_procs s = Flag.selected_procs := Some (arg_as_list s)

let force_parsing_mode s =
  let set m = Flag.parsing_mode := Some m in
  match s with
    | "oda" -> set Oda
    | "dgrog" -> set Dgrog
    | "cpodgrog" -> set Cpodgrog
    | _ -> Printf.printf "%s parsing mode %s is not available\n"
      (Common.Terminal.(color Yellow ~bold:true "Warning:"))
      (Common.Terminal.(color Blue ~bold:true s))

let parsing_mode () =
  let filename = get_filename () in
  match !Flag.parsing_mode with
    | Some x -> ()
    | None ->
      let x = if Filename.check_suffix filename ".oda"
        then (if !Flag.verbosity > 1 then print_endline "oda file format" ; add_action Action.oda ; Oda)
        else if Filename.check_suffix filename ".dgr"
          then (if !Flag.verbosity > 1 then print_endline "dgr file format" ; add_action Action.dgrog ; Dgrog)
          else if Filename.check_suffix filename ".cpr"
            then (if !Flag.verbosity > 1 then print_endline "cpr file format" ; add_action Action.cpodgrog ; Cpodgrog)
            else if Filename.check_suffix filename ".pv"
              then  (verbose_output ~shift:true ~endline:true 0 "pv source" ; Pv)
              else failwith (Message.error^" Unknown file format") in
      Flag.parsing_mode := Some x


let generic_set raw_parameter all_generic kind_of flag action =
  List.iter add_action
  [(fun () ->
    let abstract_syntax = Proceed.get_abstract_syntax () in
    let expanded_parameter =
      let parameters = arg_as_list raw_parameter in
      if List.(mem (hd all) parameters)
      then all_generic abstract_syntax
      else List.map (fun res_name -> res_name , kind_of abstract_syntax res_name) parameters in
    flag := Some expanded_parameter) ;
  action]

let set_mutex_forbidden_area s =
  let open AbstractSyntax in
  generic_set s
    all_mutex mutex_kind Flag.mutex_forbidden_area LocalDisplay.mutex_forbidden_area

let set_semaphore_forbidden_area s =
  let open AbstractSyntax in
  generic_set s
    all_semaphores semaphore_kind Flag.semaphore_forbidden_area LocalDisplay.semaphore_forbidden_area

let set_forbidden_area () = add_action LocalDisplay.forbidden_area

let set_swept_forbidden_area () = add_action LocalDisplay.swept_forbidden_area

let set_normalized_forbidden_area () = add_action LocalDisplay.normalized_forbidden_area

let set_states_space () = add_action LocalDisplay.states_space

let set_factorization () = add_action LocalDisplay.factorization

let set_fast_factorization () = add_action LocalDisplay.fast_factorization

let set_unsafe_fast_factorization () = add_action LocalDisplay.unsafe_fast_factorization

let set_safe_fast_factorization () = add_action LocalDisplay.safe_fast_factorization

let set_weak_factorization () = add_action LocalDisplay.weak_factorization

let set_mutex_busy_section s =
  let open AbstractSyntax in
  generic_set s
    all_mutex mutex_kind Flag.mutex_busy_section LocalDisplay.mutex_busy_section

let set_channel_busy_section s =
  let open AbstractSyntax in
  generic_set s
    all_channels channel_kind Flag.channel_busy_section LocalDisplay.channel_busy_section

let set_semaphore_busy_section s =
  let open AbstractSyntax in
  generic_set s
    all_semaphores semaphore_kind Flag.semaphore_busy_section LocalDisplay.semaphore_busy_section

let set_synchronization_busy_section s =
  let open AbstractSyntax in
  generic_set s
    all_synchronizations synchronization_kind Flag.synchronization_busy_section LocalDisplay.synchronization_busy_section

let set_write_busy_section s =
  let open AbstractSyntax in
  generic_set s
    (fun s -> Sod.elements (all_write_variables s)) (fun _ _ -> AbstractSyntax.Variable `Write) Flag.write_busy_section LocalDisplay.write_busy_section

let set_read_busy_section s =
  let open AbstractSyntax in
  generic_set s
    (fun s -> Sod.elements (all_read_variables s)) (fun _ _ -> AbstractSyntax.Variable `Read) Flag.read_busy_section LocalDisplay.read_busy_section

let set_access_busy_section s =
  let open AbstractSyntax in
  generic_set s
    (fun s -> Sod.elements (all_variables s)) variable_kind Flag.variable_busy_section LocalDisplay.variable_busy_section

let set_notify_busy_section s =
  let open AbstractSyntax in
  generic_set s
    (fun s -> (all_notify s)) notify_kind Flag.notify_busy_section LocalDisplay.notify_busy_section

let set_cfg_display () =
  add_action LocalDisplay.cfg

let set_silent () =
  Flag.verbosity := 0

let set_lexer_verbose () =
  add_action (fun () -> ignore (Proceed.get_abstract_syntax ())) ;
  Parser_pv_flags.lexer_verbose := true

let set_parser_verbose () =
  add_action (fun () -> ignore (Proceed.get_abstract_syntax ())) ;
  Parser_pv_flags.parser_verbose := true

end(*SetFlag*)
