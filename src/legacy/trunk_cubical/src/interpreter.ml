let branching_mode = ref ""

exception Undefined

let schedule_algorithm = ref "gatling"

let memsta = ref Common.StringMap.empty

let depth = ref 17 (* default arbitrary value *)

let scenari = ref (Array.make_matrix 0 0 0)

let current_position = ref (Array.make 0 0)

let active_processes = ref (Array.make 0 true)

let is_boolean e =
  match e with
  | Semantics.Bc(_)
  | Semantics.No(_)
  | Semantics.An(_,_)
  | Semantics.Or(_,_)
  | Semantics.Eq(_,_)
  | Semantics.Lq(_,_)
  | Semantics.Nq(_,_) -> true
  | _       -> false

let is_arithmetic e =
  match e with
  | Semantics.Ac(_)
  | Semantics.Ne(_)
  | Semantics.Pl(_,_)
  | Semantics.Mi(_,_)
  | Semantics.Ti(_,_)
  | Semantics.Di(_,_) -> true
  | _       -> false

module Eval = struct

  let rec arithmetic e =
    match e with
    | Semantics.Ac(n)      ->   n
    | Semantics.Ne(e')     -> -(arithmetic e')
    | Semantics.Pl(e',e'') ->  (arithmetic e')+(arithmetic e'')
    | Semantics.Mi(e',e'') ->  (arithmetic e')-(arithmetic e'')
    | Semantics.Ti(e',e'') ->  (arithmetic e')*(arithmetic e'')
    | Semantics.Di(e',e'') ->  (arithmetic e')/(arithmetic e'')
    | Semantics.Va(name)   ->
      (try (arithmetic (Common.StringMap.find name !memsta)) with _ -> raise Undefined)
    | _ -> raise Undefined

  let rec boolean undefined_variable_n_question_mark_eval e =
    let rec aux e =
      match e with
      | Semantics.Bc(b)      -> b
      | Semantics.No(e')     -> not(aux e')
      | Semantics.An(e',e'') ->
	if (is_boolean e') && (is_boolean e'')
	then (aux e') || (aux e'')
	else raise Undefined
      | Semantics.Or(e',e'') ->
	if (is_boolean e') && (is_boolean e'')
	then (aux e') || (aux e'')
	else raise Undefined
      | Semantics.Lq(e',e'') -> ((arithmetic e') <= (arithmetic e''))
      | Semantics.Eq(e',e'') ->
	if (is_boolean e') && (is_boolean e'')
	then (aux e') = (aux e'')
	else
	  if (is_arithmetic e') && (is_arithmetic e'')
	  then (arithmetic e') = (arithmetic e'')
	  else raise Undefined
      | Semantics.Nq(e',e'') ->
	if (is_boolean e') && (is_boolean e'')
	then (aux e') <> (aux e'')
	else
	  if (is_arithmetic e') && (is_arithmetic e'')
	  then (arithmetic e') <> (arithmetic e'')
	  else raise Undefined
      | Semantics.Va(name) ->
	(
	  try aux (Common.StringMap.find name !memsta)
	  with _ -> print_endline ("Undefined variable "^name) ; (undefined_variable_n_question_mark_eval name)
	)
      | _ -> (*print_endline "Question mark met" ;*) undefined_variable_n_question_mark_eval "?"
    in
    aux e

end (* Eval *)

module Branch =
struct

  (* On génère une matrice LxC où L, c'est-à-dire le nombre de
     lignes, est le nombre de processus en cours d'exécution tandisque
     C, c'est-à-dire le nombre de colonnes, est la profondeur
     d'exploration des branchements que l'on a donnée en argument *)

  let rec seek_P sem code already_visited =
    match code with
    | (Semantics.P(_),_)::code -> true
    | (Semantics.ITE(alternatives),_)::code ->
      (List.fold_left (fun accu x -> (accu || (seek_P sem (fst x)) already_visited)) false alternatives)
      || (seek_P sem code already_visited)
    | (Semantics.C(name),_)::code ->
      (not(Common.StringSet.mem name already_visited))
      &&
	(
          (
	    try
	      seek_P sem (List.assoc name sem.Semantics.equations) (Common.StringSet.add name already_visited)
	    with
	    | _ -> false
	  )
	  || (seek_P sem code already_visited)
	)
    | instruction::code -> seek_P sem code already_visited
    | [] -> false

  let rec seek_R sem code already_visited = match code with
    | (Semantics.R(_,_),_)::code -> true
    | (Semantics.ITE(alternatives),_)::code ->
      (List.fold_left (fun accu x -> ( accu || (seek_R sem (fst x)) already_visited)) false alternatives)
      || (seek_R sem code already_visited)
    | (Semantics.C(name),_)::code ->
      (not(Common.StringSet.mem name already_visited))
      &&
	(
          (
	    try
	      seek_R sem (List.assoc name sem.Semantics.equations) (Common.StringSet.add name already_visited)
	    with
	    | _ -> false
	  )
	  || (seek_R sem code already_visited)
	)
    | instruction::code -> seek_R sem code already_visited
    | [] -> false

  let rec seek_PR sem code already_visited = match code with
    | (Semantics.P(_),_)::code | (Semantics.R(_,_),_)::code -> true
    | (Semantics.ITE(alternatives),_)::code ->
      (List.fold_left (fun accu x -> ( accu || (seek_PR sem (fst x)) already_visited)) false alternatives)
      || (seek_PR sem code already_visited)
    | (Semantics.C(name),_)::code ->
      (not(Common.StringSet.mem name already_visited))
      &&
	(
          (
	    try
	      seek_PR sem (List.assoc name sem.Semantics.equations) (Common.StringSet.add name already_visited)
	    with
	    | _ -> false
	  )
	  || (seek_PR sem code already_visited)
	)
    | instruction::code -> seek_PR sem code already_visited
    | [] -> false

  (* La fonction qui suit détermine quelle alternative d'un
     branchement conditionnel doit être suivi. Dans le cas où l'on ne
     peut pas, faute d'informations, statuer sur la valeur d'une
     condition liée à une alternative, le choix est fait selon que
     cette alternative soit susceptible d'effectuer une prise de
     sémaphore ou pas. *)

  let rec seek_P_when_unknown sem alternatives =
    match alternatives with
    | (code,cond)::alternatives ->
      if
        (
	  try
	    Random.self_init ()
	    ;
	    (Eval.boolean (fun name -> seek_P sem code Common.StringSet.empty) cond)
	  with | _ -> failwith "Interpreter.M.branch : should not happen"
	)
      then
	code
      else
	seek_P_when_unknown sem alternatives
    | [] -> failwith "No alternative"

  let rec randomly alternatives =
    match alternatives with
    | (proc,cond)::alternatives ->
      if
        (
	  try
	    Random.self_init ()
	    ;
	    Random.bool () (* (Eval.boolean (fun name -> Random.bool ()) cond)  *)
	  with | _ -> failwith "Interpreter.M.branch : should not happen"
	)
      then
	proc
      else
	randomly alternatives
    | [] -> failwith "No alternative"

  let rec randomly_when_unknown alternatives =
    match alternatives with
    | (proc,cond)::alternatives ->
      if
        (
	  try
	    Random.self_init ()
	    ;
	    (Eval.boolean (fun name -> Random.bool ()) cond)
	  with | _ -> failwith "Interpreter.M.branch : should not happen"
	)
      then
	proc
      else
	randomly_when_unknown alternatives
    | [] -> failwith "No alternative"

(* Lorsque la fonction next est appelée, on est arrivé au bout
   d'une trace, il faut tester si toutes les éléments du tableau
   current_position sont arrivés au maximum *)

end

let branch sem alternatives =
  match !branching_mode with
    | "random-branching" -> (Branch.randomly_when_unknown alternatives)
    | "semaphore-seeker-branching" -> (Branch.seek_P_when_unknown sem alternatives)
    | _ -> failwith "Interpreter.Branch : unknown option"

let step ?branching_choice sem pid memsta =
  let i = (sem.Semantics.runpro).(pid).Semantics.ip in
  let c = (sem.Semantics.runpro).(pid).Semantics.code in
  let substitute code =
    if code <> [] then
      [ (Array.sub c 0 i); (Array.of_list code); (Array.sub c (i+1) ((Array.length c)-(i+1))) ]
    else
      (
	let fin = try Array.sub c (i+2) ((Array.length c)-(i+2)) with _ -> [||] in
	if fin <> [||]
	then [ (Array.sub c 0 i) ; fin ]
	else
	  (
	    try [ Array.sub c 0 (i-1) ]
	    with _ -> []
	  )
      )
  and terminate () = if i > 0 then Array.sub c 0 (i-1) else [||] in
  try
    (
      match c.(i) with
      | (Semantics.ITE(alternatives),_) ->
	(
	  try
	    (
	      match branching_choice with
	      | None    ->
		(sem.Semantics.runpro).(pid).Semantics.code <- Array.concat (substitute (branch sem alternatives))
	      | Some(a) ->
		(sem.Semantics.runpro).(pid).Semantics.code <- Array.concat (substitute (fst (List.nth alternatives a)))
	    )
	  with
	  | Invalid_argument "List.nth" ->
            (sem.Semantics.runpro).(pid).Semantics.code <- terminate ()
	)
      | (Semantics.C(name),_) ->
	(sem.Semantics.runpro).(pid).Semantics.code <-
	  Array.concat (substitute (try (List.assoc name sem.Semantics.equations) with _ -> []))
      | (Semantics.F(name),_) ->
        (
	  try
	    (
	      sem.Semantics.runpro <-
		(
		  Array.append
		    sem.Semantics.runpro
		    [|
                      (
			(sem.Semantics.runpro).(pid).Semantics.spawns <- ((Array.length (sem.Semantics.runpro))::((sem.Semantics.runpro).(pid).Semantics.spawns));
			{ Semantics.
			  father = pid;
			  spawns = [];
			  code = Array.of_list (List.assoc name sem.Semantics.equations) ;
			  ip = 0
			}
		      )
		    |]
		)
	      ;
	      ((sem.Semantics.runpro).(pid)).Semantics.ip <- ((((sem.Semantics.runpro).(pid)).Semantics.ip) + 2)
	    )
	  with
	  | Not_found -> ( (sem.Semantics.runpro).(pid).Semantics.ip <- (((sem.Semantics.runpro).(pid).Semantics.ip) + 2) )
        )
      (* La création de processus vide est omise *)
      | (Semantics.T(name,bexp),_) ->
	(
	  memsta := Common.StringMap.add name bexp !memsta ;
	  (sem.Semantics.runpro).(pid).Semantics.ip <- (((sem.Semantics.runpro).(pid).Semantics.ip) + 2)
	)
      | _ -> ( (sem.Semantics.runpro).(pid).Semantics.ip <- (((sem.Semantics.runpro).(pid).Semantics.ip) + 2) )
    )
  with
  | _ -> () (* print_string "IP out of range\n" ; failwith "Instruction pointer should not be out of range" *)

let max_execution_steps = ref 0

module Engine =
struct

  let gatling ?choices sem memsta =
    match choices with
    | None ->
      let circ_succ k n =
	let k' = k + 1 in
	if k'<n then k' else 0
      in
      let pid = ref 0 in
      for i = 1 to !max_execution_steps do
	step sem !pid memsta;
	pid := circ_succ !pid (Array.length (sem.Semantics.runpro))
      done
    | Some(choices) ->
      let circ_succ k n =
	let k' = k + 1 in
	if k'<n then k' else 0
      in
      let pid = ref 0 in
      for i = 1 to !max_execution_steps do
	step ~branching_choice:(choices.(!pid).(!current_position.(!pid))) sem !pid memsta ;
	pid := circ_succ !pid (Array.length (sem.Semantics.runpro))
      done

  (* La fonction exhaustive_gatling exécute tous les scenari possibles
     jusqu'à une profonfeur donnée par (depth). A priori, runpro a
     déjà été initialisé. La fonction (scenario_remaining) doit
     renvoyer false s'il n'y a plus de scenario à explorer, true dans
     le cas contraire et avoir un effet de bord qui transmet ce
     scenario *)

  let exhaustive_gatling sem memsta =
    let scenario_remaining () = false in
    while scenario_remaining () do
      gatling ~choices:!scenari sem memsta
    done
end

let engine sem memsta () =
  match !schedule_algorithm with
    | "gatling" -> Engine.gatling sem memsta
    | "exhaustive-gatling" -> Engine.exhaustive_gatling sem memsta
    | _    -> print_string "Invalid schedule option (engine)\n"

(* Cette fonction prend la sémantique en argument et renvoie le
   tableau des processus en cours d'exécution *)

let init_runpro sem =
  let f name =
    let il = try List.assoc name sem.Semantics.equations with Not_found -> [] in
    let code = Array.of_list il in
    { Semantics.
      father = -1; (* initial processes have no ancestor *)
      spawns = []; (* and no son yet *)
      code = code;
      ip = 0}
  in
  let ia = Array.of_list sem.Semantics.initials in
  sem.Semantics.runpro <- Array.map f ia;
  engine sem memsta ();
  sem.Semantics.runpro
