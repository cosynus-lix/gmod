(* Implements the function returnring the control flow graph from any PV program. *)

module Message = struct
  let warning = ref ("\027["^"38;5;1;198"^"Warning"^"\027[0m")
  let error = ref ("\027["^"1;31"^"Error"^"\027[0m")
end (*Message*)

(*The underlying assumption is that different resource have different names*)

let section_associated_with_mutex name busy_sections default =
  let filtered = AbstractSyntax.Mod.filter (fun (name' , rk ) _ -> AbstractSyntax.is_mutex rk && name = name') busy_sections in
  try snd (AbstractSyntax.Mod.choose filtered)
  with Not_found -> default

let section_associated_with_semaphore name busy_sections default =
  let filtered = AbstractSyntax.Mod.filter (fun (name' , rk ) _ -> AbstractSyntax.is_semaphore rk && name = name') busy_sections in
  try snd (AbstractSyntax.Mod.choose filtered)
  with Not_found -> default

let section_associated_with_synchronization name busy_sections default =
  let filtered = AbstractSyntax.Mod.filter (fun (name' , rk ) _ -> AbstractSyntax.is_synchronization rk && name = name') busy_sections in
  try snd (AbstractSyntax.Mod.choose filtered)
  with Not_found -> default

let section_associated_with_write name busy_sections default =
  let filtered = AbstractSyntax.Mod.filter (fun (name' , rk ) _ -> AbstractSyntax.is_write rk && name = name') busy_sections in
  try snd (AbstractSyntax.Mod.choose filtered)
  with Not_found -> default

let section_associated_with_read name busy_sections default =
  let filtered = AbstractSyntax.Mod.filter (fun (name' , rk ) _ -> AbstractSyntax.is_read rk && name = name') busy_sections in
  try snd (AbstractSyntax.Mod.choose filtered)
  with Not_found -> default

(* The following module have to be customed in order to be used as parameters for the functors ODA, DGROG and CPODGROG *)

module V =
struct
  (*A fresh identifier has to be provided for each vertex*)
  (*After reset, the first identifier to be created is 0, thus providing 0 as the identifier of the entry point*)
  let identifier_to_be_created = ref 0
  let reset_identifier () = identifier_to_be_created := 0
  let fresh_identifier () =
    let id = !identifier_to_be_created in
      incr identifier_to_be_created; id
  let decr_identifier () = decr identifier_to_be_created
  (* Each node represents a branching which is either an "if then else" instruction *)
  type end_kind = Branch | Deadlock of AbstractSyntax.instruction | Error of AbstractSyntax.instruction | Process
  let string_of_end_kind k =
    match k with
      | Branch -> "end of a branch"
      | Deadlock i -> Printf.sprintf "deadlock %s" (AbstractSyntax.string_of_instruction i)
      | Error i -> Printf.sprintf "error %s" (AbstractSyntax.string_of_instruction i)
      | Process -> "end of the process"
  type t =
    {
      mutable id:int ; (*unique identifier*)
      mutable entrypoint:string option ; (*name of the process the vertex is the entry point of*)
      (*for each entry, the first term is a boolean condition triggering a branch, the second one is the unique identifier of
      the associated branch – an arrow*)
      mutable bifurcation:(AbstractSyntax.expression*int) array ;
      (*Is it an endpoint ? Which kind of ?*)
      mutable endpoint: end_kind option
    }
  let id v = v.id
  let entrypoint v = v.entrypoint
  let bifurcation v = v.bifurcation
  let endpoint v = v.endpoint
  let default = { id = 0 ; entrypoint = None ; bifurcation = [||] ; endpoint = None }
  let compare v1 v2 =
    let d = Pervasives.compare v1.id v2.id in
    if d <> 0
    then d
    else
      if v1.entrypoint = v2.entrypoint && v1.bifurcation = v2.bifurcation
      then 0
      else failwith "different vertex share the same identifier"
  let hash { id = n } = n
  let equal = Pervasives.(=)
  let detailed_string_of { id = n ; entrypoint = e_in ; endpoint = e_out } =
    Printf.sprintf "|%i|%s%s"
      n
      (match e_in with
        | None -> ""
        | Some n -> Printf.sprintf " – entry point of process %S" n)
      (match e_out with
        | None -> ""
        | Some kind -> Printf.sprintf " – %s" (string_of_end_kind kind))

  let string_of v = Printf.sprintf "|%i|" (id v)

  let strings_of_bifurcation { id = _ ; bifurcation = b } =
    Array.map (fun (precond,id) -> Printf.sprintf "%s –> %i" (AbstractSyntax.string_of_expression precond) id) b
end(*V*)

module MoV = Map.Make(V)

module E =
struct

  let identifier_to_be_created = ref 0

  let reset_identifier () = identifier_to_be_created := 0

  let fresh_identifier () =
   let id = !identifier_to_be_created in
   incr identifier_to_be_created; id

  type t =
   {
     id:int;
     code:AbstractSyntax.instruction array (* The labels contain list of instructions to execute *)
   }

  let id e = e.id

  let code e = e.code

  let compare x1 x2 = Pervasives.compare x1.id x2.id

  let default = { id = 0 ; code = [||] }

  let remove_code { id = id ; code = _ } = { id ; code = [||] }

  let active_string_of = ref true

  let string_of { id = n ; code = code } =
    Printf.sprintf "%i:%s" n
      (if !active_string_of
       then AbstractSyntax.string_of_instruction_list ~no_new_line:true (Array.to_list code)
       else "")

end(*E*)

module D = DGROG.Make(V)(E)(BuiltInWithoutParsers.B.Integer)

let copy_without_code s =
  let return = D.Skeleton.create () in
  let () = D.Skeleton.iter_edges_e
    (fun e ->
      let e = D.Skeleton.E.(create (src e) E.(remove_code (label e)) (dst e)) in
      D.Skeleton.add_edge_e return e) s in
  return

let remove_code s =
  D.Skeleton.iter_edges_e
    (fun e ->
      let e' = D.Skeleton.E.(create (src e) E.(remove_code (label e)) (dst e)) in
      D.Skeleton.(remove_edge_e s e ; add_edge_e s e')) s

(*dot output begins*)

  let htmlization s =
    let s = Str.global_replace (Str.regexp_string "à") "&agrave;" s in
    let s = Str.global_replace (Str.regexp_string "è") "&egrave;" s in
    let s = Str.global_replace (Str.regexp_string "é") "&eacute;" s in
    let s = Str.global_replace (Str.regexp_string "ä") "&auml;" s in
    let s = Str.global_replace (Str.regexp_string "ë") "&euml;" s in
    let s = Str.global_replace (Str.regexp_string "ï") "&iuml;" s in
    let s = Str.global_replace (Str.regexp_string "ö") "&ouml;" s in
    let s = Str.global_replace (Str.regexp_string "ü") "&uuml;" s in
    let s = Str.global_replace (Str.regexp_string "â") "&acirc;" s in
    let s = Str.global_replace (Str.regexp_string "ê") "&ecirc;" s in
    let s = Str.global_replace (Str.regexp_string "î") "&icirc;" s in
    let s = Str.global_replace (Str.regexp_string "ô") "&ocirc;" s in
    let s = Str.global_replace (Str.regexp_string "û") "&ucirc;" s in
    let s = Str.global_replace (Str.regexp_string "À") "&Agrave;" s in
    let s = Str.global_replace (Str.regexp_string "È") "&Egrave;" s in
    let s = Str.global_replace (Str.regexp_string "É") "&Ecute;" s in
    let s = Str.global_replace (Str.regexp_string "Ä") "&Auml;" s in
    let s = Str.global_replace (Str.regexp_string "Ë") "&Euml;" s in
    let s = Str.global_replace (Str.regexp_string "Ï") "&Iuml;" s in
    let s = Str.global_replace (Str.regexp_string "Ö") "&Ouml;" s in
    let s = Str.global_replace (Str.regexp_string "Ü") "&Uuml;" s in
    let s = Str.global_replace (Str.regexp_string "Â") "&Acirc;" s in
    let s = Str.global_replace (Str.regexp_string "Ê") "&Ecirc;" s in
    let s = Str.global_replace (Str.regexp_string "Î") "&Icirc;" s in
    let s = Str.global_replace (Str.regexp_string "Ô") "&Ocirc;" s in
    let s = Str.global_replace (Str.regexp_string "Û") "&Ucirc;" s in
    let s = Str.global_replace (Str.regexp_string "Č") "&Chachek;" s in
    let s = Str.global_replace (Str.regexp_string "ç") "&ccedil;" s in
    s

  (*Wait until the Enter key is pressed*)
  let wait b = if b then ignore (read_line ())

  let month n = match n with
    | 0 -> "janvier"
    | 1 -> "f&eacute;vrier"
    | 2 -> "mars"
    | 3 -> "avril"
    | 4 -> "mai"
    | 5 -> "juin"
    | 6 -> "juillet"
    | 7 -> "ao&ucirc;t"
    | 8 -> "septembre"
    | 9 -> "octobre"
    | 10 -> "novembre"
    | 11 -> "d&eacute;cembre"
    | _ -> invalid_arg "month"

  let picture_skeleton ?(label="") g =
    let (filename,channel) = Filename.open_temp_file ~mode:[Open_binary] "automaton-" ".dot" in
    let tm = Unix.(localtime (time ())) in
    let sn = Unix.(Printf.sprintf "%sgénéré le %i %s %i à %i:%02i:%02i" label tm.tm_mday (month tm.tm_mon) (tm.tm_year + 1900) tm.tm_hour tm.tm_min tm.tm_sec) in
    let sn = htmlization sn in
    let () = Printf.fprintf channel
  "digraph automaton {
  label = %S ;
  rankdir=LR;
  size=\"8,5\";
  node [shape = doublecircle];" sn ;
        output_string channel ";\nnode [shape = circle];" ;
        D.Skeleton.iter_edges_e
          (fun e -> (Printf.fprintf channel "%i -> %i [ label = %i ] ;\n"
            (V.id (D.Skeleton.E.src e))
            (V.id (D.Skeleton.E.dst e))
            (E.id (D.Skeleton.E.label e))
            )
            ) g in
    let () = output_string channel "}\n" in
    let () = close_out channel in
    let command = Printf.sprintf "%s %s 2>/dev/null &" "dotaspdf " filename in (*A command that displays dot graphs*)
    let os_return = Sys.command command in
    if os_return <> 0 then failwith ("unable to execute "^command) ;
    wait true

(*dot output ends*)


module C = CPODGROG.Make(D)

let cfg2area cfg =
	let answer = D.of_mutex_section cfg in (*provide the field "vertices"*)
	let answer = C.Brick.of_list [answer] in (*turn the 1-dimension space into a brick*)
	let answer = C.Area.of_brick answer (*turn the brick into an area*)
	in answer

let null () = cfg2area (D.G.create ())

(* Read the instructions of the process linearly then draw the
   lists of arrows of the graph. Gather the arrows of the list to
   make the (directed) realization of the control flow graph. *)

(* "il" stands for "instruction list" *)

(* The instructions are loaded in a pipe until some call (C) or
   branching (ITE) instruction is met. The pipe of instruction is then
   turned into a label for the arrow to be created. *)

let make_arrow arrow_id content add_end src tgt pipe =
  let code = Array.of_list (List.rev pipe) in
  (src,{E.id=arrow_id;E.code=code},content,tgt,add_end)

(* The function "make" returns a list of arrows which are gathered by
   the function "D.draw_edges_list" *)

(* The function "make" performs a forward exploration of the
   program *)

(*

  Si on a une instruction simple on passe à la suite en transmettant
  tous les paramètres que l'on a reçu.

  Si l'on tombe sur un appel – C(name) – on vérifie que name n'a pas
  déjà été appelé, si c'est la cas on se branche au sommet
  correspondant, sinon on stocke name dans la liste des processus déjà
  appelés. Plus précisément on stocke un sommet du cfg qui correspond
  à un point d'entrée.

  Idée de l'algorithme: on descend dans les branches de l'arbre de
  décision (n-aire) du programme.

  Dans une seconde passe on supprime tous les sommets qui n'ont qu'un
  seul prédecesseur et un seul descendant. On garde cependant le point
  d'entrée du processus. Cette heuristique ne correspond pas à la
  catégorie des composantes bien qu'elle s'en approche dans l'esprit.

*)

(* The map already_called binds names of processes which have been
   called to vertices of the graph. *)

let make_bifurcation bl = Array.of_list
  (List.map (fun (precond,_) -> (precond,E.fresh_identifier ())) bl)

(*Sémantique de la fonction reduce: supprime tous les sommets qui ont une unique flèche entrante et une unique flèche sortante
en effectuant une concaténation*)

let rec reduce cfg =
	let edges = D.edges cfg in
	let an_expandable_vertex = ref None in
	let () =
		try
			D.G.iter_vertex
				(
					fun v -> (*for all vertices v*)
						match D.G.succ_e edges v with
						| [_,the_successor_bundle,y] ->
							((*one checks v has a unique successor*)
							if D.Enriched.E.contains_a_single_arrow the_successor_bundle
							then
								match D.G.pred_e edges v with
									| [x,the_predecessor_bundle,_] -> (*one checks v has a unique predecessor*)
										if D.Enriched.E.contains_a_single_arrow the_predecessor_bundle
											&& v.V.id <> 0 (*The vertex with identifier 0 is the entry point, is cannot be removed*)
										then (*OCaml version ⩾ 3.12.0 required*)
											let {E.id = id ; E.code = pred} , _ = D.Enriched.E.choose the_predecessor_bundle in
											let {E.id = _ ; E.code = succ} , _ = D.Enriched.E.choose the_successor_bundle in
											let m = D.Enriched.E.singleton {E.id=id;E.code=Array.append pred succ} D.Sh.full in
											an_expandable_vertex := Some (x,v,y,m);
											raise Exit
									| _ -> () (*more than one preceding vertex, v should not be removed*)
							)
						| _ -> () (*more than one succeeding vertex, v should not be removed*)
				)
			edges
		with Exit -> () (*one finds an expandable vertex...*)
	in (*...which is then actually removed*)
	match !an_expandable_vertex with
		| Some (x,v,y,m) -> (*removal of vertex v*) (*some merge has to be made*)
        let f key b c = match (b,c) with
          | Some x, None
          | None, Some x -> Some x
          | None, None -> None
          | Some x,Some y -> if x = y then Some x else failwith "reduce [PV2CFG]" in
        let m = try
          let _,already_there,_ = D.G.find_edge edges x y in
          let () = D.G.remove_edge_e edges (x,already_there,y)
          in D.Enriched.E.merge f already_there m
        with | Not_found -> m in
				D.G.remove_vertex edges v; D.erase_vertex cfg v;
				D.G.add_edge_e edges (x,m,y);
				reduce cfg (*reduce alters cfg: imperative*)
		| None -> () (*Terminate*)

(*Returns the unique edge of cfg arriving at v – if such an edge exists. Otherwise – that is if there is no or more than one
such edge, None is returned*)

let unique_ingoing cfg v =
  try
    D.Skeleton.fold_pred_e
      (fun e a ->
        match a with
          | None -> Some e
          | Some _ -> raise Exit)
      cfg v None
  with Exit -> None

(*Returns the unique edge of cfg starting from v – if such an edge exists. Otherwise – that is if there is no or more than one
such edge, None is returned*)

let unique_outgoing cfg v =
  try
    D.Skeleton.fold_succ_e
      (fun e a ->
        match a with
          | None -> Some e
          | Some _ -> raise Exit)
      cfg v None
  with Exit -> None

(*The function reduce_skeleton still have to be updated*)

let display_cfg cfg = print_endline (D.Skeleton.string_of ~shift:2 cfg)

let rec reduce_skeleton cfg =
  let keep_on_reducing = ref true in
  let in_e = ref None in
  let out_e = ref None in
  let expandable_vertex = ref None in
  while !keep_on_reducing do
    try
      D.Skeleton.iter_vertex
        (fun v ->
          if v.V.id <> 0
          then (
            in_e := unique_ingoing cfg v ;
            if Common.bool_of_option !in_e
            then (
              out_e := unique_outgoing cfg v ;
              if Common.bool_of_option !out_e
              then (expandable_vertex := Some v ; raise Exit)
              else in_e := None)))
        cfg ;
      keep_on_reducing := false
    with Exit -> (
      let (x , {E.id = id ; E.code = prefix_code } , _) = Common.get_some !in_e in
      let (_ , {E.id = _ ; E.code = postfix_code } , y) = Common.get_some !out_e in
      let v = Common.get_some !expandable_vertex in
      D.Skeleton.remove_vertex cfg v ;
      D.Skeleton.add_edge_e cfg (x , {E.id = id ; E.code = Array.append prefix_code postfix_code} , y) ;
      in_e := None ; out_e := None ; expandable_vertex := None)
  done

(* –––––––––––––––––––––––––––––––––––––––––––––––––––––––– *)
(* the following functions are imported from BuzySection.ml *)
(* –––––––––––––––––––––––––––––––––––––––––––––––––––––––– *)

module HL = D.HL
module Ci = D.Ci
module Sh = D.Sh

module ValueForBusySections = struct

  type t = Held of Sh.t | Level of Sh.t array

  let compare_value_for_busy_section k1 k2 =
    match k1 with
      | Held x1 ->
        (match k2 with
          | Held x2 -> D.Sh.compare x1 x2
          | _ -> -1)
      | Level lx1 ->
        (match k2 with
          | Level lx2 ->
            (let delta = ref 0 in
            let n1 = Array.length lx1 in
            let n2 = Array.length lx2 in
            try
              for i = 0 to pred (min n1 n2) do
                delta := D.Sh.compare lx1.(i) lx2.(i) ;
                if !delta <> 0 then raise Exit
              done ;
              Pervasives.compare n1 n2
            with Exit -> !delta)
          | _ -> 1)

  let update key value refmap = refmap := AbstractSyntax.Mod.add key value !refmap

  let get_Held ?(default=Sh.empty) key map =
    try
      (match AbstractSyntax.Mod.find key map with
        | Held x -> x
        | _ -> failwith "get_Held")
    with Not_found -> default

  let get_Level ?(default=Sh.empty) n key map =
    if n >= 0
    then
      try
        (match AbstractSyntax.Mod.find key map with
          | Level ax -> if n < Array.length ax then ax.(n) else default
          | _ -> failwith "get_Level: unexpected case")
      with Not_found -> default
    else failwith "get_Level: nonpositive level"

  let set_Level n key x refmap =
    if n >= 0
    then
      try
        match AbstractSyntax.Mod.find key !refmap with
        | Level ax -> (
            let d = n - (Array.length ax) in
            if d < 0
            then ax.(n) <- x
            else update key (Level (Array.append ax (Array.init (succ d) (fun i -> if i = d then x else Sh.empty)))) refmap)
        | _ -> failwith "set_Level: unexpected case"
      with Not_found -> update key (Level (Array.init (succ n) (fun i -> if i = n then x else Sh.empty))) refmap
    else failwith "set_Level: nonpositive level"

  let set_Held key x refmap =
    try
      match AbstractSyntax.Mod.find key !refmap with
      | Held _ -> update key (Held x) refmap
      | _ -> failwith "set_Held"
    with Not_found -> update key (Held x) refmap

  let get_bool ?(default=false) key map =
    match AbstractSyntax.Mod.find key map with
    | AbstractSyntax.Bc b -> b
    | AbstractSyntax.Ac _ -> failwith "What the hell this arithmetical value is doing here!"
    | _ -> failwith "get_bool"

  let set_bool key b refmap = update key (AbstractSyntax.Bc b) refmap

  let get_int ?(default=0) key map =
    match AbstractSyntax.Mod.find key map with
    | AbstractSyntax.Ac n -> n
    | AbstractSyntax.Bc _ -> failwith "What the hell this boolean value is doing here!"
    | _ -> failwith "get_int"

  let set_int key n refmap = update key (AbstractSyntax.Ac n) refmap

  let mark_final_section ?lvl key position bs =
    let final = Sh.final true position in
    let initial =
      try
        (match lvl with
          | None     -> get_Held      key !bs
          | Some lvl -> get_Level lvl key !bs)
      with Not_found -> Sh.empty in
    let updated = Sh.union initial final in
    match lvl with
      | None     -> set_Held      key updated bs
      | Some lvl -> set_Level lvl key updated bs

  let unmark_final_section ?lvl key position bs =
    let final = Sh.final true position in
    let initial =
      try
        (match lvl with
          | None     -> get_Held      key !bs
          | Some lvl -> get_Level lvl key !bs)
      with Not_found -> Sh.empty in
    let updated = Sh.difference initial final in
    match lvl with
      | None     -> set_Held      key updated bs
      | Some lvl -> set_Level lvl key updated bs

  let mark_puncture key position bs =
    let puncture = Sh.atom position in
    let initial =
      try get_Held key !bs
      with Not_found -> Sh.empty in
    let updated = Sh.union initial puncture in
    set_Held key updated bs

end (*ValueForBusySections*)

module VFBS = ValueForBusySections

(*Summarizing the behaviour of mutex according to the POSIX norm. The question mark means the case is unspecified by the POSIX
norm and is interpreted by ALCOOL as "do nothing".

–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
| MUTEX TYPE | LOCK              | UNLOCK                 |
–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
| DEFAULT    | if unlocked       | if locked              |
|            | then locked       | then unlocked          |
|            | else ?            | else ?                 |
–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
| NORMAL     | if unlocked       | if locked              |
|            | then locked       | then unlocked          |
|            | else deadlock     | else ?                 |
–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
| RECURSIVE  | incr lock_counter | if lock_counter > 0    |
|            |                   | then decr lock_counter |
|            |                   | else error             |
–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
| ERRORCHECK | if unlocked       | if locked              |
|            | then locked       | then unlocked          |
|            | else error        | else error             |
–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

Summarizing the behaviour of semaphore. The question mark means the case is unspecified by the POSIX norm and is interpreted
by ALCOOL as "do nothing".

–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
| SEMAPHORE TYPE | LOCK                        | UNLOCK                 |
–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
| DEFAULT        | if unlocked                 | if locked              |
|                | then locked                 | then unlocked          |
|                | else ?                      | else ?                 |
–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
| QUANTITATIVE   | if lock_counter < arity - 1 | if lock_counter > 0    |
|                | then incr lock_counter      | then decr lock_counter |
|                | else deadlock               | else error             |
–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

NB: A DEFAULT mutex is a DEFAULT semaphore of arity 2*)

(*The following function implements the action of the sequence of instructions, here denoted by al, on the state of the system.
This function is to be used with the function markov.*)

(*For each arrow and each resource there is an associated busy section. In term of data type we have a map whose keys are
resources and values are of type Sh.t – that is subset of the halfline. Each instruction carried by the arrow affects one of
the binding of the map. The exception Exit is raised when an instruction cannot be performed for a lack of resources*)

(*

  al: sequence of instructions

  initial: AbstractSyntax.AbstractSyntax.Mod.t

  semantics: AbstractSyntax.t

*)

let initialize_busy_section initial bs =
  let f key section =
    match key with
      | (_, AbstractSyntax.Mutex `Recursive) | (_,AbstractSyntax.Semaphore (`Quantitative, _)) ->
        let current_lvl = VFBS.get_int key initial in
        for lvl = 0 to current_lvl do VFBS.mark_final_section ~lvl key 0 bs done
      | (_, AbstractSyntax.Mutex _) | (_, AbstractSyntax.Semaphore (`Default, _)) -> if VFBS.get_bool key initial then VFBS.mark_final_section key 0 bs
      | _ -> () in
  AbstractSyntax.Mod.iter f initial

exception Stopped_at of V.end_kind * int * (VFBS.t AbstractSyntax.Mod.t) * (AbstractSyntax.expression AbstractSyntax.Mod.t)

let action_of_sequence_of_instructions initial semantics al =
  let open VFBS in
  let busy_section = ref AbstractSyntax.Mod.empty in
  let p = ref 0 in (*instruction pointer*)
  let distribution = ref initial in
  let () = initialize_busy_section initial busy_section in
  let f instruction = incr p ; match instruction with
    | AbstractSyntax.P x , _ ->
        (try
          (match snd x with
            | AbstractSyntax.Mutex `Normal ->
                if try get_bool x !distribution with Not_found -> false
                then raise (Stopped_at (V.Deadlock instruction , pred !p , !busy_section , !distribution))
                else (set_bool x true distribution ; mark_final_section x !p busy_section)
            | AbstractSyntax.Mutex `Recursive ->
                (let updated = try succ (get_int x !distribution) with Not_found -> 1 in
                  set_int x updated distribution ;
                  mark_final_section ~lvl:updated x !p busy_section)
            | AbstractSyntax.Mutex `Error_check ->
                if try get_bool x !distribution with Not_found -> false
                then raise (Stopped_at (V.Error instruction , pred !p , !busy_section , !distribution))
                else (set_bool x true distribution ; mark_final_section x !p busy_section)
            | AbstractSyntax.Mutex `Default
            | AbstractSyntax.Channel _
            | AbstractSyntax.Semaphore (`Default , _) -> (* behaves like Mutex `Default *)
                if try not (get_bool x !distribution) with Not_found -> true
                then (set_bool x true distribution ; mark_final_section x !p busy_section)
            | AbstractSyntax.Semaphore (`Quantitative , arity) -> (*done*)
                (let updated = try succ (get_int x !distribution) with Not_found -> 1 in
                 let () =
                    set_int x updated distribution ; mark_final_section ~lvl:updated x !p busy_section in
                  if updated = arity then raise (Stopped_at (V.Deadlock instruction , pred !p , !busy_section , !distribution)))
            | _ -> failwith "Warning: this message should never appear [action_of_sequence_of_instructions] – 1"
            (*This case cannot been matched since mutex_kind return the kind of the mutex x is it appears in the
              specification, and reaises Not_found otherwise*))
         with Not_found -> failwith (Printf.sprintf "The resource %S is either undeclared or not a mutex nor a semaphore nor a channel [P]" (fst x)))

    | AbstractSyntax.V x , _ ->
        (try
          (match snd x with
            | AbstractSyntax.Mutex `Recursive ->
                (
                  let updated = try pred (get_int x !distribution) with Not_found -> -1 in
                  if updated < 0 then raise (Stopped_at (V.Error instruction , pred !p , !busy_section , !distribution)) ;
                  set_int x updated distribution ;
                  unmark_final_section ~lvl:(succ updated) x !p busy_section
                )
            | AbstractSyntax.Mutex `Error_check ->
                if try get_bool x !distribution with Not_found -> false
                then (set_bool x false distribution ; unmark_final_section x !p busy_section)
                else raise (Stopped_at (V.Error instruction , pred !p , !busy_section , !distribution))
            | AbstractSyntax.Mutex _
            | AbstractSyntax.Channel _
            | AbstractSyntax.Semaphore (`Default , _) -> (*behaves like Mutex `Default*)
                if try get_bool x !distribution with Not_found -> false
                then (set_bool x false distribution ; unmark_final_section x !p busy_section)
            | AbstractSyntax.Semaphore (`Quantitative , arity) ->
                (let updated = try pred (get_int x !distribution) with Not_found -> -1 in
                  if updated < 0 then raise (Stopped_at (V.Error instruction ,  pred !p , !busy_section , !distribution)) ;
                  set_int x updated distribution ;
                  unmark_final_section ~lvl:(succ updated) x !p busy_section)
            (*This case cannot been matched since mutex_kind return the kind of the mutex x is it appears in the
            specification, and reaises Not_found otherwise*)
            | _ -> failwith "Warning: this message should never appear [action_of_sequence_of_instructions] – 4"
          )
         with Not_found -> failwith (Printf.sprintf "The resource %S is either undeclared or not a mutex nor a semaphore nor a channel [P]" (fst x)) )

    | AbstractSyntax.W x , _ ->
        (try mark_puncture x !p busy_section
         with Not_found -> failwith (Printf.sprintf "The resource %S is either undeclared or not a synchronization" (fst x) ))

    | AbstractSyntax.T (var, expr) , _ ->
        (mark_puncture (var, AbstractSyntax.Variable `Write) !p busy_section ;
        Common.StringSet.iter (fun var -> mark_puncture (var,AbstractSyntax.Variable `Read) !p busy_section) (AbstractSyntax.all_variables_occurring_in_expression expr))

    | AbstractSyntax.R (_, var) , _ -> mark_puncture (var, AbstractSyntax.Variable `Write) !p busy_section

    | AbstractSyntax.M (x , _) , _

    | AbstractSyntax.N x , _

    | AbstractSyntax.A x , _ -> mark_puncture x !p busy_section

    (*instructions that do not affect the busy section are ignored*)

    | _ -> () in (*end of definition of the function f*)

  (Array.iter f al ; (!busy_section , !distribution))

(* ––––––––––––––––––––––––––––––––––––––––––––––––––––––––– *)

type value_for_busy_section(*_on_graphs*) = Held of D.t | Level of D.t array

let is_empty_vfbs vbfs =
  match vbfs with
  | Held x -> D.is_empty x
  | Level ax -> Array.length ax = 1 && D.is_full ax.(0)

let display_value_for_busy_section ?(shift=0) vfbs =
  match vfbs with
  | Held value -> (Printf.printf "Held\n%s\n" (D.string_of value))
  | Level array_of_values -> (Array.iteri
    (fun i value -> Printf.printf "Level %i\n%s\n" i (D.string_of value) ) array_of_values)

(* ––––––––––––––––––––––––––––––––––––––––––––––––––––––––– *)

module Header = struct

  let underline s = Common.Terminal.deco ~underline:true s

  let distribution_at_point = "  " ^ "Distribution at point"

  let process = underline "Process:"

  let cfg = " " ^ underline "Control Flow Graph"

  let distribution = " " ^ underline "Occupancy Distribution"

  let busy_sections = " " ^ underline "Busy Sections"

end(*Header*)

let display_distribution dist =
  MoV.iter
    (fun v dist_at_v ->
      Printf.printf "%s%s\n" Header.distribution_at_point (V.detailed_string_of v) ;
      AbstractSyntax.Mod.iter
        (fun res value ->
          Printf.printf "   %s = %s\n" (AbstractSyntax.string_of_resource res) (AbstractSyntax.string_of_expression value) )
        (MoV.find v dist)
      )
  dist

let display_busy_sections busy_sections =
  AbstractSyntax.Mod.iter
    (fun res value ->
      match value with
        | Held value -> Printf.printf "   %s = %s\n" (AbstractSyntax.string_of_resource res) (D.string_of value)
        | Level array_of_values -> (
            Printf.printf "   %s\n    locus where occuppancy is\n" (AbstractSyntax.string_of_resource res) ;
            Array.iteri (fun i value -> Printf.printf "      ⩾  %i = %s\n"  i (D.string_of value) ) array_of_values))
    busy_sections

let display_busy_sections_from_selected_mutex selection busy_sections =
  AbstractSyntax.Mod.iter
    (fun res value -> if AbstractSyntax.is_mutex (snd res) && List.mem (fst res) selection then
      match value with
        | Held value -> Printf.printf "   %s = %s\n" (AbstractSyntax.string_of_resource res) (D.string_of value)
        | Level array_of_values -> (
            Printf.printf "   %s\n    locus where occuppancy is\n" (AbstractSyntax.string_of_resource res) ;
            Array.iteri (fun i value -> Printf.printf "      ⩾  %i = %s\n"  i (D.string_of value) ) array_of_values))
    busy_sections

let display_busy_sections_from_selected_semaphores selection busy_sections =
  AbstractSyntax.Mod.iter
    (fun res value -> if AbstractSyntax.is_semaphore (snd res) && List.mem (fst res) selection then
      match value with
        | Held value -> Printf.printf "   %s = %s\n" (AbstractSyntax.string_of_resource res) (D.string_of value)
        | Level array_of_values -> (
            Printf.printf "   %s\n    locus where occuppancy is\n" (AbstractSyntax.string_of_resource res) ;
            Array.iteri (fun i value -> Printf.printf "      ⩾  %i = %s\n"  i (D.string_of value) ) array_of_values))
    busy_sections

let display_forks cfg =
  D.Skeleton.iter_vertex
    (fun v ->
        let bif = V.strings_of_bifurcation v in
          if bif <> [||]
          then
            Printf.printf "  Forks from |%i|\n%s"
            v.V.id
            (Array.fold_left (fun accu s -> (accu^"   "^s^"\n")) "" bif))
    cfg

let display_debug name cfg distribution busy_sections = print_endline "XXXXXXX" ;
  let name = Common.Terminal.rgb 5 2 0 name in
  Printf.printf "%s %s\n" Header.process name;
  print_endline Header.cfg ;
  print_endline (D.Skeleton.string_of ~shift:2 cfg) ;
  display_forks cfg ;
  print_endline Header.distribution ;
  display_distribution distribution ;
  print_endline Header.busy_sections ;
  E.active_string_of := false ; display_busy_sections busy_sections ; E.active_string_of := true ;
  print_endline ""

(* ––––––––––––––––––––––––––––––––––––––––––––––––––––––––– *)

let compare_value_for_busy_section k1 k2 =
  match k1 with
    | Held x1 -> (
        match k2 with
          | Held x2 -> D.compare x1 x2
          | _ -> -1)
    | Level lx1 -> (
        match k2 with
          | Level lx2 -> (
              let delta = ref 0 in
              let n1 = Array.length lx1 in
              let n2 = Array.length lx2 in
              try
                for i = 0 to pred (min n1 n2) do
                  delta := D.compare lx1.(i) lx2.(i) ;
                  if !delta <> 0 then raise Exit
                done ;
                Pervasives.compare n1 n2
              with Exit -> !delta)
          | _ -> 1)

let update key value refmap = refmap := AbstractSyntax.Mod.add key value !refmap

let get_Held ?(default=D.create ()) key map =
  try
    (match AbstractSyntax.Mod.find key map with
      | Held x -> x
      | _ -> failwith "get_Held")
  with Not_found -> default

let get_Level ?(default=D.create ()) n key map =
  try
    (match AbstractSyntax.Mod.find key map with
      | Level ax -> if n < Array.length ax then ax.(n) else default
      | _ -> failwith "get_Level")
  with Not_found -> default

let set_Level n key x refmap =
  try
    match AbstractSyntax.Mod.find key !refmap with
    | Level ax -> (
        let d = n - (Array.length ax) in
        if d < 0
        then ax.(n) <- x
        else update key (Level (Array.append ax (Array.make (succ d) x))) refmap)
    | _ -> failwith "set_Level"
  with Not_found -> update key (Level (Array.make (succ n) x)) refmap

let set_Held key x refmap =
  try
    match AbstractSyntax.Mod.find key !refmap with
    | Held x -> update key (Held x) refmap
    | _ -> failwith "set_Held"
  with Not_found -> update key (Held x) refmap

let get_bool ?(default=false) key map =
  match AbstractSyntax.Mod.find key map with
  | AbstractSyntax.Bc b -> b
  | _ -> failwith "get_bool"

let set_bool key b refmap = update key (AbstractSyntax.Bc b) refmap

let get_int ?(default=0) key map =
  match AbstractSyntax.Mod.find key map with
  | AbstractSyntax.Ac n -> n
  | _ -> failwith "get_int"

let set_int key n refmap = update key (AbstractSyntax.Ac n) refmap

(* ––––––––––––––––––––––––––––––––––––––––––––––––––––––––– *)

let update_n_check semantics name cfg distribution busy_sections arrow_id source destination il  =
  let check key a b =
    match a with
    | Some a' ->
      (match b with
        | Some b' ->
            if a' = b'
            then a
            else failwith (Printf.sprintf "process %S does not satify the Markov property – due to key %S" name (fst key))
        | None -> a)
    | None -> b in
  let initial =
    try MoV.find source !distribution
    with Not_found -> AbstractSyntax.Mod.empty in
  let destination = ref destination in
  let dist_at_destination_before_actions = ref (
    try MoV.find !destination !distribution
    with Not_found -> AbstractSyntax.Mod.empty) in
  let code = ref (Array.of_list (List.rev il)) in
  let bs_to_destination , dist_at_destination_after_actions =
    try action_of_sequence_of_instructions initial semantics !code (* sequence_of_instructions *)
    with Stopped_at (koep , p , bs , dist) ->
      (
        destination := { V.id = V.fresh_identifier () ; V.entrypoint = None ; V.bifurcation = [||] ; endpoint = Some koep} ;
        dist_at_destination_before_actions := AbstractSyntax.Mod.empty ;
        code := Array.sub !code 0 p ;
        bs , dist) in
  (*check whether it is a Markov process and update the distribution at point destination*)
  distribution := MoV.add
    !destination
    (AbstractSyntax.Mod.merge check dist_at_destination_after_actions !dist_at_destination_before_actions)
    !distribution ;
  (*update skeleton*)
  let edge = (D.Skeleton.E.create source {E.id = arrow_id ; E.code = !code} !destination) in
  D.Skeleton.add_edge_e cfg edge ;
  (*Each resource appearing among the collected instructions implies to update the corresponding entry of the
  association table gathering the busy sections*)
  AbstractSyntax.Mod.iter
    (fun res sec ->
      match sec with
        | VFBS.Held sec ->
            (let to_be_updated = get_Held res !busy_sections in
              let key = {E.id = arrow_id ; E.code = [||]} in (*the code is intentionally forgotten*)
              let () = D.draw to_be_updated source key sec !destination in (*to_be_updated was actually updated*)
              set_Held res to_be_updated busy_sections)
        | VFBS.Level sec_ar ->
            (let key = {E.id = arrow_id ; E.code = [||]} in (*the code is intentionally forgotten*)
              Array.iteri (*to_be_updated was actually updated*)
                (fun i sec ->
                  let default = D.create () in
                  let () = if i = 0 then D.draw ~add_end:true default source {E.id = arrow_id ; E.code = !code} Sh.full !destination in
                  let to_be_updated = get_Level ~default i res !busy_sections in
                  let () = D.draw to_be_updated source key sec !destination in
                  set_Level i res to_be_updated busy_sections)
                sec_ar))
    bs_to_destination

(*Collect all the resources declared in the program and fill the map according to the type*)

let initial_distribution_at_entrypoint semantics =
  List.fold_left
    (fun accu res -> match snd res with
      | AbstractSyntax.Mutex `Recursive | AbstractSyntax.Semaphore (`Quantitative,_) -> AbstractSyntax.Mod.add res (AbstractSyntax.Ac 0) accu
      | AbstractSyntax.Mutex _ | AbstractSyntax.Semaphore (`Default,_) -> AbstractSyntax.Mod.add res (AbstractSyntax.Bc false) accu
      | AbstractSyntax.Channel cv -> AbstractSyntax.Mod.add res (AbstractSyntax.Bc false) accu
(*
      | AbstractSyntax.Channel (`Fifo , _) -> AbstractSyntax.Mod.add (fst res , AbstractSyntax.Mutex `Fifo) (AbstractSyntax.Bc false) accu
      | AbstractSyntax.Channel (`Lifo , _) -> AbstractSyntax.Mod.add (fst res , AbstractSyntax.Mutex `Lifo) (AbstractSyntax.Bc false) accu
*)
      | _ -> accu)
    AbstractSyntax.Mod.empty semantics.AbstractSyntax.declarations

(* ––––––––––––––––––––––––––––––––––––––––––––––––––––––––– *)

(* The auxiliary function cfg_of_process should return the list of endpoints – these data are necessary to deal with branching
the proper way*)

(* –– Specification of the function cfg_of_process ––

   1 – returns the list of all endpoints as first component of the returned value
   2 – returns the cgf of the input process – whence the name
   3 – given a branching points, all the endpoints are gathered to a single one

*)

(*Hypothesis: The vertex whose id number is 0 is the entry point*)

(*"ep" stands for "end points", "al" stands for "arrow list"*)

(*Pour l'instant on se contente de calculer cfg, busy_section, et distribution. Il faut cependant continuer à renvoyer la même
valeur qu'avec l'ancienne fonction cfg afin de permettre la compilation. Je ferai les modif au dernier moment ... TODO*)

(*There always should be a single Regular endpoint*)

let dummy_position = (-1,-1)

let cfg ?(terminal_display=false) ?(dot_display=false) ?(flat_fork=true) name semantics =
  (*clean up internal references of module E and V – basically the identifier counters are set to 0*)
  let () = V.reset_identifier (); E.reset_identifier () in
  (*create a map associating each defined name with the corresponding body process*)
  let proc_dico = List.fold_left
    (fun accu (name,il) -> Common.StringMap.add name il accu)
    Common.StringMap.empty
    (AbstractSyntax.equations_converter semantics.AbstractSyntax.equations) in
  (*initial sequence of instructions from the process the control flow graph of which is to build*)
  let il = try Common.StringMap.find name proc_dico
    with Not_found -> failwith (Printf.sprintf "%s Undefined process %S" !Message.warning name) in
  (*entry point of the control flow graph*)
  let entrypoint = { V.id = V.fresh_identifier () ; V.entrypoint = Some name ; V.bifurcation = [||] ; endpoint = None } in
  (*the three components of the answer*)
  let cfg = D.Skeleton.create () in
  let busy_sections = ref AbstractSyntax.Mod.empty in
  let distribution = ref (MoV.singleton entrypoint (initial_distribution_at_entrypoint semantics)) in
  let rec cfg_of_process
    ?converging_vertex     (*converging vertex created by forks*)
    ?arrow_id              (*provide the identifier of the arrow to be created*)
    called_names           (*the processes which have already been called along the current branch*)
    collected_instructions (*the instructions which have already been collected along the current branch*)
    source                 (*the source of the current branch*)
    callbacks              (*the instructions to be executed once the current branch is terminated*)
    il                     (*instructions of the current branch*)
    =
    match il with
      (*The separation points/double points are just forgotten*)
      (*| (DT,_)::il | (DDT,_)::il -> cfg_of_process ?arrow_id*)
					(*called_names collected_instructions source callbacks il*)
      (*The instructions which do not alter the control flow are stacked on*)
      | ((AbstractSyntax.Nop,text_position) as instruction)::il
      | ((AbstractSyntax.P(_),text_position) as instruction)::il
      | ((AbstractSyntax.V(_),text_position) as instruction)::il
      | ((AbstractSyntax.M(_),text_position) as instruction)::il
      | ((AbstractSyntax.N(_),text_position) as instruction)::il
      | ((AbstractSyntax.A(_),text_position) as instruction)::il
      | ((AbstractSyntax.W(_),text_position) as instruction)::il
      | ((AbstractSyntax.E(_),text_position) as instruction)::il
      | ((AbstractSyntax.T(_),text_position) as instruction)::il
      | ((AbstractSyntax.D(_),text_position) as instruction)::il ->
	      let collected_instructions = instruction::collected_instructions in
					cfg_of_process ?converging_vertex ?arrow_id called_names collected_instructions source callbacks il
      | ((AbstractSyntax.S(x,m),(a,b)) as instruction)::il ->
	      let collected_instructions = (AbstractSyntax.V x,(b+1,b+1))::instruction::(AbstractSyntax.P x,(a,a))::collected_instructions in
		      cfg_of_process ?converging_vertex ?arrow_id called_names collected_instructions source callbacks il
      | ((AbstractSyntax.R(x,c),(a,b)) as instruction)::il ->
	      let to_waiting_vertex_identifier = match arrow_id with Some id -> id | None -> E.fresh_identifier () in
	      let waiting_loop_identifier = E.fresh_identifier () in
	      let arrow_id = E.fresh_identifier () in (*define the "keep going" arrow identifier*)
	      (*The instruction pointer turn around the directed loop until some message is available*)
        let bifurcation = [|AbstractSyntax.I_EmptyMessageBox (fst x) , waiting_loop_identifier ; AbstractSyntax.No (AbstractSyntax.I_EmptyMessageBox (fst x)) , arrow_id|] in
	      let waiting_vertex = { V.id = V.fresh_identifier () ; entrypoint = None ; V.bifurcation = bifurcation ; endpoint = None } in
				(*Create the arrow from source to the waiting point and the loop*)
				(*Keep in mind the list of instructions is "reversed"*)
        let instructions_to_waiting_vertex = (AbstractSyntax.P x,(a,a)) :: collected_instructions in (*instructions carried by the arrow to waiting vertex*)
        let () = update_n_check semantics name cfg distribution busy_sections
          to_waiting_vertex_identifier source waiting_vertex instructions_to_waiting_vertex in
        (*waiting loop – this cannot make a process non Markovian, nevertheless the test is performed*)
        let () = update_n_check semantics name cfg distribution busy_sections
          waiting_loop_identifier waiting_vertex waiting_vertex [(AbstractSyntax.P x,dummy_position);(AbstractSyntax.V x,dummy_position)] in
        let ep = cfg_of_process ?converging_vertex ~arrow_id called_names [(AbstractSyntax.V x,(b+1,b+1));instruction] waiting_vertex callbacks il in
        ep
      (*instructions below may create branchings*)
      | (AbstractSyntax.ITE(bl),text_position) :: il ->
        let bifurcation = make_bifurcation bl in
        (*forking_vertex might be useless if there are no collected instructions and flat forks are allowed*)
        let forking_vertex = { V.id = V.fresh_identifier () ; entrypoint = None ; V.bifurcation = bifurcation ; endpoint = None } in
        let converging_vertex =
          match converging_vertex with
            | Some v -> Printf.printf "converging_vertex = %s\n" (V.string_of v) ; v
            | None -> { V.id = V.fresh_identifier () ; entrypoint = None;V.bifurcation = [||] ; endpoint = None } in
        let to_forking_vertex_identifier = match arrow_id with Some id -> id | None -> E.fresh_identifier () in
	      if collected_instructions <> [] || not flat_fork
	      then (*some arrow from the source to the forking point has to be created*)
          let () = update_n_check semantics name cfg distribution busy_sections
            to_forking_vertex_identifier source forking_vertex collected_instructions in
          let ep = List.fold_left2
			      (fun accuep (_,arrow_id) (_,il) ->
              let ep = cfg_of_process ~converging_vertex ~arrow_id called_names [] forking_vertex [] il in
              let ep = List.append accuep ep in
              ep)
            [] (Array.to_list bifurcation) bl in
          let () = List.iter (fun x -> x.V.endpoint <- Some V.Branch) ep in
          let () = List.iter
            (fun v ->
              let arrow_id = E.fresh_identifier () in
              if D.Skeleton.mem_vertex cfg v && not (D.Skeleton.mem_edge cfg v converging_vertex)
              then update_n_check semantics name cfg distribution busy_sections
                arrow_id v converging_vertex []) ep in
		      if il <> []
		      then
			      let ep = cfg_of_process ~converging_vertex called_names [] converging_vertex callbacks il in
            ep
		      else (*no more instruction to deal with – il=[]*)
			      if callbacks <> []
			      then
				      let ep = cfg_of_process ~converging_vertex called_names [] converging_vertex (List.tl callbacks) (List.hd callbacks) in
              ep
				      (*Terminal case – neither instruction nor callback remaining*)
			      else (V.decr_identifier () ; ep)
        else (*collected_instructions = [] and flat fork are allowed – no need to create any arrow*)
          (
            match arrow_id with
              | Some arrow_id ->
                (*if some arrow identifier is provided, then the call come from a branching –either ITE or R– hence there is a
                precondition*)
                let source_bifurcation = source.V.bifurcation in
                let precondition_index = Common.Array.findi
                  (fun (_,identifier) -> identifier = arrow_id)
                  source_bifurcation in
                let source_precondition = fst source_bifurcation.(precondition_index) in
                (*Add the precondition to every branches of the fork to be graft*)
                let bifurcation = Array.map
                  (fun (current_precondition,id) ->
                    (AbstractSyntax.and_expression source_precondition current_precondition),id)
                  bifurcation in
                (*One grafts the new branches, the one indexed by precondition_index is split into several ones*)
                let () = source.V.bifurcation <- Array.concat
                  [
                    Array.sub source_bifurcation 0 precondition_index;
                    bifurcation;
                    let start = succ precondition_index in
                    let len = (Array.length source_bifurcation) - start in
                    Array.sub source_bifurcation start len
                  ] in (*treating the branches – recursion case*)
                let ep = List.fold_left2
                  (
                    fun accuep (_,arrow_id) (_,il) ->
                      let ep = cfg_of_process ~converging_vertex  ~arrow_id called_names [] source [] il in
                      let ep = List.append accuep ep
                      in ep
                  ) [] (Array.to_list bifurcation) bl in
                let () = List.iter
                    (fun v ->
                      let arrow_id = E.fresh_identifier () in
                        if D.Skeleton.mem_vertex cfg v && not (D.Skeleton.mem_edge cfg v converging_vertex)
                        then update_n_check semantics name cfg distribution busy_sections
                          arrow_id v converging_vertex [])
                    ep in
                if il <> []
                then
                  let ep = cfg_of_process ~converging_vertex ~arrow_id called_names [] converging_vertex callbacks il in
                  ep
                else (*prerequite: il=[] – no more instruction to deal with*)
                  if callbacks <> []
                  then
                    let ep = cfg_of_process ~converging_vertex called_names [] converging_vertex (List.tl callbacks) (List.hd callbacks) in
                    ep
                  else (V.decr_identifier () ; ep) (*Terminal case – neither instruction nor callback remaining*)
              | None ->
                  (*One grafts the new branches, the one indexed by precondition_index is split into several ones*)
                  let () = source.V.bifurcation <- bifurcation in
                  let ep = List.fold_left2
                    (
                      fun accuep (_,arrow_id) (_,il) ->
                      let ep = cfg_of_process ~converging_vertex ~arrow_id called_names [] source [] il in
                      let ep = List.append accuep ep in
                      ep
                    ) [] (Array.to_list bifurcation) bl in
                  let () = List.iter
                      (fun v ->
                        let arrow_id = E.fresh_identifier () in
                          if D.Skeleton.mem_vertex cfg v && not (D.Skeleton.mem_edge cfg v converging_vertex)
                          then update_n_check semantics name cfg distribution busy_sections
                            arrow_id v converging_vertex [])
                      ep in
                  if il <> []
                  then
                    let ep = cfg_of_process ~converging_vertex called_names [] converging_vertex callbacks il in
                    ep
                  else (*prerequite: il=[] – no more instruction to deal with*)
                    if callbacks <> []
                    then
                      let ep = cfg_of_process ~converging_vertex called_names [] converging_vertex (List.tl callbacks) (List.hd callbacks) in
                      ep
                    else (V.decr_identifier () ; ep) (*Terminal case – neither instruction nor callback remaining*)
          )
      | (AbstractSyntax.C(name),text_position) :: il -> ((*Call instruction. May raise the exception NonMarkovian*)
					let arrow_id = match arrow_id with Some id -> id | None -> E.fresh_identifier () in
			    try (*Have the process name already been called along the current branch ?*)
				    let v = Common.StringMap.find name called_names in
            let () = update_n_check
              semantics name
              cfg distribution busy_sections
              arrow_id source v collected_instructions in
				    (*Yes: a loop is created – terminal case without creating any endpoint, yet creates an arrow*)
				    []
				  with Not_found -> (*No: inlining*)
				    let il' =
					    try Common.StringMap.find name proc_dico
				      with Not_found -> failwith (Printf.sprintf "%s Undefined process %S\n" !Message.error name) in
				    (*The following vertex is the entry point of some call*)
				    let entrypoint = { V.id = V.fresh_identifier () ; V.entrypoint = Some name ; V.bifurcation = [||] ; endpoint = None } in
				    (*The endpoints are not returned*)
            let () = update_n_check semantics name cfg distribution busy_sections
              arrow_id source entrypoint collected_instructions in
            let ep = cfg_of_process (*?converging_vertex*) (Common.StringMap.add name entrypoint called_names) [] entrypoint (il::callbacks) il'
				    in ep)
      | ((AbstractSyntax.F(_),_))::il -> (
          let err_msg = Printf.sprintf "%s No dynamic creation of process allowed" !Message.warning in
          failwith err_msg)
      | [] -> (
          match callbacks with
            | il :: callbacks -> cfg_of_process
              called_names ?converging_vertex collected_instructions source callbacks il
            | [] -> (*Terminal case – a new endpoint may be created*)
                if collected_instructions <> []
                then
                  let terminal_vertex = { V.id = V.fresh_identifier () ; V.entrypoint = None ; V.bifurcation = [||] ; endpoint = Some V.Process } in
                  let arrow_id = match arrow_id with Some id -> id | None -> E.fresh_identifier () in
                  let () = update_n_check semantics name cfg distribution busy_sections
                    arrow_id source terminal_vertex collected_instructions in
                  [terminal_vertex] (*The newly created endpoint is returned*)
                else [source]) in
      (*end of the recursive function*)
  let endpoints =
    cfg_of_process
      (Common.StringMap.add name entrypoint Common.StringMap.empty) (*no process have been called yet – initialization*)
      [] (*no instruction have been collected yet – initialization*)
      entrypoint (*entry point of the program is provided*)
      [] (*no callbacks – initialization*)
      il in
  if terminal_display then display_debug name cfg !distribution !busy_sections ;
  if dot_display then picture_skeleton ~label:(Printf.sprintf "graphe de flot de contrôle du processus %s\n" name) cfg ;
  (cfg , !busy_sections , !distribution , endpoints)


let is_yoneda cfg src tgt =
  (
    match D.G.succ_e cfg tgt with
      | [_,the_successor_bundle,_] -> D.Enriched.E.contains_a_single_arrow the_successor_bundle
      | _ -> false
  ) &&
    (
      match D.G.pred_e cfg tgt with
				| [_,the_predecessor_bundle,_] -> D.Enriched.E.contains_a_single_arrow the_predecessor_bundle
				| _ -> false
    ) &&
    (
      try D.Enriched.E.contains_a_single_arrow (let (_,x,_) = D.G.find_edge cfg src tgt in x)
      with Not_found -> false
    )

(* Warning: cfg is altered by the following function *)

(* Warning: the specification of shrink is not clear *)

(* Raise Ivalid_argument if src or tgt does not belong to cfg *)

(* Specification: the arrows from living_v to dying_v are removed and
   the vertices living_v and dying_v are identififed *)

(* Warning: one must have living_v ≠ dying_v *)

(*to be tested*)

let shrink_to_source cfg living_v dying_v =
  if living_v = dying_v then (raise (Invalid_argument "shrink_to_source [PV2CFG]"))
  ; (*Remove the shrinked arrow. May raise Invalid_Argument*)
  D.G.remove_edge cfg living_v dying_v
  ; (*The case of a loop on dying_v is treated in advance*)
  (
    try let (_,save,_) = D.G.find_edge cfg dying_v dying_v in
	D.G.remove_edge cfg dying_v dying_v;
	D.G.add_edge_e cfg (living_v,save,living_v);
    with Not_found -> ()
  )
  ;
  D.G.iter_succ_e
    (
      fun (_,m,y) -> D.G.remove_edge cfg dying_v y;
	D.G.add_edge_e cfg (living_v,m,y)
    ) cfg dying_v
  ;
  D.G.iter_pred_e
    (
      fun (x,m,_) -> D.G.remove_edge cfg x dying_v;
	D.G.add_edge_e cfg (x,m,living_v)
    ) cfg dying_v

(*to be tested*)

let shrink_to_target cfg dying_v living_v =
  if living_v = dying_v then (raise (Invalid_argument "shrink_to_source [PV2CFG]"))
  ; (*Remove the shrinked arrow. May raise Invalid_Argument*)
  D.G.remove_edge cfg dying_v living_v
  ; (*The case of a loop on dying_v is treated in advance*)
  (
    try let (_,save,_) = D.G.find_edge cfg dying_v dying_v in
	D.G.remove_edge cfg dying_v dying_v;
	D.G.add_edge_e cfg (living_v,save,living_v);
    with Not_found -> ()
  )
  ;
  D.G.iter_succ_e
    (
      fun (_,m,y) -> D.G.remove_edge cfg dying_v y;
	D.G.add_edge_e cfg (living_v,m,y)
    ) cfg dying_v
  ;
  D.G.iter_pred_e
    (
      fun (x,m,_) -> D.G.remove_edge cfg x dying_v;
	D.G.add_edge_e cfg (x,m,living_v)
    ) cfg dying_v

(*A vertex with a single ingoing arrow γ and a single outgoing arrow δ
  can be removed by concatenating γ and δ. The transformation is
  performed on site by the function reduce.*)


let origin g = D.Skeleton.find_vertex (fun v -> (V.id v) = 0) g

(* End of file *)
