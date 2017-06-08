open Common

exception Undefined

exception PV_error of string

module Highlight =
struct

  let terminal_width = ref 120

  let center = ref
    (fun n s -> String.make (!terminal_width - String.length s + n / 2) ' ' ^ s)

  let parallel_separator = ref (fun () -> (String.make 7 '_')^"\n\n")
  let utf8_enabled = ref true
  let color_enabled = ref true
  let boolean = ref (fun b -> Terminal.rgb ~active:!color_enabled 0 5 5 b)
  let integer = ref (fun n -> Terminal.rgb ~active:!color_enabled 3 0 1 n)
  let variable = ref id
  let unknown = ref (fun x -> Terminal.rgb ~active:!color_enabled ~bold:true 1 0 0 x)
  let keyword = ref (fun kw -> Terminal.(color ~active:!color_enabled Blue ~bold:true kw))
  let constant = ref (fun cst -> Terminal.(color ~active:!color_enabled White cst))
  let error = ref (fun err -> Terminal.(color ~active:!color_enabled Red ~bold:true ~underline:true err))
  let paragraph = ref (fun parg -> Terminal.(color ~active:!color_enabled ~underline:true ~bold:true Green parg))
  let good_news = ref (fun parg -> Terminal.(color ~active:!color_enabled ~bold:true Green parg))
  let resource = ref (fun err -> Terminal.(color ~active:!color_enabled Green err))

end

type ex_type =
  | T_int
  | T_bool

let string_of_ex_type t =
  match t with
  | T_int -> "int"
  | T_bool -> "bool"

type expression =
  | Va of string (* Variable *)
  | Unknown      (* Dummy default value *)
  (* --------------------------------------------------- *)
  | Ac of int                 (* Arithmetic constant     *)
  | Ne of expression            (* Negative                *)
  | Ab of expression            (* Magnitude               *)
  | Pl of expression * expression (* Plus   - Addition       *)
  | Mi of expression * expression (* Minus  - Substraction   *)
  | Ti of expression * expression (* Times  - Multiplication *)
  | Di of expression * expression (* Divide - Division       *)
  | Mo of expression * expression (* Modulo - Remainder      *)
  (* --------------------------------------------------- *)
  | Bc of bool                (* Boolean constant *)
  | An of expression * expression (* And              *)
  | Or of expression * expression (* Or               *)
  | No of expression            (* Not              *)
  | Eq of expression * expression (* Equal            *)
  | Lq of expression * expression (* Less or Equal    *)
  | Ls of expression * expression (* Strictly Less    *)
  | Gq of expression * expression (* Greater or Equal *)
  | Gs of expression * expression (* Strictly Greater *)
  | Nq of expression * expression (* Not Equal        *)
  | I_EmptyMessageBox of string (* Internal instruction – not available to programer *)
  (* --------------------------------------------- *)
  | It of  expression * expression (* Interval – for statical analysis *)

let and_expression e1 e2 =
  match e1 with
    | Bc true -> e2
    | Bc false -> Bc false
    | _ -> (
      match e2 with
        | Bc true -> e1
        | Bc false -> Bc false
        | _ ->
            if e1 = No e2 || e2 = No e1
            then Bc false
            else  An (e1 , e2))

let int_of_expression ex =
  match ex with
  | Ac n -> n
  | _ -> failwith "int_of_expression"

let bool_of_expression ex =
  match ex with
  | Bc b -> b
  | _ -> failwith "bool_of_expression"

type mutex_var = [ `Error_check | `Normal | `Recursive | `Default ]

type channel_var = [ `Fifo | `Lifo ]

let string_of_mutex_var mv = match mv with
  | `Error_check -> "Error_check"
  | `Normal      -> "Normal"
  | `Recursive   -> "Recursive"
  | `Default     -> "Default"

let string_of_channel_var cv = match cv with
  | `Fifo        -> "Fifo"
  | `Lifo        -> "Lifo"

type semaphore_var = [ `Default | `Quantitative ]

let string_of_semaphore_var sv = match sv with
  | `Default      -> "Default"
  | `Quantitative -> "Quantitative"

type access_var = [ `Read | `Write ]

let string_of_access_var av = match av with
  | `Read  -> "Read"
  | `Write -> "Write"

type notify_var = [ `Some | `All ]

let string_of_notify_var av = match av with
  | `Some  -> "Some"
  | `All -> "All"

type resource_kind =
  | Mutex of mutex_var
  | Semaphore of (semaphore_var * int)
  | Synchronization of int
  | Channel of (channel_var * int)
  | Monitor
  | Notify of notify_var
  | Variable of access_var

let arity_of_kind k =
  match k with
  | Semaphore (_,arity)
  | Channel (_,arity)
  | Synchronization arity -> Some arity
  | _ -> None

let string_of_resource_kind rk = match rk with
  | Mutex mv -> Printf.sprintf "Mutex (%s)" (string_of_mutex_var mv)
  | Monitor -> "Monitor"
  | Semaphore (sv , arity) -> Printf.sprintf "Semaphore (%s,%i)" (string_of_semaphore_var sv) arity
  | Channel (cv,size) -> Printf.sprintf "Channel (%s %i)" (string_of_channel_var cv) size
  | Synchronization arity -> Printf.sprintf "Synchronization (%i)" arity
  | Variable av -> Printf.sprintf "Variable (%s)" (string_of_access_var av)
  | Notify nv -> Printf.sprintf "Notify (%s)" (string_of_notify_var nv)

let string_of_resource res = Printf.sprintf "%s%s"
  (Terminal.rgb 1 4 2 (fst res))
  (Terminal.grey 15 (":" ^ string_of_resource_kind (snd res)))

module Resource =
  struct
    type t = string * resource_kind
    let compare = Pervasives.compare
    let id = fst
    let kind = snd
  end

module Mod = Map.Make (Resource)

module Sod = Set.Make (Resource)

type instr =
  | P of Resource.t
  | V of Resource.t
  | S of Resource.t * string (*send 1st argument: name of the stack/queue 2nd argument: message*)
  | R of Resource.t * string (*receive 1st argument: name of the stack/queue 2nd argument: container*)
  | M of Resource.t * string (*monitoring*)
  | N of Resource.t (*One of the processes waiting for the signal may keep on going*)
  | A of Resource.t (*All processes blocked by the argument are released*)
  | F of string (*fork*)
  | W of Resource.t (*wait*)
  | C of string (*call*)
  | E of string (*?*)
  | D of ex_type * string (*variable declaration*)
  | T of string * expression (*assign*)
  | ITE of ((expression * instruction list) list) (*If Then Else – it is actually more like a match case*)
  | Nop (*The instruction that does nothing*)
and instruction =
    instr * pos
and pos = int * int

type aribo_kind = Arithmetic | Boolean | Identifier

let rec check_aribo = function
  | Va(s) -> Identifier
  | Ac(n) -> Arithmetic
  | Ne(a) ->
    let x = check_aribo a in
    if x <> Boolean then Arithmetic else raise Undefined
  | Pl(a1,a2)
  | Mi(a1,a2)
  | Ti(a1,a2)
  | Di(a1,a2) ->
    let x = check_aribo a1 and y =  check_aribo a2 in
    if x <> Boolean && y <> Boolean then Arithmetic else raise Undefined
  | Unknown -> Identifier
  | Bc(b) -> Boolean
  | No(b) ->
    let x = check_aribo b in
    if x <> Arithmetic then Boolean else raise Undefined
  | An(e1,e2)
  | Or(e1,e2)
  | Eq(e1,e2)
  | Nq(e1,e2) ->
    let z = (check_aribo e1,check_aribo e2) in
    (
      match z with
      | (Identifier,_)
      | (_,Identifier)
      | (Boolean,Boolean)
      | (Arithmetic,Arithmetic) -> Boolean
      | _ -> raise Undefined
    )
  | Lq(e1,e2) ->
    let x = check_aribo e1 and y = check_aribo e2 in
    if x <> Arithmetic && y <> Arithmetic then Boolean else raise Undefined
  | _ -> failwith "The interval notation is not taken into account yet"

let rec variable_occurs_in_expression s = function
  | Va(s') -> s = s'
  | Unknown
  | Ac(_)
  | Bc(_) -> false
  | No(e)
  | Ne(e) -> variable_occurs_in_expression s e
  | Pl(e1,e2)
  | Mi(e1,e2)
  | Ti(e1,e2)
  | Di(e1,e2)
  | An(e1,e2)
  | Or(e1,e2)
  | Eq(e1,e2)
  | Nq(e1,e2)
  | Lq(e1,e2) -> (variable_occurs_in_expression s e1) || (variable_occurs_in_expression s e2)
  | _ -> failwith "The interval notation is not taken into account yet"

let all_variables_occurring_in_expression e =
  let rec all_variables_occurring_in_expression accu e = match e with
    | Va(s) -> Common.StringSet.singleton s
    | Unknown
    | Ac(_)
    | Bc(_) -> StringSet.empty
    | No(e)
    | Ne(e) -> all_variables_occurring_in_expression accu e
    | Pl(e1,e2)
    | Mi(e1,e2)
    | Ti(e1,e2)
    | Di(e1,e2)
    | An(e1,e2)
    | Or(e1,e2)
    | Eq(e1,e2)
    | Nq(e1,e2)
    | Lq(e1,e2) ->
      let aux1 = all_variables_occurring_in_expression accu e1 in
      let aux2 = all_variables_occurring_in_expression accu e2 in
      StringSet.union aux1 aux2
    | _ -> failwith "The interval notation is not taken into account yet" in
  all_variables_occurring_in_expression StringSet.empty e

let rec string_of_expression =
  let open Highlight in
  function
  | Va(s) -> !variable s
  | Ac(n) -> !integer (string_of_int n)
  | Ne(a) -> (!integer "(")^(!integer "-")^(string_of_expression a)^(!integer ")")
  | Ab(n) -> (!integer "abs(")^(string_of_expression n)^(!integer ")")
  | Pl(a1,a2) -> (!integer "(")^(string_of_expression a1)^(!integer "+")^(string_of_expression a2)^(!integer ")")
  | Mi(a1,a2) -> (!integer "(")^(string_of_expression a1)^(!integer "-")^(string_of_expression a2)^(!integer ")")
  | Ti(a1,a2) -> (!integer "(")^(string_of_expression a1)^(!integer "*")^(string_of_expression a2)^(!integer ")")
  | Di(a1,a2) -> (!integer "(")^(string_of_expression a1)^(!integer "/")^(string_of_expression a2)^(!integer ")")
  | Mo(a1,a2) -> (!integer "(")^(string_of_expression a1)^(!integer "%")^(string_of_expression a2)^(!integer ")")
  | It(a1,a2) -> (!integer "[")^(string_of_expression a1)^(!integer ",")^(string_of_expression a2)^(!integer "]")
  | Bc(b) -> (!boolean (string_of_bool b))
  | No(b) -> (!boolean (Glyph.negation ()))^(string_of_expression b)
  | An(b1,b2) -> (!boolean "(")^(string_of_expression b1)^(!boolean "&")^(string_of_expression b2)^(!boolean ")")
  | Or(b1,b2) -> (!boolean "(")^(string_of_expression b1)^(!boolean "|")^(string_of_expression b2)^(!boolean ")")
  | Eq(e1,e2) -> (!boolean "(")^(string_of_expression e1)^(!boolean "=")^(string_of_expression e2)^(!boolean ")")
  | Gq(e1,e2) -> (!boolean "(")^(string_of_expression e1)^(!boolean ">=")^(string_of_expression e2)^(!boolean ")")
  | Gs(e1,e2) -> (!boolean "(")^(string_of_expression e1)^(!boolean ">")^(string_of_expression e2)^(!boolean ")")
  | Nq(e1,e2) -> (!boolean "(")^(string_of_expression e1)^(!boolean "<>")^(string_of_expression e2)^(!boolean ")")
  | Lq(e1,e2) -> (!boolean "(")^(string_of_expression e1)^(!boolean "<=")^(string_of_expression e2)^(!boolean ")")
  | Ls(e1,e2) -> (!boolean "(")^(string_of_expression e1)^(!boolean "<")^(string_of_expression e2)^(!boolean ")")
  | I_EmptyMessageBox mb -> (!boolean "empty ")^mb
  | Unknown ->  !unknown "?"

let indentation_compteur = ref 0

let left_margin = ref ""

let shift_right n =
  indentation_compteur := !indentation_compteur + n;
  left_margin := String.make !indentation_compteur ' '

let shift_left n =
  indentation_compteur := !indentation_compteur - n;
  left_margin := String.make !indentation_compteur ' '

let string_of_instruction ?(hide_branchings=false) i =
  let open Highlight in
  let rec aux blil =
    let bil = (List.hd blil) in
    let blil' = (List.tl blil) in
    if (blil' <> []) then
      ((aux2 (snd bil))^(!keyword "+[")^(string_of_expression (fst bil))^(!keyword (if blil'=[List.hd blil'] then "]-" else "]+"))^(aux blil'))
    else
      (aux2 (snd bil))
  and aux2 il =
    match il with
    | [hil]    ->  aux3 hil
    | hil::il' -> (aux3 hil)^(aux2 il')
    | _        -> ""
  and aux3 i =
    match i with
    | (P(s),_)   -> Printf.sprintf "%s%s%s%s" (!left_margin) (!keyword "P(") (fst s) (!keyword ")")
    | (V(s),_)   -> Printf.sprintf "%s%s%s%s" (!left_margin) (!keyword "V(") (fst s) (!keyword ")")
    | (R(s,r),_) -> Printf.sprintf "%s%s%s%s%s%s" (!left_margin) (!keyword "R(") (fst s) (!keyword ",") r (!keyword ")")
    | (S(s,r),_) -> Printf.sprintf "%s%s%s%s%s%s" (!left_margin) (!keyword "S(") (fst s) (!keyword ",{") r (!keyword "})")
    | (M(s,r),_) -> Printf.sprintf "%s%s%s% s%s" (!left_margin) (!keyword "M(") (fst s) r (!keyword ")")
    | (N(s),_)   -> Printf.sprintf "%s%s%s%s" (!left_margin) (!keyword "N(") (fst s) (!keyword ")")
    | (A(s),_)   -> Printf.sprintf "%s%s%s%s" (!left_margin) (!keyword "A(") (fst s) (!keyword ")")
    | (F(s),_)   -> Printf.sprintf "%s%s%s%s" (!left_margin) (!keyword "F(") s (!keyword ")")
    | (W(s),_)   -> Printf.sprintf "%s%s%s%s" (!left_margin) (!keyword "W(") (fst s) (!keyword ")")
    | (C(s),_)   -> Printf.sprintf "%s%s%s%s" (!left_margin) (!keyword "C(") s (!keyword ")")
    | (E(s),_)   -> Printf.sprintf "%s%s%s%s" (!left_margin) (!keyword "E(") s (!keyword ")")
    | (T(s,a),_) -> Printf.sprintf "%s%s%s%s" (!left_margin) s (!keyword ":=") (string_of_expression a)
    | (D(et,v),_) -> Printf.sprintf "%s%s%s%s%s%s" (!left_margin) (!keyword "D(") v (!keyword ",") (string_of_ex_type et ) (!keyword ")")
    | (ITE(blil),_) ->
      if hide_branchings then
        Printf.sprintf "%s%s%s" (!keyword "Branching out of ") (string_of_int (List.length blil)) (!keyword " alternatives")
      else
        Printf.sprintf "%s%s%s" (!keyword "\n(\n") (aux blil) (!keyword "\n)")
    | (Nop,_) -> Printf.sprintf "%s%s" (!left_margin) (!keyword "Nop")
  in
  aux3 i

let print_instruction i =
  let open Highlight in
  let answer = ref [] in
  let add_line s = answer := s::!answer in
  let rec
      aux blil = match blil with
      | bil :: blil' ->
	(
	  add_line !left_margin ;
	  let il = snd bil in
	  if il = [] then
	    add_line (!keyword "()")
	  else
	    (
	      match List.hd il with
	      | (ITE(_),_) -> aux2 il
	      | _ ->
		(
		  add_line (!keyword "(") ; add_line "\n" ; (shift_right 2) ;
		  add_line !left_margin ;
		  (aux2 il) ; (shift_left 2) ; add_line ("\n"^(!left_margin)) ;
		  add_line (!keyword ")")
		)
	    );
	  if (blil' <> []) then
	    (
	      add_line ((!keyword "+[")^(string_of_expression (fst bil))^(!keyword "]+\n"));
	      aux blil'
	    )
	)
      | [] -> failwith "print_instruction : Unexpected case"
  and aux2 il =
    match il with
    | i::il' -> ((aux3 (List.hd il)) ; (aux2 il'))
    | [] -> ()
  and aux3 i =
    match fst i with
    | Nop    -> add_line (!keyword "Nop")
    | P(s)   -> add_line ((!keyword "P(")^(fst s)^(!keyword ")"))
    | V(s)   -> add_line ((!keyword "V(")^(fst s)^(!keyword ")"))
    | R(s,r) -> add_line ((!keyword "R(")^(fst s)^(!keyword ",")^r^(!keyword ")"))
    | S(s,r) -> add_line ((!keyword "S(")^(fst s)^(!keyword ",{")^r^(!keyword "})"))
    | M(s,r) -> add_line ((!keyword "M(")^(fst s)^r^(!keyword ")"))
    | N(s)   -> add_line ((!keyword "N(")^(fst s)^(!keyword ")"))
    | A(s)   -> add_line ((!keyword "A(")^(fst s)^(!keyword ")"))
    | F(s)   -> add_line ((!keyword "F(")^s^(!keyword ")"))
    | W(s)   -> add_line ((!keyword "W(")^(fst s)^(!keyword ")"))
    | C(s)   -> add_line ((!keyword "C(")^s^(!keyword ")"))
    | E(s)   -> add_line ((!keyword "E(")^s^(!keyword ")"))
    | T(s,a) -> add_line (s^(!keyword ":=")^(string_of_expression a))
    | D(et,v) -> add_line ((!keyword "D(")^v^(!keyword ":")^(string_of_ex_type et)^(!keyword ")"))
    | ITE(blil) ->
      (
	add_line ((!keyword ("("))) ; (shift_right 2) ; add_line "\n";
	(aux blil) ; (shift_left 2) ; add_line ("\n"^(!left_margin));
	add_line (!keyword ")")
      )
  in
  aux3 i
  ;
  String.concat "" (List.rev !answer)

let string_of_instruction_list ?(no_new_line=false) il =
  let rec aux il =
    match il with
    | i::il' -> string_of_instruction i ^ aux il'
    | []     -> ""
  in
  if il = [] then !Highlight.constant (if no_new_line then "Void" else "\nVoid") else (if no_new_line then "" else "\n") ^ aux il


let rec variable_occurs_in_process s = function
  | (T(s',e), _)::il -> (s = s') || (variable_occurs_in_expression s e) || (variable_occurs_in_process s il)
  | (ITE blil, _)::il -> (variable_occurs_in_process s il) || (List.exists (fun (e,il) -> (variable_occurs_in_expression s e) || (variable_occurs_in_process s il)) blil)
  | (D(_,s'),_)::il -> s = s' || variable_occurs_in_process s il
  | (P _, _)::il
  | (V _, _)::il
  | (R _, _)::il
  | (S _, _)::il
  | (M _, _)::il
  | (N _, _)::il
  | (A _, _)::il
  | (F _, _)::il
  | (W _, _)::il
  | (C _, _)::il
  | (E _, _)::il
  | (Nop, _)::il -> variable_occurs_in_process s il
  | [] -> false

let all_variables_occurring_in_process ?(in_depth=false) p =
  let rec all_variables_occurring_in_process accu p = match p with
    | (T(s,e), _)::p ->
      StringSet.add s (StringSet.union (all_variables_occurring_in_expression e) accu)
    | (ITE blil, _)::p ->
      let aux2 =
        List.fold_left
	  (fun accu2 (e,p2) ->
            StringSet.union
	      (all_variables_occurring_in_expression e)
              (all_variables_occurring_in_process accu2 p2)
          ) accu blil
      in
      let aux = all_variables_occurring_in_process accu p in
      StringSet.union aux aux2
    | (D(_,s),_)::p -> StringSet.add s (all_variables_occurring_in_process accu p)
    | (P _, _)::p
    | (V _, _)::p
    | (R _, _)::p
    | (S _, _)::p
    | (M _, _)::p
    | (N _, _)::p
    | (A _, _)::p
    | (F _, _)::p
    | (W _, _)::p
    | (C _, _)::p
    | (E _, _)::p
    | (Nop, _)::p -> all_variables_occurring_in_process accu p
    | [] -> accu
  in
  all_variables_occurring_in_process StringSet.empty p

let all_variables_override_in_process p =
  let rec all_variables_override_in_process accu p = match p with
    | (T(s,_), _)::p -> StringSet.add s accu
    | (ITE blil, _)::p ->
      let aux2 = List.fold_left
	(fun accu2 (e,p2) -> all_variables_override_in_process accu2 p2)
	accu blil in
      let aux = all_variables_override_in_process accu p in
      StringSet.union aux aux2
    | (D(_,s),_)::p -> StringSet.add s (all_variables_override_in_process accu p)
    | (P _, _)::p
    | (V _, _)::p
    | (R _, _)::p
    | (S _, _)::p
    | (M _, _)::p
    | (N _, _)::p
    | (A _, _)::p
    | (F _, _)::p
    | (W _, _)::p
    | (C _, _)::p
    | (E _, _)::p
    | (Nop, _)::p -> all_variables_override_in_process accu p
    | [] -> accu
  in
  all_variables_override_in_process StringSet.empty p


(* "Oil" stands for "ordered instruction list" *)

module type OIL =
sig
  type t
  val compare: t -> t -> int
  val collect_calls: t -> StringSet.t
end(*OIL*)

module Oil =
struct
  type t = instruction list

  let collect_calls il1 =
    let rec aux il2 st = match il2 with
      | (F(s))::il2' -> aux il2' (StringSet.add s st)
      |  ins::il2'   -> aux il2' st
      |  []          -> st
    in
    aux il1 (StringSet.empty)

  let rec compare l1 l2 = match (l1,l2) with
    | (h1::l1',h2::l2') ->
      let c = Pervasives.compare h1 h2 in
      if c = 0 then compare l1' l2' else c
    | ([],[]) ->  0
    | ([],l2) -> -1
    | (l1,[]) ->  1

end(*Oil*)

let multiset_inc k b m =
  try StringMap.add k ((StringMap.find k m) + b) m
  with Not_found -> StringMap.add k b m

(* Renvoie l'ensemble des clés auxquelles une valeur est attachée *)

(* Soil : Set of instruction list *)

module Soil = Set.Make(Oil)

type process =
    {
      father: int                 ; (* Le pid de celui qui a créé le processus *)
      mutable spawns: int list            ; (* Les descandants du processus *)
      mutable ip    : int                 ; (* La prochaine instruction à exécuter *)
      mutable code  : instruction array   (* Le code du processus, celui-ci peut-être modifié dynamiquement au cours de l'exécution symbolique *)
    }

type process_class =
  {
    name: string ;
    resources: Sod.t ;
    calls: string list ;
    body: instruction list
  }

type t =
  {
    declarations: Resource.t list ;
    equations: process_class list ; (* Le dictionnaire associe à chaque nom de processus le corps de ses instructions *)
    initials: string list ; (* Liste des processus lancés initialement *)
    mutable runpro: process array ; (*running processes*)
    inclusions: string list ;
  }

let name_from_process_class {name ;  _} = name

let body_from_process_class {body ;  _} = body

let name_n_body_from_process_class {name ; body ; _} = name , body

let equations_converter equations = List.map name_n_body_from_process_class equations

let name_to_body name equations = (*: string -> process_class list -> instruction list*)
  (List.find (fun pc -> pc.name = name) equations).body

let all_variables_occurring_in_program s = [] (*dummy*) (*s.variables*)

let is_mutex x = match x with
  | Mutex mv -> true
  | _ -> false

let is_write x = match x with Variable `Write -> true | _ -> false

let is_read x = match x with Variable `Read -> true | _ -> false

let is_synchronization x = match x with Synchronization _ -> true | _ -> false

let is_semaphore x = match x with Semaphore _ -> true | _ -> false

let is_lifo x = match x with Channel (`Lifo,_) -> true | _ -> false

let is_fifo x = match x with Channel (`Fifo,_) -> true | _ -> false

let is_channel x = match x with Channel _ -> true | _ -> false

let is_monitor x = match x with Monitor -> true | _ -> false

let is_notify x = match x with Notify _ -> true | _ -> false

let is_notify_some x = match x with Notify `Some -> true | _ -> false

let is_notify_all x = match x with Notify `All -> true | _ -> false

let is_variable x = match x with Variable _ -> true | _ -> false

let mos_of_list list_of_bindings =
  List.fold_left (fun accu (key,bind) -> StringMap.add key bind accu) StringMap.empty list_of_bindings

let lifo s =
  let f (n,r) =
    match r with
    | Channel (`Lifo,k) -> Some (n, k)
    | _ -> None in
  List.filter_map f s.declarations

let fifo s =
  let f (n,r) =
    match r with
    | Channel (`Fifo,k) -> Some (n, k)
    | _ -> None in
  List.filter_map f s.declarations

let semaphore s q =
  let f (n,r) = match r with
    | Semaphore (q',k) when q' = q -> Some (n, k)
    | _ -> None in
  List.filter_map f s.declarations

let semaphore_arity s name =
  let rec semaphore_arity name resources =
    match resources with
    | (name',Semaphore (_,n)) :: remaining when name = name' -> n
    | _::remaining -> semaphore_arity name remaining
    | [] -> Common.Terminal.(Printf.printf "%s The semaphore %s is not declared. However, its %s is assumed to be %s\n" (color Red "Warning:") (color Red name) (color Red "arity") (color Red "2"); 2)
  in
  semaphore_arity name s.declarations

let semaphore_kind s name =
  let rec semaphore_kind name resources =
    match resources with
    | (name',((Semaphore (q,_)) as kind)) :: remaining when name = name' -> kind
    | _ :: remaining -> semaphore_kind name remaining
    | [] -> raise Not_found in
  semaphore_kind name s.declarations

let channel_kind s name =
  let rec channel_kind name resources =
    match resources with
    | (name',((Channel _) as kind)) :: remaining when name = name' -> kind
    | _ :: remaining -> channel_kind name remaining
    | [] -> raise Not_found in
  channel_kind name s.declarations

let write_kind s name =
  let rec write_kind name resources =
    match resources with
    | (name',(Variable `Write as kind)) :: remaining when name = name' -> kind
    | _ :: remaining -> write_kind name remaining
    | [] -> raise Not_found in
  write_kind name s.declarations

let read_kind s name =
  let rec read_kind name resources =
    match resources with
    | (name',(Variable `Read as kind)) :: remaining when name = name' -> kind
    | _ :: remaining -> read_kind name remaining
    | [] -> raise Not_found in
  read_kind name s.declarations

let variable_kind s name =
  let rec variable_kind name resources =
    match resources with
    | (name',(Variable _ as kind)) :: remaining when name = name' -> kind
    | _ :: remaining -> variable_kind name remaining
    | [] -> raise Not_found in
  variable_kind name s.declarations

let synchronization_kind s name =
  let rec synchronization_kind name resources =
    match resources with
    | (name',((Synchronization _) as kind)) :: remaining when name = name' -> kind
    | _ :: remaining -> synchronization_kind name remaining
    | [] -> raise Not_found in
  synchronization_kind name s.declarations

let monitor_kind s name =
  let rec monitor_kind name resources =
    match resources with
    | (name',(Monitor as kind)) :: remaining when name = name' -> kind
    | _ :: remaining -> monitor_kind name remaining
    | [] -> raise Not_found in
  monitor_kind name s.declarations

let synchronization_arity s name =
  let rec synchronization_arity name resources =
    match resources with
    | (name',Synchronization n) :: remaining when name = name' -> n
    | _::remaining -> synchronization_arity name remaining
    | [] -> Common.Terminal.(Printf.printf "%s The semaphore %s is not declared. However, its %s is assumed to be %s\n" (color Red "Warning:") (color Red name) (color Red "arity") (color Red "2"); 2)
  in
  synchronization_arity name s.declarations

let mutex s q =
  let f (n,r) =
    match r with
    | Mutex q' when q' = q -> Some n
    | _ -> None in
  List.filter_map f s.declarations

let all_mutex s =
  let f (n,r) = if is_mutex(*_extended*) r then Some (n,r) else None in
  let explicitly_declared_mutex = List.filter_map f s.declarations in
  let f e = List.filter_map f (Sod.elements e.resources) in
  List.fold_left (fun accu e -> (f e) @ accu) explicitly_declared_mutex s.equations

let all_semaphores s =
  let f (n,r) = if is_semaphore r then Some (n,r) else None in
  List.filter_map f s.declarations

let all_channels s =
  let f (n,r) = if is_channel r then Some (n,r) else None in
  List.filter_map f s.declarations

let all_synchronizations s =
  let f (n,r) = if is_synchronization r then Some (n,r) else None in
  List.filter_map f s.declarations

let all_monitors s =
  let f (n,r) = if is_monitor r then Some (n,r) else None in
  List.filter_map f s.declarations

let all_write_variables s =
  let f accu { resources ; _ } = Sod.union (Sod.filter (fun (_,rk) -> rk = Variable `Write) resources) accu  in
  List.fold_left f Sod.empty s.equations

let all_read_variables s =
  let f accu { resources ; _ } = Sod.union (Sod.filter (fun (_,rk) -> rk = Variable `Read) resources) accu in
  List.fold_left f Sod.empty s.equations

let all_variables s =
  let f accu { name ; resources ; _ } = Sod.union (Sod.filter (fun (ident,rk) -> is_variable rk) resources) accu in
  List.fold_left f Sod.empty s.equations

let all_notify s =
  let f (n,r) = if is_notify r then Some (n,r) else None in
  List.filter_map f s.declarations

let all_notify_all s =
  let f (n,r) = if is_notify_all r then Some (n,r) else None in
  List.filter_map f s.declarations

let all_notify_some s =
  let f (n,r) = if is_notify_some r then Some (n,r) else None in
  List.filter_map f s.declarations

let synchronization s =
  List.filter_map
    (fun (n, r) ->
      match r with
      | Synchronization k -> Some (n,k)
      | _ -> None
    ) s.declarations

let monitor s =
  List.filter_map
    (fun (n, r) ->
      match r with
        | Monitor -> Some n
        | _ -> None
    ) s.declarations

let empty () =
  {
    declarations  = []   ;
    equations  = []   ;
    initials   = []   ;
    runpro     = [||] ;
    inclusions = []   ;
  }

  (* Terminal output below *)

let print_process(*2*) il =
  if il = [] then
    !Highlight.constant "Void\n"
  else
    (String.concat (!Highlight.keyword ".\n") (List.map print_instruction(*2*) il))^"\n"

let print_runpro(*2*) pa =
  let answer = ref [] in
  let aux i = String.concat ""
    [
      ((!Highlight.keyword (!Highlight.parallel_separator ())));
      ("PID : "^(string_of_int i))^"\n" ;
      "Spawns PID's : "^(String.concat " " (List.map (fun pid -> string_of_int pid) pa.(i).spawns))^"\n" ;
      "Current IP : "^(string_of_int ((pa.(i).ip)/2))^"\n" ;
      (!Highlight.keyword (!Highlight.parallel_separator ()));
      print_process (Array.to_list pa.(i).code)
    ]
  in
  let n = Array.length pa in
  for i = 0 to (n-1) do
    answer := (aux i)::!answer
  done;
  String.concat "" (List.rev !answer)


let runpro semantics = semantics.runpro

    (* running_process needs to be "adapted" to the case where
       "interpretation" is performed before analysis. *)
let running_processes semantics =
  let runpro = semantics.runpro in
  if runpro = [||]
  then !Highlight.error "\nNo running processus\n\n"
  else
    !Highlight.paragraph ("\nRunning Process"^if Array.length runpro > 1 then "es\n" else "\n") ^
      !left_margin ^
      print_runpro runpro

let resources env =
  let answer = ref "" in
  let add_line s = answer := !answer ^ s in
  let print_list s =
    List.iter (fun e -> add_line (!Highlight.resource e ^ "\n")) s in
  let print_list_aux string_of_bind m =
    let aux (k,b) =
      add_line (!Highlight.resource k ^ " of arity " ^ string_of_bind b ^ "\n")
    in
    List.iter aux m
  in
  (
    if env.declarations = [] then
      add_line (!Highlight.error "\nNo Resource\n")
    else
      let mtx_switch = mutex env `Default in
      let mtx_rec = mutex env `Recursive in
      let mtx_normal = mutex env `Normal in
      let mtx_ec = mutex env `Error_check in
      if mtx_switch@mtx_rec@mtx_normal@mtx_ec = [] then
	add_line (!Highlight.constant "\nNo Mutex\n")
      else
	(
	  if mutex env `Default <> [] then
	    (
	      add_line (!Highlight.good_news "\nSwitch Mutex\n");
	      add_line !left_margin; print_list mtx_switch
	    );
	  if mutex env `Recursive <> [] then
	    (
	      add_line (!Highlight.good_news "\nRecursive Mutex\n") ;
	      add_line (!left_margin) ; print_list mtx_rec
	    );
	  if mutex env `Normal <> [] then
	    (
	      add_line (!Highlight.good_news "\nNormal Mutex\n") ;
	      add_line (!left_margin) ; print_list mtx_normal
	    );
	  if mutex env `Error_check <> [] then
	    (
	      add_line (!Highlight.good_news "\nError Check Mutex\n") ;
	      add_line (!left_margin) ; print_list mtx_ec
	    )
	);
      let mon = monitor env in
      if mon = [] then
	add_line (!Highlight.good_news "\nNo Monitor\n")
      else
	(
	  add_line (!Highlight.good_news "\nMonitor\n") ;
	  add_line (!left_margin) ; print_list mon
	);
      let sems = semaphore env `Default in
      let semq = semaphore env `Quantitative in
      if sems = [] && semq = [] then
	add_line (!Highlight.good_news "\nNo Semaphore\n")
      else
	(
	  if sems <> [] then
	    (
	      add_line (!Highlight.good_news "\nSwitch Semaphore\n") ;
	      add_line (!left_margin) ; print_list_aux string_of_int sems
	    );
	  if semq <> [] then
	    (
	      add_line (!Highlight.good_news "\nQuantitative Semaphore\n") ;
	      add_line (!left_margin) ; print_list_aux string_of_int semq
	    )
	);
      let fifo = fifo env in
      if fifo = [] then
	add_line (!Highlight.good_news "\nNo Pipe\n")
      else
	(
	  add_line (!Highlight.good_news "\nPipe\n") ;
	  add_line (!left_margin) ; print_list_aux string_of_int fifo
	);
      let lifo = lifo env in
      if lifo = [] then
	add_line (!Highlight.good_news "\nNo Pile\n")
      else
	(
	  add_line (!Highlight.good_news "\nPile\n") ;
	  add_line (!left_margin) ; print_list_aux string_of_int lifo
	);
      let sync = synchronization env in
      if sync = [] then
	add_line (!Highlight.good_news "\nNo Synchronization\n")
      else
	(
	  add_line (!Highlight.good_news "\nSynchronization\n");
	  add_line (!left_margin) ; print_list_aux string_of_int sync
	)
  );
  !answer

let equation_names equations =
  let ans = ref "" in
  List.iter (fun (n,_) -> ans := !ans ^ n ^ "\n") equations;
  !ans

let all_declared_processes env = List.map (fun p -> p.name) env.equations

let equations env to_display =
  let answer = ref "" in
  let add_line s = answer := !answer ^ s in
  (
    match to_display with
    | None ->
      let print_list_aux print_bind m =
	let aux (k,b) =
	  if List.mem k env.initials then
	    (
	      add_line Highlight.(!parallel_separator ());
              add_line (!Highlight.center 15 ((!Highlight.good_news k)^(!Highlight.constant " (initial)")));
	      add_line "\n";
	      add_line Highlight.(!parallel_separator ());
	      print_bind b
	    )
	  else
	    (add_line ((!Highlight.resource k)^" = "^"\n");
	     print_bind b)
	in
	List.iter aux m
      in
      if env.equations = [] then
	add_line (!Highlight.error "\nNo equation\n\n")
      else
	(
	  add_line (!Highlight.good_news "\nEquation(s)\n") ;
	  add_line !left_margin;
	  print_list_aux
	    (fun s -> add_line (print_process s))
	    (equations_converter env.equations)
	)
    | Some sl ->
      let print_list_aux print_bind m =
	let aux (k,b) =
	  if List.mem k sl
	  then
	    (
	      if List.mem k env.initials then
		(
		  add_line Highlight.(!parallel_separator ()) ;
		  add_line (!Highlight.center 15 ((!Highlight.resource k)^(!Highlight.constant " (initial)"))) ;
		  add_line "\n" ;
		  add_line Highlight.(!parallel_separator ()) ;
		  print_bind b
		)
	      else
		(add_line ((!Highlight.resource k)^" = "^"\n");print_bind b)
	    )
	in List.iter aux m in
      if env.equations = [] then
	add_line (!Highlight.error "\nNo equation\n\n")
      else
	(
	  add_line (!Highlight.good_news "\nEquation(s)\n") ;
	  add_line (!left_margin) ; print_list_aux (fun s -> add_line (print_process s)) (equations_converter env.equations)
	)
  );
  !answer

let variables semantics = "" (*Cette fonction est amenée à disparaitre*)
(*
  let header =
    match semantics.variables with
    | [] -> !Highlight.error "\nNo variable\n"
    | [_] -> !Highlight.good_news "\nThe unique variable is\n"
    | _ -> !Highlight.good_news "\nThe variables are\n" in
  let variables = String.concat " " (List.map fst semantics.variables) in
  Printf.sprintf "%s %s" header variables
*)

let initials semantics =
  let initials = semantics.initials in
  let header =
    match initials with
    | [] -> !Highlight.error "\nNo initial process"
    | [_] -> (!Highlight.good_news "\nThe unique initial process is")^" "
    | _ -> (!Highlight.good_news "\nThe initial processes are")^" "
  in Printf.sprintf "%s%s\n" header (String.concat "|" semantics.initials)

let mutex_var_of_kind kind =
  match kind with
  | Mutex var -> var
  | _ -> invalid_arg "mutex_var_of_kind"

let semaphore_var_of_kind kind =
  match kind with
  | Semaphore var -> var
  | _ -> invalid_arg "semaphore_var_of_kind"

let mutex_kind { declarations ; _ } name =
  (snd (List.find (fun (x,rk) -> x = name && is_mutex rk) declarations))

let notify_kind { declarations ; _ } name =
  (snd (List.find (fun (x,rk) -> x = name && is_notify rk) declarations))

let mutex_var s name = mutex_var_of_kind (mutex_kind s name)

let semaphore_var { declarations ; _ } name =
  fst (semaphore_var_of_kind (snd (List.find (fun (x,rk) -> x = name && is_semaphore rk) declarations)))

let semvar_of_kind k =
  match k with
  | Semaphore (semaphore_var , _) -> semaphore_var
  | _ -> failwith "semaphore_var_of_kind"

let colliding_declarations r1 r2 =
  let aux rk = match snd rk with
    | Mutex mv -> 0
    | Semaphore _ -> 0
    | Synchronization _ -> 1
    | Monitor -> 2
    | Channel _ -> 3
    | Notify _ -> 5
    | Variable _ -> 6 in
  fst r1 = fst r2 && aux r1 = aux r2 && r1 <> r2
