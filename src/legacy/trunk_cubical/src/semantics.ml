open Common

exception Undefined

module Highlight =
struct

let center = ref
  (fun n s -> String.make (120 (*fixed terminal width*) - String.length s + n / 2) ' ' ^ s)

  let parallel_separator = ref (fun () -> (String.make 7 '_')^"\n\n")
  let utf8_enabled = ref true
  let color_enabled = ref true
  let boolean = ref (fun b -> Terminal.rgb 0 5 5 b)
  let integer = ref (fun n -> Terminal.rgb 3 0 1 n)
  let variable = ref id
  let unknown = ref (fun x -> Terminal.rgb ~bold:true 1 0 0 x)
  let keyword = ref (fun kw -> Terminal.(color Blue ~bold:true kw))
  let constant = ref (fun cst -> Terminal.(color White cst))
  let error = ref (fun err -> Terminal.(color Red ~bold:true ~underline:true err))
  let paragraph = ref (fun parg -> Terminal.(color ~underline:true ~bold:true Green parg))
  let good_news = ref (fun parg -> Terminal.(color ~bold:true Green parg))
  let resource = ref (fun err -> Terminal.(color Green err))
end

type ex_type =
  | T_int
  | T_bool

let string_of_ex_type t =
  match t with
  | T_int -> "int"
  | T_bool -> "bool"

type ex_aribo =
  | Va of string (* Variable *)
  | Unknown      (* Dummy default value *)
  (* --------------------------------------------------- *)
  | Ac of int                 (* Arithmetic constant     *)
  | Ne of ex_aribo            (* Negative                *)
  | Ab of ex_aribo            (* Magnitude               *)
  | Pl of ex_aribo * ex_aribo (* Plus   - Addition       *)
  | Mi of ex_aribo * ex_aribo (* Minus  - Substraction   *)
  | Ti of ex_aribo * ex_aribo (* Times  - Multiplication *)
  | Di of ex_aribo * ex_aribo (* Divide - Division       *)
  (* --------------------------------------------------- *)
  | Bc of bool                (* Boolean constant *)
  | An of ex_aribo * ex_aribo (* And              *)
  | Or of ex_aribo * ex_aribo (* Or               *)
  | No of ex_aribo            (* Not              *)
  | Eq of ex_aribo * ex_aribo (* Equal            *)
  | Lq of ex_aribo * ex_aribo (* Less or Equal    *)
  | Ls of ex_aribo * ex_aribo (* Strictly Less    *)
  | Gq of ex_aribo * ex_aribo (* Greater or Equal *)
  | Gs of ex_aribo * ex_aribo (* Strictly Greater *)
  | Nq of ex_aribo * ex_aribo (* Not Equal        *)
  | I_EmptyMessageBox of string (* Internal instruction – not available to programer *)
  (* --------------------------------------------- *)
  | It of  ex_aribo * ex_aribo (* Interval – for statical analysis *)

type instr =
  | P of string
  | V of string
  | R of string * string (*receive 1st argument: name of the stack/queue 2nd argument: message*)
  | S of string * string (*send 1st argument: name of the stack/queue 2nd argument: container*)
  | M of string * string (*monitoring*)
  | N of string (*One of the processes waiting for the signal may keep on going*)
  | A of string (*All processes blocked by the argument are released*)
  | F of string (*fork*)
  | W of string (*wait*)
  | C of string (*call*)
  | E of string (*?*)
  | D of ex_type * string (*variable declaration*)
  | T of string * ex_aribo (*assign*)
  | ITE of ((instruction list * ex_aribo) list) (*If Then Else – it is actually more like a match case*)
  | NIL (*The instruction that does nothing*)
  | Nop
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
(*
  | Unknown ->  Identifier
*)
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


let rec string_of_ex_aribo =
  let open Highlight in
  function
  | Va(s) -> !variable s
  | Ac(n) -> !integer (string_of_int n)
  | Ne(a) -> (!integer "(")^(!integer "-")^(string_of_ex_aribo a)^(!integer ")")
  | Ab(n) -> (!integer "abs(")^(string_of_ex_aribo n)^(!integer ")")
  | Pl(a1,a2) -> (!integer "(")^(string_of_ex_aribo a1)^(!integer "+")^(string_of_ex_aribo a2)^(!integer ")")
  | Mi(a1,a2) -> (!integer "(")^(string_of_ex_aribo a1)^(!integer "-")^(string_of_ex_aribo a2)^(!integer ")")
  | Ti(a1,a2) -> (!integer "(")^(string_of_ex_aribo a1)^(!integer "*")^(string_of_ex_aribo a2)^(!integer ")")
  | Di(a1,a2) -> (!integer "(")^(string_of_ex_aribo a1)^(!integer "/")^(string_of_ex_aribo a2)^(!integer ")")
  | It(a1,a2) -> (!integer "[")^(string_of_ex_aribo a1)^(!integer ",")^(string_of_ex_aribo a2)^(!integer "]")
  (*
    | Unknown ->  magenta ~bold:true "?"
  *)
  | Bc(b) -> (!boolean (string_of_bool b))
  | No(b) -> (!boolean (Glyph.negation ()))^(string_of_ex_aribo b)
  | An(b1,b2) -> (!boolean "(")^(string_of_ex_aribo b1)^(!boolean "&")^(string_of_ex_aribo b2)^(!boolean ")")
  | Or(b1,b2) -> (!boolean "(")^(string_of_ex_aribo b1)^(!boolean "|")^(string_of_ex_aribo b2)^(!boolean ")")
  | Eq(e1,e2) -> (!boolean "(")^(string_of_ex_aribo e1)^(!boolean "=")^(string_of_ex_aribo e2)^(!boolean ")")
  | Gq(e1,e2) -> (!boolean "(")^(string_of_ex_aribo e1)^(!boolean ">=")^(string_of_ex_aribo e2)^(!boolean ")")
  | Gs(e1,e2) -> (!boolean "(")^(string_of_ex_aribo e1)^(!boolean ">")^(string_of_ex_aribo e2)^(!boolean ")")
  | Nq(e1,e2) -> (!boolean "(")^(string_of_ex_aribo e1)^(!boolean "<>")^(string_of_ex_aribo e2)^(!boolean ")")
  | Lq(e1,e2) -> (!boolean "(")^(string_of_ex_aribo e1)^(!boolean "<=")^(string_of_ex_aribo e2)^(!boolean ")")
  | Ls(e1,e2) -> (!boolean "(")^(string_of_ex_aribo e1)^(!boolean "<")^(string_of_ex_aribo e2)^(!boolean ")")
  | I_EmptyMessageBox mb -> (!boolean "empty ")^mb
  | Unknown ->  !unknown "?"
(*
  | _ -> Globals.unknown_instruction ()
*)


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
  let rec aux ilbl =
    let ilb = (List.hd ilbl) and ilbl' = (List.tl ilbl) in
    if (ilbl' <> []) then
      ((aux2 (fst ilb))^(!keyword "+[")^(string_of_ex_aribo (snd ilb))^(!keyword (if ilbl'=[List.hd ilbl'] then "]-" else "]+"))^(aux ilbl'))
    else
      (aux2 (fst ilb))
  and aux2 il =
    match il with
    | [hil]    ->  aux3 hil
    | hil::il' -> (aux3 hil)^(aux2 il')
    | _        -> ""
  and aux3 i =
    match i with
    | (P(s),_)   -> (!left_margin)^(!keyword "P(")^s^(!keyword ")")
    | (V(s),_)   -> (!left_margin)^(!keyword "V(")^s^(!keyword ")")
    | (R(s,r),_) -> (!left_margin)^(!keyword "R(")^s^(!keyword ",")^r^(!keyword ")")
    | (S(s,r),_) -> (!left_margin)^(!keyword "S(")^s^(!keyword ",{")^r^(!keyword "})")
    | (M(s,r),_) -> (!left_margin)^(!keyword "M(")^s^r^(!keyword ")")
    | (N(s),_)   -> (!left_margin)^(!keyword "N(")^s^(!keyword ")")
    | (A(s),_)   -> (!left_margin)^(!keyword "A(")^s^(!keyword ")")
    | (F(s),_)   -> (!left_margin)^(!keyword "F(")^s^(!keyword ")")
    | (W(s),_)   -> (!left_margin)^(!keyword "W(")^s^(!keyword ")")
    | (C(s),_)   -> (!left_margin)^(!keyword "C(")^s^(!keyword ")")
    | (E(s),_)   -> (!left_margin)^(!keyword "E(")^s^(!keyword ")")
    | (T(s,a),_) -> (!left_margin)^s^(!keyword ":=")^(string_of_ex_aribo a)
    | (D(et,v),_) -> (!left_margin)^(!keyword "D(")^v^(!keyword ",")^(string_of_ex_type et )^(!keyword ")")
    (*| (T(s,a),_) -> (!left_margin)^(!keyword "@(")^s^(!keyword ",")^(string_of_ex_aribo a)^(!keyword ")")*)
    | (ITE(ilbl),_) ->
      if hide_branchings then
        (!keyword "Branching out of ")^(string_of_int (List.length ilbl))^(!keyword " alternatives")
      else
        (!keyword "\n(\n")^(aux ilbl)^(!keyword "\n)")
    | (NIL,_) -> (!left_margin)^(!keyword "NIL")
  in
  aux3 i

let print_instruction(*2*) i =
  let open Highlight in
  let answer = ref [] in
  let add_line s = answer := s::!answer in
  let rec
      aux ilbl = match ilbl with
      | ilb::ilbl' ->
	(
	  add_line !left_margin ;
	  let il = fst ilb in
	  if il=[] then
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
	  if (ilbl' <> []) then
	    (
	      add_line ((!keyword "+[")^(string_of_ex_aribo (snd ilb))^(!keyword "]+\n"));
	      aux ilbl'
	    )
	)
      | [] -> failwith "print_instruction : Unexpected case"
  and aux2 il =
    match il with
    | i::il' -> ((aux3 (List.hd il)) ; (aux2 il'))
    | [] -> ()
  and aux3 i =
    match fst i with
    | P(s)   -> add_line ((!keyword "P(")^s^(!keyword ")"))
    | V(s)   -> add_line ((!keyword "V(")^s^(!keyword ")"))
    | R(s,r) -> add_line ((!keyword "R(")^s^(!keyword ",")^r^(!keyword ")"))
    | S(s,r) -> add_line ((!keyword "S(")^s^(!keyword ",{")^r^(!keyword "})"))
    | M(s,r) -> add_line ((!keyword "M(")^s^r^(!keyword ")"))
    | N(s)   -> add_line ((!keyword "N(")^s^(!keyword ")"))
    | A(s)   -> add_line ((!keyword "A(")^s^(!keyword ")"))
    | F(s)   -> add_line ((!keyword "F(")^s^(!keyword ")"))
    | W(s)   -> add_line ((!keyword "W(")^s^(!keyword ")"))
    | C(s)   -> add_line ((!keyword "C(")^s^(!keyword ")"))
    | E(s)   -> add_line ((!keyword "E(")^s^(!keyword ")"))
    | T(s,a) -> add_line (s^(!keyword ":=")^(string_of_ex_aribo a))
    | D(et,v) -> add_line ((!keyword "D(")^v^(!keyword ":")^(string_of_ex_type et)^(!keyword ")"))

    (*| T(s,a) -> add_line ((!keyword "@(")^s^(!keyword ",")^(string_of_ex_aribo a)^(!keyword ")"))*)
    | ITE(ilbl) ->
      (
	add_line ((!keyword ("("))) ; (shift_right 2) ; add_line "\n";
	(aux ilbl) ; (shift_left 2) ; add_line ("\n"^(!left_margin));
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
  | (ITE ilbl, _)::il -> (variable_occurs_in_process s il) || (List.exists (fun (il,e) -> (variable_occurs_in_expression s e) || (variable_occurs_in_process s il)) ilbl)
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
  | (E _, _)::il -> variable_occurs_in_process s il
  | [] -> false

let all_variables_occurring_in_process ?(in_depth=false) p =
  let rec all_variables_occurring_in_process accu p = match p with
    | (T(s,e), _)::p ->
      StringSet.add s (StringSet.union (all_variables_occurring_in_expression e) accu)
    | (ITE ilbl, _)::p ->
      let aux2 =
        List.fold_left
	  (fun accu2 (p2,e) ->
            StringSet.union
	      (all_variables_occurring_in_expression e)
              (all_variables_occurring_in_process accu2 p2)
          ) accu ilbl
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
    | (E _, _)::p -> all_variables_occurring_in_process accu p
    | [] -> accu
  in
  all_variables_occurring_in_process StringSet.empty p

let all_variables_override_in_process p =
  let rec all_variables_override_in_process accu p = match p with
    | (T(s,_), _)::p -> StringSet.add s accu
    | (ITE ilbl, _)::p ->
      let aux2 = List.fold_left
	(fun accu2 (p2,e) -> all_variables_override_in_process accu2 p2)
	accu ilbl in
      let aux = all_variables_override_in_process accu p
      in StringSet.union aux aux2
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
    | (E _, _)::p -> all_variables_override_in_process accu p
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


(*
  let string_of_instruction i =
    let rec aux ilbl =
      (
	let ilb = (List.hd ilbl) and ilbl' = (List.tl ilbl) in
	if ilbl' <> [] then
	  ((aux2 (fst ilb))^"+["^(string_of_ex_aribo (snd ilb))^"]+"^(aux ilbl'))
	else
	  ((aux2 (fst ilb))^"+["^(string_of_ex_aribo (snd ilb))^"]")
      )
    and aux2 il = (aux3 (fst(List.hd il))) ^ (aux2 (List.tl il))
    and aux3 i = match i with
      | P(s)      -> "P." ^ s
      | V(s)      -> "V." ^ s
      | R(s1,s2)  -> "R." ^ s1 ^ "." ^ s2
      | S(s1,s2)  -> "S." ^ s1 ^ "{" ^ s2 ^ "}"
      | M(s1,s2)  -> "M." ^ s1 ^ "." ^s2
      | N(s)      -> "N." ^ s
      | A(s)      -> "A." ^ s
      | F(s)      -> "F." ^ s
      | W(s)      -> "W." ^ s
      | C(s)      -> "C." ^ s
      | E(s)      ->        s
      | D(t,s)    -> "D." ^ s
      | T(s,a)    -> "T." ^ s ^ (string_of_ex_aribo a)
      | ITE(ilbl) -> "(" ^ (aux ilbl) ^ ")"
    in
    aux3 i
*)

(*
  let instruction_compare i1 i2 =
    let aux = String.compare (string_of_instruction (fst i1)) (string_of_instruction (fst i2))
    in
    if aux <> 0
    then aux
    else Pervasives.compare (snd i1) (snd i2)
*)


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

type mutex_var = [ `Error_check | `Normal | `Recursive | `Switch ]
type sem_var = [ `Switch | `Quantitative ]

type resource_kind =
| Mutex of mutex_var
| Monitor
| Semaphore of (sem_var * int)
| Fifo of int
| Lifo of int
| Synchronization of int
| FifoMutex (*Always supposed to be of `Switch kind*)
| LifoMutex (*Always supposed to be of `Switch kind*)

let arity_of_kind k =
  match k with
  | Semaphore (_,arity)
  | Fifo arity
  | Lifo arity
  | Synchronization arity -> Some arity
  | _ -> None

type t =
  {
    resources: (string * resource_kind) list;
    variables: string list;
    equations: (string * instruction list) list; (* Le dictionnaire associe à chaque nom de processus le corps de ses instructions *)
    initials: string list; (* Liste des processus lancés initialement *)
    mutable runpro: process array; (*running processes*)
    inclusions: string list;
  }

let all_variables_occurring_in_program s =
  let f accu p = StringSet.union (all_variables_occurring_in_process p) accu
  in List.fold_left f StringSet.empty (List.map snd s.equations)

let is_mutex x =
  x = Mutex `Error_check || x = Mutex `Normal || x = Mutex  `Recursive || x = Mutex `Switch || x = FifoMutex || x = LifoMutex

let is_synchronization x = match x with Synchronization _ -> true | _ -> false

let is_semaphore x = match x with Semaphore _ -> true | _ -> false

let mos_of_list list_of_bindings =
  List.fold_left (fun accu (key,bind) -> StringMap.add key bind accu) StringMap.empty list_of_bindings

let lifo s =
  let f (n,r) =
    match r with
    | Lifo k -> Some (n, k)
    | _ -> None
  in
  List.filter_map f s.resources

let lifo_mutex s =
  let f (n,r) =
    match r with
    | LifoMutex -> Some n
    | _ -> None
  in
  List.filter_map f s.resources

let fifo s =
  let f (n,r) =
    match r with
    | Fifo k -> Some (n, k)
    | _ -> None
  in
  List.filter_map f s.resources

let fifo_mutex s =
  let f (n,r) =
    match r with
    | FifoMutex -> Some n
    | _ -> None
  in
  List.filter_map f s.resources

let semaphore q s =
  let f (n,r) = match r with
    | Semaphore (q',k) when q' = q -> Some (n, k)
    | _ -> None
  in
  List.filter_map f s.resources

let semaphore_arity name s =
  let rec semaphore_arity name resources =
    match resources with
    | (name',Semaphore (_,n))::remaining when name=name' -> n
    | _::remaining -> semaphore_arity name remaining
    | [] -> Common.Terminal.(Printf.printf "%s The semaphore %s is not declared. However, its %s is assumed to be %s\n" (color Red "Warning:") (color Red name) (color Red "arity") (color Red "2"); 2)
  in
  semaphore_arity name s.resources

let semaphore_kind name s =
  let rec semaphore_kind name resources =
    match resources with
    | (name',Semaphore (q,_))::remaining when name=name' -> q
    | _::remaining -> semaphore_kind name remaining
    | [] -> raise Not_found
  in semaphore_kind name s.resources

let mutex q s =
  let f (n,r) =
    match r with
    | Mutex q' when q' = q -> Some n
    | _ -> None
  in
  List.filter_map f s.resources

let synchronization s =
  List.filter_map
    (fun (n, r) ->
      match r with
      | Synchronization k -> Some (n,k)
      | _ -> None
    ) s.resources

let monitor s =
  List.filter_map
    (fun (n, r) ->
      match r with
        | Monitor -> Some n
        | _ -> None
    ) s.resources

let empty () =
  {
    resources  = []   ;
    variables  = []   ;
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
      print_process(*2*) (Array.to_list pa.(i).code)
    ]
  in
  let n = Array.length pa in
  for i = 0 to (n-1) do
    answer := (aux i)::!answer
  done;
  String.concat "" (List.rev !answer)

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
    if env.resources = [] then
      add_line (!Highlight.error "\nNo Resource\n")
    else
      let mtx_switch = mutex `Switch env in
      let mtx_rec = mutex `Recursive env in
      let mtx_normal = mutex `Normal env in
      let mtx_ec = mutex `Error_check env in
      if mtx_switch@mtx_rec@mtx_normal@mtx_ec = [] then
	add_line (!Highlight.constant "\nNo Mutex\n")
      else
	(
	  if mutex `Switch env <> [] then
	    (
	      add_line (!Highlight.good_news "\nSwitch Mutex\n");
	      add_line !left_margin; print_list mtx_switch
	    );
	  if mutex `Recursive env <> [] then
	    (
	      add_line (!Highlight.good_news "\nRecursive Mutex\n") ;
	      add_line (!left_margin) ; print_list mtx_rec
	    );
	  if mutex `Normal env <> [] then
	    (
	      add_line (!Highlight.good_news "\nNormal Mutex\n") ;
	      add_line (!left_margin) ; print_list mtx_normal
	    );
	  if mutex `Error_check env <> [] then
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
      let sems = semaphore `Switch env in
      let semq = semaphore `Quantitative env in
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
	    env.equations
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
	  add_line (!left_margin) ; print_list_aux (fun s -> add_line (print_process s)) env.equations
	)
  );
  !answer

let specify_equations semantics =
  print_endline "Which equation would you like to see? ";
  Scanf.scanf "%s"
    (fun s ->
      let the_equations_to_print = Str.split (Str.regexp "[' ']+") s in
      equations semantics (Some the_equations_to_print))

let variables semantics =
  let header =
    match semantics.variables with
    | [] -> !Highlight.error "\nNo variable\n"
    | [_] -> !Highlight.good_news "\nThe unique variable is\n"
    | _ -> !Highlight.good_news "\nThe variables are\n" in
  let variables = String.concat " " semantics.variables
  in Printf.sprintf "%s %s" header variables

let initials semantics =
  let initials = semantics.initials in
  let header =
    match initials with
    | [] -> !Highlight.error "\nNo initial process"
    | [_] -> (!Highlight.good_news "\nThe unique initial process is")^" "
    | _ -> (!Highlight.good_news "\nThe initial processes are")^" "
  in Printf.sprintf "%s%s\n" header (String.concat "|" semantics.initials)

