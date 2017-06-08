%{

  open AbstractSyntax_without_parser

  type expr_type = INT | BOOL | UNDEFINED

  let default n = function
    | Some n -> n
    | None -> n

  let se () = Parsing.symbol_start (), Parsing.symbol_end ()

  let flatten_sum = function
    | (_,il)::[] -> il
    | sum        -> [ITE sum, se ()]

  let check_type_conformity tv_list te v =
    if List.for_all (fun t -> t = te || t = UNDEFINED) tv_list
    then (v,te)
    else failwith "Type mismatch"

  let check_type_homogeneity tv_list te v =
    let rec check_type_homogeneity tv_list = match tv_list with
      | t::(t'::tv_list as remaining) ->
          if t = t' || t = UNDEFINED || t' = UNDEFINED
          then check_type_homogeneity remaining
          else failwith "Type mismatch"
      | _ -> (v,te) in
    check_type_homogeneity tv_list

  let declarations = ref []

  exception Output of resource_kind

  let fifo_lifo name =
    try
      List.iter (fun (n,rk) ->
        if is_lifo rk
        then raise (Output rk)
        else (
          if is_fifo rk
          then raise (Output rk))) !declarations ;
      failwith (Printf.sprintf "The resource %S was not declared " name )
    with Output rk -> rk

  let collect_declarations names t =
    let current_declaration = ref None in
    let f name =
      if List.exists
        (fun r -> current_declaration := Some r ; colliding_declarations r (name , t))
        !declarations
      then raise (PV_error (Printf.sprintf "Declarations %s and %s collide" (string_of_resource (name,t))
        (match !current_declaration with Some r -> string_of_resource r | None -> "")))
      else declarations := (name,t) :: !declarations in
    List.iter f names

  let collected_resources = ref Sod.empty

  let collected_calls = ref Common.StringSet.empty

  let equations = ref []

  let collect_resource res = collected_resources := Sod.add res !collected_resources

  let collect_call c = collected_calls := Common.StringSet.add c !collected_calls

  let collect_equation e = equations := e :: !equations

  let resources () =
    let return = !collected_resources in
    let () = collected_resources := Sod.empty in
    return

  let calls () =
    let return = Common.StringSet.elements !collected_calls in
    let () = collected_calls := Common.StringSet.empty in
    return

  let resource_of_string ident =
    try
      List.iter (fun (n,rk) -> if ident = n then raise (Output rk)) !declarations ;
      failwith (Printf.sprintf "The resource %S was not declared" ident )
    with Output rk -> (ident , rk)

  let mutex_or_semaphore_of_string ident =
    try
      List.iter
        (fun (n,rk) -> if ident = n && (is_mutex rk || is_semaphore rk) then raise (Output rk)) !declarations ;
      failwith (Printf.sprintf "The resource %S was not declared" ident )
    with Output rk -> (ident , rk)

  (*Actions*)

  module Action = struct

    let program (*equations*) initials =
      { declarations = !declarations ;
        equations = !equations ;
        initials ; runpro = [||] ;
        inclusions = []}

    let send ident msg =
      let res = resource_of_string ident in
      collect_resource res ;
      [S (res,msg), se ()]

    let receive ident container =
      let res = resource_of_string ident in
      collect_resource res ;
      [R (res,container), se ()]

    let monitor ident smth =
      let res = resource_of_string ident in
      collect_resource res ;
      [M (res,smth), se ()]

    let notify_some ident =
      let res = (ident,Notify `Some) in
      collect_resource res ;
      [N res, se ()]

    let notify_all ident =
      let res = (ident,Notify `All) in
      collect_resource res ;
      [N res, se ()]

    let synchronize ident =
      let res = resource_of_string ident in
      collect_resource res ;
      [W res, se ()]

    let take ident =
      let res = mutex_or_semaphore_of_string ident in
      collect_resource res ;
      [P res, se ()]

    let release ident =
      let res = mutex_or_semaphore_of_string ident in
      collect_resource res ;
      [V res, se ()]

    let call procname =
      collect_call procname ;
      [C procname, se ()]

    let alter ident expr =
      collect_resource (ident , (Variable `Write)) ;
      [T (ident,fst expr), se ()]

  end(*Action*)

(*
  let verbose = ref false
*)

  let triggered s = if !Parser_pv_flags.parser_verbose then print_endline s

  let one_parameter s p = if !Parser_pv_flags.parser_verbose then Printf.printf "%s %s\n" s p

%}

%token INIT
%token P V S R M N A F C W T E
%token PLUS MINUS TIMES DIVIDE ABS MODULO
%token VOID
%token TINT TBOOL
%token SC DOT DDOT COMMA DEFINE DDEFINE LPAR RPAR LBRC RBRC INVALID_CHARACTER
%token EOF
%token LEQ LS GEQ GS EQ NEQ
%token NOT AND OR QM
%token MONITOR MUTEX_DEFAULT MUTEX_REC MUTEX_NORMAL MUTEX_EC SEMAPHORE SEMAPHORE_QUANT SYNCHRONIZATION FIFO LIFO PROCESS
%token <bool>   BOOL
%token <string> INCLUDE
%token <string> IDENT
%token <int>    INT
%token <string> MSG

%right DDEFINE
%left OR
%left AND
%left MINUS
%left PLUS
%left DOT
%left DIVIDE
%left TIMES
%left EQ LEQ LS GEQ GS NEQ DEFINE
%left DDOT
%nonassoc NOT
%nonassoc ABS

%type <AbstractSyntax_without_parser.t> program
%start program
%%

  program:
| declarations inits EOF { triggered "program" ; Action.program $2 }
  ;

  declarations:
| declaration declarations { triggered "declarations recursive" }
| { triggered "declarations terminal" }
  ;

  declaration:
| MUTEX_DEFAULT idents { triggered "declaration MUTEX_DEFAULT"   ; collect_declarations $2 (Mutex `Default)     }
| MUTEX_REC idents     { triggered "declaration MUTEX_RECURSIVE" ; collect_declarations $2 (Mutex `Recursive)   }
| MUTEX_NORMAL idents  { triggered "declaration MUTEX_NORMAL"    ; collect_declarations $2 (Mutex `Normal)      }
| MUTEX_EC idents      { triggered "declaration MUTEX_EC"        ; collect_declarations $2 (Mutex `Error_check) }

| SEMAPHORE int_opt idents       { triggered "declaration SEMAPHORE"       ; collect_declarations $3 (Semaphore (`Default     , default 2 $2)) }
| SEMAPHORE_QUANT int_opt idents { triggered "declaration SEMAPHORE_QUANT" ; collect_declarations $3 (Semaphore (`Quantitative, default 2 $2)) }

| SYNCHRONIZATION int_opt idents { triggered "declaration SYNCHRONIZATION" ; collect_declarations $3 (Synchronization (default 0 $2)) }

| FIFO int_opt idents { triggered "declaration FIFO" ; collect_declarations $3 (Channel (`Fifo , default 0 $2) ) }
| LIFO int_opt idents { triggered "declaration LIFO" ; collect_declarations $3 (Channel (`Lifo , default 0 $2) ) }

| PROCESS equations { triggered "declaration PROCESS" }

| INCLUDE { () }
  ;

  int_opt:
| INT { Some $1 }
| { None }
  ;

  idents:
| IDENT idents { triggered "idents 1" ; $1 :: $2 }
| IDENT { triggered "idents 2" ; [$1] }
  ;

  equations:
| equation equations { triggered "equations 1" }
| {triggered "equations 2" }
  ;

  equation:
| IDENT DEFINE sum { if !Parser_pv_flags.parser_verbose then Printf.printf "equation %s\n" $1 ; collect_equation { name = $1 ; resources = resources () ; calls = calls () ; body = flatten_sum $3 } }
  ;

  sum:
| sequence PLUS expr_opt sum { triggered "sum PLUS" ; ($3 , $1) :: $4 }
| sequence { triggered "sum terminal" ; [(Bc true , $1)] }
  ;

  sequence:
| sequence DOT instruction { triggered "sequence recursive" ; $1 @ $3 }
| instruction { triggered "sequence terminal" ; $1 }
  ;

  instruction:
| LPAR RPAR { [] }
| LPAR sum RPAR { flatten_sum $2 }
| IDENT { collect_call $1 ; [C $1, se ()] }
| P LPAR IDENT RPAR             { triggered ("instruction P(" ^ $3 ^ ")") ; Action.take $3 }
| V LPAR IDENT RPAR             { triggered ("instruction V(" ^ $3 ^ ")") ; Action.release $3 }
| S LPAR IDENT             RPAR { triggered ("instruction S(" ^ $3 ^ ")") ; Action.send $3 "" }
| S LPAR IDENT COMMA MSG   RPAR { triggered ("instruction S(" ^ $3 ^ "," ^ $5 ^ ")") ; Action.send $3 $5 }
| R LPAR IDENT RPAR             { triggered ("instruction R(" ^ $3 ^ ")") ; Action.receive $3 "" }
| R LPAR IDENT COMMA IDENT RPAR { triggered ("instruction R(" ^ $3 ^ "," ^ $5 ^ ")") ; Action.receive $3 $5 }
| M LPAR IDENT COMMA IDENT RPAR { triggered ("instruction M(" ^ $3 ^ "," ^ $5 ^ ")") ; Action.monitor $3 $5 }
| N LPAR IDENT RPAR { triggered ("instruction N(" ^ $3 ^ ")") ; Action.notify_some $3 }
| A LPAR IDENT RPAR { triggered ("instruction A(" ^ $3 ^ ")") ; Action.notify_all $3 }
| C LPAR IDENT RPAR { triggered ("instruction C(" ^ $3 ^ ")") ; Action.call $3 }
| W LPAR IDENT RPAR { triggered ("instruction W(" ^ $3 ^ ")") ; Action.synchronize $3 }
| E LPAR IDENT RPAR { triggered ("instruction E(" ^ $3 ^ ")") ; [E $3, se ()] }
| T LPAR IDENT COMMA   expr RPAR { triggered ("instruction T(" ^ $3 ^ ",...)") ; Action.alter $3 $5 }
|        IDENT DDEFINE expr      { triggered ("instruction " ^ $1 ^ " := " ^ "...") ; Action.alter $1 $3 }
| F LPAR IDENT RPAR { triggered ("instruction F(" ^ $3 ^ ")") ; [F $3, se ()] }
  ;

  expr_opt:
| LBRC expr RBRC PLUS { fst(check_type_homogeneity [snd $2] BOOL (fst $2)) }
| LBRC RBRC PLUS { Unknown }
| { Unknown }
  ;

  expr:
| IDENT                  { collect_resource ($1,(Variable `Read)) ; (Va $1),UNDEFINED }
| IDENT DDOT TINT        { collect_resource ($1,(Variable `Read)) ; (Va $1),INT }
| IDENT DDOT TBOOL       { collect_resource ($1,(Variable `Read)) ; (Va $1),BOOL }
| QM DDOT TINT           { Unknown,INT }
| QM DDOT TBOOL          { Unknown,BOOL }
| QM                     { Unknown,BOOL }
| INT                    { (Ac $1),INT }
| MINUS expr             { check_type_conformity [snd $2] INT (Ne (fst $2)) }
| ABS   expr             { check_type_conformity [snd $2] INT (Ab (fst $2)) }
|       expr PLUS   expr { check_type_conformity [snd $1;snd $3] INT (Pl (fst $1,fst $3)) }
|       expr MINUS  expr { check_type_conformity [snd $1;snd $3] INT (Mi (fst $1,fst $3)) }
|       expr TIMES  expr { check_type_conformity [snd $1;snd $3] INT (Ti (fst $1,fst $3)) }
|       expr DIVIDE expr { check_type_conformity [snd $1;snd $3] INT (Di (fst $1,fst $3)) }
|       expr MODULO expr { check_type_conformity [snd $1;snd $3] INT (Mo (fst $1,fst $3)) }
| BOOL                   { (Bc $1),BOOL }
| NOT   expr             { check_type_conformity [snd $2] BOOL (No (fst $2)) }
|       expr AND    expr { check_type_conformity [snd $1;snd $3] BOOL (An (fst $1,fst $3)) }
|       expr OR     expr { check_type_conformity [snd $1;snd $3] BOOL (Or (fst $1,fst $3)) }
|       expr DEFINE expr { check_type_homogeneity [snd $1;snd $3] BOOL (Eq (fst $1,fst $3)) }
|       expr EQ expr     { check_type_homogeneity [snd $1;snd $3] BOOL (Eq (fst $1,fst $3)) }
|       expr LEQ    expr { check_type_homogeneity [snd $1;snd $3] BOOL (Lq (fst $1,fst $3)) }
|       expr LS     expr { check_type_homogeneity [snd $1;snd $3] BOOL (Ls (fst $1,fst $3)) }
|       expr GEQ    expr { check_type_homogeneity [snd $1;snd $3] BOOL (Gq (fst $1,fst $3)) }
|       expr GS     expr { check_type_homogeneity [snd $1;snd $3] BOOL (Gs (fst $1,fst $3)) }
|       expr NEQ    expr { check_type_homogeneity [snd $1;snd $3] BOOL (Nq (fst $1,fst $3)) }
| LPAR  expr RPAR        { $2 }
  ;

  inits:
| INIT init_list { $2 }
  ;

  init_list:
| IDENT init_list { $1 :: $2 }
| INT TIMES IDENT init_list { (Common.List.make $1 $3) @ $4 }
| INT IDENT init_list { (Common.List.make $1 $2) @ $3 }
| IDENT OR init_list { $1 :: $3 }
| { [] }
  ;
