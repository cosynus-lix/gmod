%{
  (*open Type*)
  open Semantics

  type expr_type = INT | BOOL | UNDEFINED

  let default n = function
    | Some n -> n
    | None -> n

  let se () = Parsing.symbol_start (), Parsing.symbol_end ()

  let flatten_sum = function
    | (il,_)::[] -> il
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
      | _ -> (v,te)
    in
    check_type_homogeneity tv_list

  let collected_variables = ref (Common.StringSet.empty)

  let collect_variable v = collected_variables := Common.StringSet.add v !collected_variables

  let variables () = Common.StringSet.elements !collected_variables

(*let init_runpro x = ignore(Interpreter.init_runpro x); x*)

%}

%token INIT PROCS
%token P V S R M N A F C W T E
%token PLUS MINUS TIMES DIVIDE ABS
%token VOID
%token TINT TBOOL
%token SC DOT DDOT COMMA DEFINE DDEFINE LPAR RPAR LBRC RBRC INVALID_CHARACTER
%token EOF
%token LEQ LS GEQ GS EQ NEQ
%token NOT AND OR QM
%token MONITOR MUTEX MUTEX_REC MUTEX_NORMAL MUTEX_EC SEMAPHORE SEMAPHORE_QUANT SYNCHRONIZATION FIFO LIFO
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

%type <Semantics.t> program
%start program
%%

program:
| declarations PROCS equations inits EOF { (*init_runpro*) {resources = $1; variables = variables (); equations = $3; initials = $4; runpro = [||]; inclusions = []} }
  ;

  declarations:
| declaration declarations { $1@$2 }
| { [] }
  ;

  declaration:
| MUTEX idents { List.map (fun n -> n, Mutex `Switch) $2 }
| MUTEX_REC idents { List.map (fun n -> n, Mutex `Recursive) $2 }
| MUTEX_NORMAL idents { List.map (fun n -> n, Mutex `Normal) $2 }
| MUTEX_EC idents { List.map (fun n -> n, Mutex `Error_check) $2 }
| SEMAPHORE int_opt idents { List.map (fun n -> n, Semaphore (`Switch, default 2 $2)) $3 }
| SEMAPHORE_QUANT int_opt idents { List.map (fun n -> n, Semaphore (`Quantitative, default 2 $2)) $3 }
| SYNCHRONIZATION int_opt idents { List.map (fun n -> n, Synchronization (default 0 $2)) $3 }
| FIFO int_opt idents { (List.map (fun n -> n, Fifo (default 0 $2)) $3)@(List.map (fun n -> n, FifoMutex) $3) }
| LIFO int_opt idents { (List.map (fun n -> n, Lifo (default 0 $2)) $3)@(List.map (fun n -> n, LifoMutex) $3) }
| INCLUDE { [] }
  ;

  int_opt:
| INT { Some $1 }
| { None }
  ;

  idents:
| IDENT idents { $1::$2 }
| IDENT { [$1] }
  ;

  equations:
| equation equations { $1::$2 }
|  { [] }
  ;

  equation:
| IDENT DEFINE sum { $1,(flatten_sum $3) }
  ;

  sum:
| sequence PLUS expr_opt sum { ($1,$3)::$4 }
| sequence { [($1,Bc true)] }
  ;

  sequence:
| sequence DOT instruction { $1@$3 }
| instruction { $1 }
  ;

  instruction:
| LPAR RPAR { [] }
| LPAR sum RPAR { flatten_sum $2 }
| IDENT { [C $1, se ()] }
| P LPAR IDENT RPAR { [P $3, se ()] }
| V LPAR IDENT RPAR { [V $3, se ()] }
| S LPAR IDENT             RPAR { [S ($3,""), se ()] }
| S LPAR IDENT COMMA MSG   RPAR { [S ($3,$5), se ()] }
| R LPAR IDENT RPAR             { [R ($3,""), se ()] }
| R LPAR IDENT COMMA IDENT RPAR { [R ($3,$5), se ()] }
| M LPAR IDENT COMMA IDENT RPAR { [M ($3,$5), se ()] }
| N LPAR IDENT RPAR { [N $3, se ()] }
| A LPAR IDENT RPAR { [A $3, se ()] }
| C LPAR IDENT RPAR { [C $3, se ()] }
| W LPAR IDENT RPAR { [W $3, se ()] }
| E LPAR IDENT RPAR { [E $3, se ()] }
| T LPAR IDENT COMMA   expr RPAR { collect_variable $3; [T ($3,fst $5), se ()] }
|        IDENT DDEFINE expr      { [T ($1,fst $3), se ()] }
| F LPAR IDENT RPAR { [F $3, se ()] }
  ;

  expr_opt:
| LBRC expr RBRC PLUS { fst(check_type_homogeneity [snd $2] BOOL (fst $2)) }
| { Unknown }
  ;

  expr:
| IDENT                  { collect_variable $1; (Va $1),UNDEFINED }
| IDENT DDOT TINT        { collect_variable $1; (Va $1),INT }
| IDENT DDOT TBOOL       { collect_variable $1; (Va $1),BOOL }
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
| IDENT init_list { $1::$2 }
| INT TIMES IDENT init_list { (Common.List.make $1 $3)@$4 }
| INT IDENT init_list { (Common.List.make $1 $2)@$3 }
| IDENT OR init_list { $1::$3 }
| { [] }
  ;
