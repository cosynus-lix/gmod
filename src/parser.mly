%{
    open Lang
%}

%token LPAR RPAR LACC RACC SEMICOLON COMMA
%token IF THEN ELSE RETURN NOT WHILE
%token T_VOID T_BOOL T_INT T_MUTEX
%token ISEQ LE LT EQ PLUS MINUS MULT AND OR
%token ASSERT
%token EOF
%token SPAWN P V ATOMIC
%token <string> IDENT
%token <int> V_INT
%token <bool> V_BOOL

%nonassoc NOT
%nonassoc ELSE
%nonassoc LE
%left ISEQ
%left PLUS
%left DOT SEMICOLON

%start decls
%type <Lang.D.t> decls
%%

decls:
    | declarations EOF { $1 }

declarations:
    | { [] }
    | declaration declarations { $1::$2 }

declaration:
    | typ IDENT LPAR declaration_arguments RPAR instr { D.fct $1 $2 (Array.of_list $4) $6 }
    | typ IDENT EQ instr { D.cst $1 $2 $4 }

declaration_arguments:
    | { [] }
    | typ IDENT declaration_arguments { ($1,$2)::$3 }

instrs:
    | instr SEMICOLON instrs { $1::$3 }
    | instr { [$1] }
    | { [] }

instr:
    | command { P.Cmd $1 }
    | P LPAR IDENT RPAR { P.P $3 }
    | V LPAR IDENT RPAR { P.V $3 }
    | LACC instrs RACC { P.Seq $2 }
    | IF expr instr ELSE instr { P.If ($2,$3,$5) }
//    | IF expr instr { P.If ($2,$3,P.Seq []) }
    | WHILE expr instr { P.While ($2,$3) }
    | IDENT LPAR expr_list RPAR { P.Call ($1,$3) }
    | SPAWN instr { P.Spawn $2 }
    | RETURN expr { P.Return $2 }

command:
    | ASSERT expr { E.Assert $2 }
    | IDENT EQ expr { E.Assign ($1, $3) }
    | typ IDENT { E.New_var ($1, $2, None) }

typ:
    | T_VOID { T.Void }
    | T_BOOL { T.Bool }
    | T_INT { T.Int }
    | T_MUTEX { T.Mutex }

expr:
    | IDENT { E.Var $1 }
    | V_INT { E.Val (E.Int $1) }
    | V_BOOL { E.Val (E.Bool $1) }
//    | NOT expr { E.Not $2 }
//    | expr PLUS expr { E.Add ($1,$3) }
//    | expr MINUS expr { E.Sub ($1,$3) }
//    | expr MULT expr { EMult ($1,$3) }
//    | expr ISEQ expr { EIs_eq ($1,$3) }
//    | expr AND expr { EAnd ($1,$3) }
//    | expr OR expr { EOr ($1,$3) }
//    | expr LE expr { ELe ($1,$3) }
//    | expr LT expr { ELt ($1,$3) }
    | LPAR expr RPAR { $2 }

expr_list:
    | { [] }
    | expr { [$1] }
    | expr COMMA expr_list { $1::$3 }
