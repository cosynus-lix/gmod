%{

  open BuiltInWithoutParsers
  open Expression.ODA
  
  let variable name = Variable name
  let interval b1 b2 x1 x2 = Interval (b1,b2,x1,x2)
  let final b x e = Final (b,x,e) 
  let atom x = Atom x
  let coatom x = Coatom x
  let circle () = Circle
  let halfline () = HalfLine
  let compacthalfline () = CompactHalfLine
  let complement e = Complement e
  let intersection e1 e2 = Intersection (e1,e2)
  let union e1 e2 = Union (e1,e2)
  let difference e1 e2 = Difference (e1,e2)
  let future e1 e2 = Future (e1,e2)
  let past e1 e2 = Past (e1,e2)
  let closure e = Closure e
  let interior e = Interior e
  let add_int_point x (lx,b) = (x::lx),b
  let add_infinity_point (lx,b) = lx,true

%}

%token <string> IDENT ERROR
%token <int> INTEGER
%token COMMA SEMICOLON EQUAL END
%token UNION INTERSECTION DIFFERENCE COMPLEMENT
%token LPAR RPAR LCURL RCURL LSQBR RSQBR
%token CLOSURE INTERIOR
%token FUTURE PAST
%token INFINITY CIRCLE HALFLINE COMPACTHALFLINE
%token COMPARE LESS_OR_EQUAL GREATER_OR_EQUAL STRICTLY_LESS STRICTLY_GREATER
%token <BuiltInWithoutParsers.support option> SUPPORT

%left COMPARE 
%left LESS_OR_EQUAL GREATER_OR_EQUAL STRICTLY_LESS STRICTLY_GREATER
%left FUTURE PAST
%left CLOSURE INTERIOR
%left DIFFERENCE
%left UNION
%left INTERSECTION
%left COMPLEMENT

%start output
%type <BuiltInWithoutParsers.support option*Expression.ODA.operator> output
%%
  ;

  int_infinity_list:
  | INTEGER int_infinity_list  {add_int_point $1 $2}
  | INFINITY int_infinity_list {add_infinity_point $2}
  | INTEGER                    {add_int_point $1 ([],false)}
  | INFINITY                   {add_infinity_point ([],false)}
  ;

  atom:
  | RSQBR INTEGER COMMA INTEGER RSQBR {interval false true  $2 $4}
  | RSQBR INTEGER COMMA INTEGER LSQBR {interval false false $2 $4}
  | LSQBR INTEGER COMMA INTEGER RSQBR {interval true true   $2 $4}
  | LSQBR INTEGER COMMA INTEGER LSQBR {interval true false  $2 $4}
  | RSQBR INTEGER COMMA INFINITY RSQBR {final false $2 true }
  | RSQBR INTEGER COMMA INFINITY LSQBR {final false $2 false}
  | LSQBR INTEGER COMMA INFINITY RSQBR {final true  $2 true }
  | LSQBR INTEGER COMMA INFINITY LSQBR {final true  $2 false}
  | LCURL int_infinity_list RCURL {atom $2}
  | RCURL int_infinity_list LCURL {coatom $2}
  | CIRCLE   {circle ()}
  | HALFLINE {halfline ()}
  | COMPACTHALFLINE {compacthalfline ()}
  ;

  atom_list:
  | atom atom_list {union $1 $2}
  | atom           {$1}
  ;

  expression:
  | IDENT {variable $1}
  | atom_list {$1}
  | LPAR expression RPAR {$2}
  | DIFFERENCE expression {complement $2}
  | COMPLEMENT expression {complement $2}
  | CLOSURE    expression {closure $2}
  | INTERIOR   expression {interior $2}
  | expression INTERSECTION     expression {intersection $1 $3}
  | expression UNION            expression {union $1 $3}
  | expression DIFFERENCE       expression {difference $1 $3}
  | expression FUTURE           expression {future $1 $3}
  | expression PAST             expression {past $1 $3}
  ;

  output:
  | expression         END {None,$1}
  | SUPPORT expression END {$1,$2}
  ;
