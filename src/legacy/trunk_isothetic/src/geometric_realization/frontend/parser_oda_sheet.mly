%{

  open Expression.ODA
  let variable name = Variable name
  let interval b1 b2 x1 x2 = Interval (b1,b2,x1,x2)
  let final b x e = Final (b,x,e) 
  let atom x = Atom x
  let coatom x = Coatom x
  let add_int_point x (lx,b) = (x::lx),b
  let add_infinity_point (lx,b) = lx,true
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
  let compare e1 e2 = Compare (e1,e2)
  let less_or_equal e1 e2 = Less_or_equal (e1,e2)
  let greater_or_equal e1 e2 = Greater_or_equal (e1,e2)
  let strictly_less e1 e2 = Strictly_less (e1,e2)
  let strictly_greater e1 e2 = Strictly_greater (e1,e2)
  let empty = Expression.empty
  let add_declaration k v accu = (k::(fst accu)),(Expression.add k v (snd accu))
  let initial k v = [k],(Expression.add k v empty)
  let output support (domain,moe) = domain,
    (
      Expression.map
	(
	  fun (so,expr) ->
	    match so with
	      | None -> (Some support),expr 
	      | x -> x,expr 
	) moe
    )
  let output support (domain,moe) = 
    domain,Expression.map (fun expr -> (Some support),expr) moe
  let output_without_support (domain,moe) = 
    domain,Expression.map (fun expr -> None,expr) moe

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
%token <BuiltInWithoutParsers.support option*Expression.ODA.operator> VALUE
%token <BuiltInWithoutParsers.support> SUPPORT

%left COMPARE
%left LESS_OR_EQUAL GREATER_OR_EQUAL STRICTLY_LESS STRICTLY_GREATER
%left FUTURE PAST
%left CLOSURE INTERIOR
%left DIFFERENCE
%left UNION
%left INTERSECTION
%left COMPLEMENT

%start output
%type <string list*(BuiltInWithoutParsers.support option*Expression.ODA.operator) Expression.t> output
%%
  ;

  int_infinity_list:
  | INTEGER  COMMA int_infinity_list {add_int_point $1 $3}
  | INFINITY COMMA int_infinity_list {add_infinity_point $3}
  | INTEGER  int_infinity_list       {add_int_point $1 $2}
  | INFINITY int_infinity_list       {add_infinity_point $2}
  | INTEGER                          {add_int_point $1 ([],false)}
  | INFINITY                         {add_infinity_point ([],false)}
  ;

  atom:
  | RSQBR INTEGER COMMA INTEGER RSQBR  {interval false true  $2 $4}
  | RSQBR INTEGER COMMA INTEGER LSQBR  {interval false false $2 $4}
  | LSQBR INTEGER COMMA INTEGER RSQBR  {interval true true   $2 $4}
  | LSQBR INTEGER COMMA INTEGER LSQBR  {interval true false  $2 $4}
  | RSQBR INTEGER COMMA INFINITY RSQBR {final false $2 true }
  | RSQBR INTEGER COMMA INFINITY LSQBR {final false $2 false}
  | LSQBR INTEGER COMMA INFINITY RSQBR {final true  $2 true }
  | LSQBR INTEGER COMMA INFINITY LSQBR {final true  $2 false}
  | LCURL int_infinity_list RCURL      {atom $2}
  | RCURL int_infinity_list LCURL      {coatom $2}
  | CIRCLE                             {circle ()}
  | HALFLINE                           {halfline ()}
  | COMPACTHALFLINE                    {compacthalfline ()}
  ;

  atom_list:
  | atom_list atom {union $1 $2}
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
  | expression LESS_OR_EQUAL    expression {less_or_equal $1 $3}
  | expression GREATER_OR_EQUAL expression {greater_or_equal $1 $3}
  | expression STRICTLY_LESS    expression {strictly_less $1 $3}
  | expression STRICTLY_GREATER expression {strictly_greater $1 $3}
  | COMPARE LPAR expression RPAR LPAR expression RPAR {compare $3 $6}
  ;

  declarations:
  | IDENT EQUAL expression declarations {add_declaration $1 $3 $4}
  | IDENT EQUAL expression {add_declaration $1 $3 ([],empty)}
  ;

  output:
  | SUPPORT declarations END {output $1 $2}
  | SUPPORT              END {output $1 ([],empty)}
  | declarations         END {output_without_support $1}
  | END                  {[],empty}
