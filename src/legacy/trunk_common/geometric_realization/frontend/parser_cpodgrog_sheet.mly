%{

  open Expression

  let display_how_many_entries (m1,m2) =
    if false
    then 
      let n2 = fold (fun key x accu -> accu+1) m2 0 in
      let n1 = List.length m1 in
      Printf.printf 
	"The list has %i %s.\nThe map has %i %s.\n" 
	n1 
	(if n1>1 then "entries" else "entry") 
	n2 
	(if n2>1 then "entries" else "entry")

  let display_constant aux = if false then Printf.printf "Constant =\n%s" (Message.yellow ~bold:true ("\""^(BuiltInWithoutParsers.Area.string_of aux)^"\"\n"))



%}

%token <string> IDENT QUOTED ERROR
%token <BuiltInWithoutParsers.ODA.OverInteger.Sh.t*bool> AREA
%token <int> INT
%token DOUBLEDASH ARROW SEMICOLON COLON END
%token LPAR RPAR LCURL RCURL LSQBR RSQBR
%token EQUAL
%token INTERSECTION UNION DIFFERENCE COMPLEMENT CLOSURE INTERIOR FUTURE PAST COMPARE
%token TIMES FACTORIZE
%left COMPARE FACTORIZE
%left FUTURE PAST
%left INTERIOR
%left CLOSURE
%left DIFFERENCE
%left UNION
%left INTERSECTION
%left COMPLEMENT
%left TIMES
%start output
%type <(string list)*(Expression.Area.operator Map.Make(String).t)> output
%%

expression:
| INT {Area.Int $1}
| IDENT {Area.Variable $1}
| QUOTED {Area.Constant (let aux = BuiltInAuxiliary.CPODGROG.OverInteger.of_string $1 in display_constant aux;aux)}
| LPAR       expression   RPAR       {$2}
| expression INTERSECTION expression {Area.Intersection ($1,$3)}
| expression UNION        expression {Area.Union ($1,$3)}
| expression DIFFERENCE   expression {Area.Difference ($1,$3)}
|            DIFFERENCE   expression {Area.Complement ($2)}
|            COMPLEMENT   expression {Area.Complement ($2)}
|            CLOSURE      expression {Area.Closure $2}
|            INTERIOR     expression {Area.Interior $2}
|            FACTORIZE    expression {Area.Factorize $2}
| expression FUTURE       expression {Area.Future ($1,$3)}
| expression PAST         expression {Area.Past ($1,$3)}
| expression TIMES        expression {Area.Product ($1,$3)}
| COMPARE LPAR expression RPAR LPAR expression RPAR {Area.Compare ($3,$6)}
  ;

  declarations:
| IDENT EQUAL expression SEMICOLON declarations 
      {($1::fst $5),Expression.add $1 $3 (snd $5)}
| IDENT EQUAL expression           declarations 
	  {($1::fst $4),Expression.add $1 $3 (snd $4)}
| IDENT EQUAL expression SEMICOLON
	      {[$1],Expression.add $1 $3 Expression.empty}
| IDENT EQUAL expression
		  {[$1],Expression.add $1 $3 Expression.empty}
  ;

  output:
| declarations END {display_how_many_entries $1;$1}
