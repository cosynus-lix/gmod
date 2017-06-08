%{

  open Expression

  let display_how_many_entries (m1,m2) =
    if false
    then
      let n2 = fold (fun key x accu -> accu+1) m2 0 in
      let n1 = List.length m1 in
      Printf.printf "The list has %i %s.\nThe map has %i %s.\n" n1 (if n1>1 then "entries" else "entry") n2 (if n2>1 then "entries" else "entry")

  let display_constant aux = if false then Printf.printf "Constant =\n%s" (Message.yellow (BuiltIn.DGROG.OverInteger.string_of aux))

%}

%token <string> IDENT QUOTED ERROR
%token <BuiltInWithoutParsers.ODA.OverInteger.Sh.t*bool> AREA
%token DOUBLEDASH ARROW SEMICOLON COLON END
%token LPAR RPAR LCURL RCURL LSQBR RSQBR
%token EQUAL
%token INTERSECTION UNION DIFFERENCE CLOSURE INTERIOR FUTURE PAST FUTURE_CLOSURE PAST_CLOSURE COMPLEMENT
%token LESS_OR_EQUAL GREATER_OR_EQUAL STRICTLY_LESS STRICTLY_GREATER
%left EQUAL
%left LESS_OR_EQUAL GREATER_OR_EQUAL STRICTLY_GREATER STRICTLY_LESS
%left FUTURE PAST
%left INTERIOR CLOSURE
%left UNION
%left INTERSECTION
%left DIFFERENCE
%left COMPLEMENT
%start output
%type <(string list)*(Expression.DGROG.operator Map.Make(String).t)> output
%%

expression:
| IDENT                                  {DGROG.Variable $1}
| QUOTED                                 {DGROG.Constant (let aux = BuiltInAuxiliary.DGROG.OverInteger.of_string $1 in display_constant aux;aux)}
| expression INTERSECTION     expression {DGROG.Intersection ($1,$3)}
| expression UNION            expression {DGROG.Union ($1,$3)}
| expression DIFFERENCE       expression {DGROG.Difference ($1,$3)}
| expression LESS_OR_EQUAL    expression {DGROG.Less_or_equal ($1,$3)}
| expression GREATER_OR_EQUAL expression {DGROG.Greater_or_equal ($1,$3)}
| expression STRICTLY_LESS    expression {DGROG.Strictly_less ($1,$3)}
| expression STRICTLY_GREATER expression {DGROG.Strictly_greater ($1,$3)}
|            COMPLEMENT       expression {DGROG.Complement $2}
| CLOSURE    expression                  {DGROG.Closure $2}
| INTERIOR   expression                  {DGROG.Interior $2}
| expression FUTURE           expression {DGROG.Future ($1,$3)}
| expression PAST             expression {DGROG.Past ($1,$3)}
|            FUTURE_CLOSURE   expression {DGROG.FutureClosure $2}
|            PAST_CLOSURE     expression {DGROG.PastClosure $2}
| LPAR       expression       RPAR       {$2}
  ;

  declarations:
| IDENT EQUAL expression SEMICOLON declarations {($1::fst $5),Expression.add $1 $3 (snd $5)}
| IDENT EQUAL expression declarations           {($1::fst $4),Expression.add $1 $3 (snd $4)}
| IDENT EQUAL expression SEMICOLON              {[$1],Expression.add $1 $3 Expression.empty}
| IDENT EQUAL expression                        {[$1],Expression.add $1 $3 Expression.empty}
  ;

  output:
| declarations END {display_how_many_entries $1;$1}
