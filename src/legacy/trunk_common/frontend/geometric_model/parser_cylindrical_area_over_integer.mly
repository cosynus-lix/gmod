%{
  open Type

  let variable s = match s with
  | "S" -> Clx.Brick(X.of_array [|M.full M.circle|])
  | "H" -> Clx.Brick(X.of_array [|M.full M.line|])
  | _   -> Clx.Variable(s)

  let full_of_binary_word s =
    let answer = ref [||]
    in
      (
	for i = 0 to ((String.length s)-1)
	do
	  match s.[i] with 
	    | '0' -> answer := Array.append !answer [|M.full M.circle|]
	    | '1' -> answer := Array.append !answer [|M.full M.line|]
	    | _   -> ()
	done
	;
	X.of_array !answer
      )

  let empty_of_binary_word s =
    let answer = ref [||]
    in
      (
	for i = 0 to ((String.length s)-1)
	do
	  match s.[i] with 
	    | '0' -> answer := Array.append !answer [|M.empty M.circle|]
	    | '1' -> answer := Array.append !answer [|M.empty M.line|]
	    | _   -> ()
	done
	;
	X.normalize (X.of_array (!answer))
      )

%} /* clôture de l'en-tête */

%token OPENPAR CLOSEPAR OSQB CSQB OCRB CCRB DEFINE END
%token UNION INTERSECTION PRODUCT DIFFERENCE MINUS
%token COMPLEMENT DOWNWARD UPWARD INTERIOR CLOSURE BOUNDARY 
%token COMPRESS OPTIMIZE UPPER_CORNERS DEADLOCKS REACHABLE 
%token MCTD MCTI GINZU PAST_CONE FUTURE_CONE DEADLOCK_ATTRACTOR INFINITY_ATTRACTOR
%token ORDER_CONVEX CUBE NORMALIZE
%token EXPONENT EMBED
%token DEFINE EQUAL SC POINT COMMA
%token < string > IDNAME
%token < int > INT
%token < string > FULL
%token < string > VOID
%token < Type.X.t > BRICK
%left EQUAL
%left PAST_CONE FUTURE_CONE
%left DIFFERENCE
%left UNION
%left INTERSECTION
%left PRODUCT
%left EXPONENT
%left COMMA
%nonassoc POINT COMPLEMENT DOWNWARD UPWARD CUBE INTERIOR CLOSURE ORDER_CONVEX BOUNDARY NORMALIZE COMPRESS UPPER_CORNERS DEADLOCKS REACHABLE MCTD MCTI GINZU DEADLOCK_ATTRACTOR INFINITY_ATTRACTOR EMBED
/*point d'entrée de l'automate de reconnaissance du langage*/
%start result
%type < ( (string list) * (Type.Clx.t Type.Mos.t) ) > result
%%

  aux:
|     IDNAME                      {variable $1}
|     VOID                        {Clx.Brick(empty_of_binary_word $1)} 
|     FULL                        {Clx.Brick(full_of_binary_word $1)}
| OPENPAR  INT COMMA INT CLOSEPAR {Clx.Brick(X.of_list [M.bounded true  true  $2 $4 M.circle])}
| OPENPAR  INT COMMA INT OPENPAR  {Clx.Brick(X.of_list [M.bounded true  false $2 $4 M.circle])}
| CLOSEPAR INT COMMA INT OPENPAR  {Clx.Brick(X.of_list [M.bounded false false $2 $4 M.circle])}
| CLOSEPAR INT COMMA INT CLOSEPAR {Clx.Brick(X.of_list [M.bounded false true  $2 $4 M.circle])}
| OSQB INT COMMA INT CSQB         {Clx.Brick(X.of_list [M.bounded true  true  $2 $4 M.line])}
| OSQB INT COMMA INT OSQB         {Clx.Brick(X.of_list [M.bounded true  false $2 $4 M.line])}
| CSQB INT COMMA INT OSQB         {Clx.Brick(X.of_list [M.bounded false false $2 $4 M.line])}
| CSQB INT COMMA INT CSQB         {Clx.Brick(X.of_list [M.bounded false true  $2 $4 M.line])}
| OSQB INT COMMA MINUS OSQB       {Clx.Brick(X.of_list [M.terminal true  $2])}
| CSQB INT COMMA MINUS OSQB       {Clx.Brick(X.of_list [M.terminal false $2])}
| OSQB INT CSQB                   {Clx.Brick(X.of_list [M.atom M.line $2])}
| OPENPAR INT CLOSEPAR            {Clx.Brick(X.of_list [M.atom M.circle $2])}
| CLOSEPAR INT OPENPAR            {Clx.Brick(X.of_list [M.complement (M.atom M.circle $2)])}
| aux UNION              aux      {Clx.Union($1,$3)}
| aux INTERSECTION       aux      {Clx.Intersection($1,$3)}
| aux DIFFERENCE         aux      {Clx.Difference($1,$3)}
|     COMPLEMENT         aux      {Clx.Complement($2)} 
|     OCRB               aux CCRB {$2}
|     DOWNWARD           aux      {Clx.Downward($2)}
|     UPWARD             aux      {Clx.Upward($2)}
|     UPPER_CORNERS      aux      {Clx.Upper_corners($2)}
|     INTERIOR           aux      {Clx.Interior($2)}
|     CLOSURE            aux      {Clx.Closure($2)}
|     CUBE               aux      {Clx.Cube($2)}
|     ORDER_CONVEX       aux      {Clx.Cubical_order_convex($2)}
|     BOUNDARY           aux      {Clx.Boundary($2)}
|     NORMALIZE          aux      {Clx.Normalize($2)}
|     COMPRESS           aux      {Clx.Compress($2)}
|     DEADLOCKS          aux      {Clx.Deadlocks($2)}
|     REACHABLE          aux      {Clx.Reachable($2)}
| aux PAST_CONE          aux      {Clx.Past_cone($3,$1)}
| aux FUTURE_CONE        aux      {Clx.Future_cone($1,$3)}
|     MCTD               aux      {Clx.Might_go_deadlock($2)}
|     MCTI               aux      {Clx.Might_go_infinity($2)}
|     GINZU              aux      {Clx.Ginzu($2)}
|     DEADLOCK_ATTRACTOR aux      {Clx.Deadlock_attractor($2)}
|     INFINITY_ATTRACTOR aux      {Clx.Infinity_attractor($2)}
| aux PRODUCT            aux      {Clx.Product($1,$3)}
| aux EXPONENT           INT      {Clx.Exponent($1,$3)}
  ;


  loader:
| IDNAME EQUAL aux loader {( $1::(fst $4) , Mos.add $1 $3 (snd $4   ) )}
| IDNAME EQUAL aux        {(         [$1] , Mos.add $1 $3 (Mos.empty) )}
  ;

  result:
| loader END {$1}
  ;
%%
