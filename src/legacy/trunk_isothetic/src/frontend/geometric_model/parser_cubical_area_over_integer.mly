%{

  open Type
  open Color

%} /* clôture de l'en-tête */

%token OPENPAR CLOSEPAR OSQB CSQB DEFINE OCRB CCRB END
%token UNION INTERSECTION PRODUCT DIFFERENCE MINUS
%token VOID COMPLEMENT FULL DOWNWARD UPWARD INTERIOR CLOSURE BOUNDARY 
%token COMPRESS OPTIMIZE UPPER_CORNERS DEADLOCKS REACHABLE 
%token MCTD MCTI GINZU PAST_CONE FUTURE_CONE DEADLOCK_ATTRACTOR INFINITY_ATTRACTOR BASE COMMON
%token ORDER_CONVEX CUBE NORMALIZE
%token EXPONENT
%token DEFINE EQUAL SC POINT COMMA
%token <int> CUBSET
%token <string> IDNAME ERROR
%token <int> INT
%token <Type.C.t> BRICK
%left EQUAL
%left PAST_CONE FUTURE_CONE
%left DIFFERENCE
%left UNION
%left INTERSECTION
%left PRODUCT
%left EXPONENT
%left COMMA
%nonassoc POINT COMPLEMENT DOWNWARD UPWARD CUBE INTERIOR CLOSURE ORDER_CONVEX BOUNDARY NORMALIZE COMPRESS UPPER_CORNERS DEADLOCKS REACHABLE MCTD MCTI GINZU DEADLOCK_ATTRACTOR INFINITY_ATTRACTOR BASE CUBSET COMMON
/*point d'entrée de l'automate de reconnaissance du langage*/
%start result
%type <(string list)*(Type.Cbx.t Type.Mos.t)> result
%%

  aux:
|     IDNAME                          {if $1="H" then Cbx.Brick(C.full 1) else Cbx.Variable($1)}
|     VOID               INT CLOSEPAR {Cbx.Brick(C.empty $2)} 
|     FULL               INT CSQB     {Cbx.Brick(C.full $2)}
| OSQB INT COMMA INT  CSQB            {Cbx.Brick(C.of_list [I.bounded true true   $2 $4])}
| OSQB INT COMMA INT  OSQB            {Cbx.Brick(C.of_list [I.bounded true false  $2 $4])}
| CSQB INT COMMA INT  OSQB            {Cbx.Brick(C.of_list [I.bounded false false $2 $4])}
| CSQB INT COMMA INT  CSQB            {Cbx.Brick(C.of_list [I.bounded false true  $2 $4])}
| OSQB INT COMMA MINUS OSQB           {Cbx.Brick(C.of_list [I.terminal true  $2])}
| CSQB INT COMMA MINUS OSQB           {Cbx.Brick(C.of_list [I.terminal false $2])}
| OSQB INT CSQB                       {Cbx.Brick(C.of_list [I.atom $2  ])}
| aux UNION              aux          {Cbx.Union($1,$3)}
| aux INTERSECTION       aux          {Cbx.Intersection($1,$3)}
| aux DIFFERENCE         aux          {Cbx.Difference($1,$3)}
|     COMPLEMENT         aux          {Cbx.Complement($2)} 
|     OCRB               aux CCRB     {$2}
|     DOWNWARD           aux          {Cbx.Downward($2)}
|     UPWARD             aux          {Cbx.Upward($2)}
|     UPPER_CORNERS      aux          {Cbx.Upper_corners($2)}
|     INTERIOR           aux          {Cbx.Interior($2)}
|     CLOSURE            aux          {Cbx.Closure($2)}
|     CUBE               aux          {Cbx.Cube($2)}
|     ORDER_CONVEX       aux          {Cbx.Cubical_order_convex($2)}
|     BOUNDARY           aux          {Cbx.Boundary($2)}
|     NORMALIZE          aux          {Cbx.Normalize($2)}
|     COMPRESS           aux          {Cbx.Compress($2)}
|     DEADLOCKS          aux          {Cbx.Deadlocks($2)}
|     REACHABLE          aux          {Cbx.Reachable($2)}
| aux PAST_CONE          aux          {Cbx.Past_cone($3,$1)}
| aux FUTURE_CONE        aux          {Cbx.Future_cone($1,$3)}
|     MCTD               aux          {Cbx.Might_go_deadlock($2)}
|     MCTI               aux          {Cbx.Might_go_infinity($2)}
|     COMMON             aux          {Cbx.Common($2)}
|     GINZU              aux          {Cbx.Ginzu($2)}
|     DEADLOCK_ATTRACTOR aux          {Cbx.Deadlock_attractor($2)}
|     INFINITY_ATTRACTOR aux          {Cbx.Infinity_attractor($2)}
|     BASE               aux          {Cbx.Base($2)}
| aux PRODUCT            aux          {Cbx.Product($1,$3)}
| aux EXPONENT           INT          {Cbx.Exponent($1,$3)}
|     CUBSET             aux          {Cbx.Cubical_set($1,$2)}
|     ERROR                           {failwith (Printf.sprintf "%s unknown token %s.\n" error (blue ~bold:true $1))}
  ;


  loader:
| IDNAME EQUAL aux loader {( $1::(fst $4) , Mos.add $1 $3 (snd $4   ) )}
| IDNAME EQUAL aux        {(         [$1] , Mos.add $1 $3 (Mos.empty) )}
  ;

  result:
| loader END {$1}
  ;
%%
