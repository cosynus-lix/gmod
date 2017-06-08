%{
  open Type
%} /* clôture de l'en-tête */

%token OPENPAR CLOSEPAR DEFINE OCRB CCRB END
%token UNION INTERSECTION PRODUCT DIFFERENCE MINUS
%token VOID COMPLEMENT FULL DOWNWARD UPWARD INTERIOR CLOSURE BOUNDARY 
%token COMPRESS OPTIMIZE UPPER_CORNERS DEADLOCKS REACHABLE 
%token MCTD MCTI GINZU PAST_CONE FUTURE_CONE DEADLOCK_ATTRACTOR INFINITY_ATTRACTOR
%token ORDER_CONVEX CUBE NORMALIZE
%token EXPONENT
%token DEFINE EQUAL SC POINT COMMA
%token < string > IDNAME
%token < int > INT
%token < Type.T.t > BRICK
%left EQUAL
%left PAST_CONE FUTURE_CONE
%left DIFFERENCE
%left UNION
%left INTERSECTION
%left PRODUCT
%left EXPONENT
%left COMMA
%nonassoc POINT COMPLEMENT DOWNWARD UPWARD CUBE INTERIOR CLOSURE ORDER_CONVEX BOUNDARY NORMALIZE COMPRESS UPPER_CORNERS DEADLOCKS REACHABLE MCTD MCTI GINZU DEADLOCK_ATTRACTOR INFINITY_ATTRACTOR 
/*point d'entrée de l'automate de reconnaissance du langage*/
%start result
%type < ( (string list) * (Type.Ctx.t Type.Mos.t) ) > result
%%

  aux:
|     IDNAME                          {if $1="S" then Ctx.Brick(T.full 1) else Ctx.Variable($1)}
|     VOID               INT CLOSEPAR {Ctx.Brick(T.empty $2)} 
|     FULL               INT CLOSEPAR {Ctx.Brick(T.full  $2)}
| OPENPAR  INT COMMA INT CLOSEPAR     {Ctx.Brick(T.of_list [Type.A.bounded true  true  $2 $4])}
| OPENPAR  INT COMMA INT OPENPAR      {Ctx.Brick(T.of_list [Type.A.bounded true  false $2 $4])}
| CLOSEPAR INT COMMA INT OPENPAR      {Ctx.Brick(T.of_list [Type.A.bounded false false $2 $4])}
| CLOSEPAR INT COMMA INT CLOSEPAR     {Ctx.Brick(T.of_list [Type.A.bounded false true  $2 $4])}
| OPENPAR  INT COMMA MINUS OPENPAR    {Ctx.Brick(T.of_list [Type.A.terminal true  $2])}
| CLOSEPAR INT COMMA MINUS OPENPAR    {Ctx.Brick(T.of_list [Type.A.terminal false $2])}
| OPENPAR  INT CLOSEPAR               {Ctx.Brick(T.of_list [Type.A.atom $2  ])}
| CLOSEPAR INT OPENPAR                {Ctx.Brick(T.of_list [Type.A.coatom $2])}
| aux UNION              aux          {Ctx.Union($1,$3)}
| aux INTERSECTION       aux          {Ctx.Intersection($1,$3)}
| aux DIFFERENCE         aux          {Ctx.Difference($1,$3)}
|     COMPLEMENT         aux          {Ctx.Complement($2)} 
|     OCRB               aux CCRB     {$2}
|     DOWNWARD           aux          {Ctx.Downward($2)}
|     UPWARD             aux          {Ctx.Upward($2)}
|     UPPER_CORNERS      aux          {Ctx.Upper_corners($2)}
|     INTERIOR           aux          {Ctx.Interior($2)}
|     CLOSURE            aux          {Ctx.Closure($2)}
|     CUBE               aux          {Ctx.Cube($2)}
|     ORDER_CONVEX       aux          {Ctx.Cubical_order_convex($2)}
|     BOUNDARY           aux          {Ctx.Boundary($2)}
|     NORMALIZE          aux          {Ctx.Normalize($2)}
|     COMPRESS           aux          {Ctx.Compress($2)}
|     DEADLOCKS          aux          {Ctx.Deadlocks($2)}
|     REACHABLE          aux          {Ctx.Reachable($2)}
| aux PAST_CONE          aux          {Ctx.Past_cone($3,$1)}
| aux FUTURE_CONE        aux          {Ctx.Future_cone($1,$3)}
|     MCTD               aux          {Ctx.Might_go_deadlock($2)}
|     MCTI               aux          {Ctx.Might_go_infinity($2)}
|     GINZU              aux          {Ctx.Ginzu($2)}
|     DEADLOCK_ATTRACTOR aux          {Ctx.Deadlock_attractor($2)}
|     INFINITY_ATTRACTOR aux          {Ctx.Infinity_attractor($2)}
| aux PRODUCT            aux          {Ctx.Product($1,$3)}
| aux EXPONENT           INT          {Ctx.Exponent($1,$3)}
  ;


  loader:
| IDNAME EQUAL aux loader {( $1::(fst $4) , Mos.add $1 $3 (snd $4   ) )}
| IDNAME EQUAL aux        {(         [$1] , Mos.add $1 $3 (Mos.empty) )}
  ;

  result:
| loader END {$1}
  ;
%%
