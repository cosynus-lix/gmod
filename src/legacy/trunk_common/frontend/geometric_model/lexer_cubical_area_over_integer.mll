{
  open Parser_cubical_area_over_integer
}

let chiffre = ['0'-'9'] (* Un chiffre est l'un des symboles 0 1 2 3 4 5 6 7 8 9. *)
let alphabetique_minuscule = ['a'-'z']
let alphabetique_majuscule = ['A'-'Z']
let alphabetique_special = ['_']
let alphabetique = alphabetique_minuscule | alphabetique_majuscule | alphabetique_special
let alphanumerique = alphabetique | chiffre
let ident_admis = (alphabetique + (alphanumerique)*)
let entier = (chiffre+)
let any = ([^ ' ']+)

  rule entry_point = parse
    | " "    {entry_point lexbuf}
    | "\t"   {entry_point lexbuf}
    | "\n"   {entry_point lexbuf}
    | "\r"   {entry_point lexbuf}
    | ident_admis  {IDNAME(Lexing.lexeme lexbuf)}
    | entier {INT(int_of_string(Lexing.lexeme lexbuf))}
    | "T("   {FULL}
    | "="    {EQUAL}
    | ","    {COMMA}
    | ";"    {SC}
    | "{"    {OCRB}
    | "}"    {CCRB}
    | "("    {OPENPAR}
    | ")"    {CLOSEPAR}
    | "["    {OSQB}
    | "]"    {CSQB}
    | "*"    {PRODUCT}
    | "&"    {INTERSECTION}
    | "|"    {UNION}
    | "-"    {MINUS}
    | "/"    {DIFFERENCE}
    | "!"    {COMPLEMENT}
    | "^"    {EXPONENT}
    | "."    {POINT}
    | "@("   {VOID}
    | "up."  {UPWARD}
    | "do."  {DOWNWARD}
    | "in."  {INTERIOR}
    | "cl."  {CLOSURE}
    | "cu."  {CUBE}
    | "or."  {ORDER_CONVEX}
    | "bo."  {BOUNDARY}
    | "no."  {NORMALIZE}
    | "co."  {COMPRESS}
    | "uc."  {UPPER_CORNERS}
    | "dl."  {DEADLOCKS}
    | "da."  {DEADLOCK_ATTRACTOR}
    | "ia."  {INFINITY_ATTRACTOR}
    | "re."  {REACHABLE}
    | "com." {COMMON}
    | "mgd." {MCTD}
    | "mgi." {MCTI}
    | "gnz." {GINZU}
    | "<-"   {PAST_CONE}
    | "->"   {FUTURE_CONE}
    | "ba."  {BASE}
    | "cs0." {CUBSET(0)}
    | "cs1." {CUBSET(1)}
    | "cs2." {CUBSET(2)}
    | eof    {END}
    | any    {ERROR(Lexing.lexeme lexbuf)}	  
