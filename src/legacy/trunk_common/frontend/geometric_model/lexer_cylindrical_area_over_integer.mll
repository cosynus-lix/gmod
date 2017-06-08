{
  open Parser_cylindrical_area_over_integer
  let extract_binary_word s = String.sub s 2 ((String.length s)-3)
}

let chiffre = ['0'-'9'] (* Un chiffre est l'un des symboles 0 1 2 3 4 5 6 7 8 9. *)
let alphabetique_minuscule = ['a'-'z']
let alphabetique_majuscule = ['A'-'Z']
let alphabetique_special = ['_']
let alphabetique = alphabetique_minuscule | alphabetique_majuscule | alphabetique_special
let alphanumerique = alphabetique | chiffre
let ident_admis = alphabetique + (alphanumerique)*
let chiffre = ['0'-'9'] (* Un chiffre est l'un des symboles 0 1 2 3 4 5 6 7 8 9. *)
let entier = chiffre+
let binary_word = ['0'-'1']*
let full = "C("+(['0'-'1']*)+')'
let empty = "@("+(['0'-'1']*)+')'

  rule entry_point = parse
    | " "                                            {entry_point lexbuf}
    | "\t"                                           {entry_point lexbuf}
    | "\n"                                           {entry_point lexbuf}
    | "\r"                                           {entry_point lexbuf}
    | ident_admis                                    {IDNAME (Lexing.lexeme lexbuf)}
    | entier                                         {INT(int_of_string(Lexing.lexeme lexbuf))}
    | full                                           {FULL (extract_binary_word(Lexing.lexeme lexbuf))}
    | "="                                            {EQUAL}
    | ","                                            {COMMA}
    | ";"                                            {SC}
    | "("                                            {OPENPAR}
    | ")"                                            {CLOSEPAR}
    | "["                                            {OSQB}
    | "]"                                            {CSQB}
    | "{"                                            {OCRB}
    | "}"                                            {CCRB}
    | "*"                                            {PRODUCT}
    | "&"                                            {INTERSECTION}
    | "|"                                            {UNION}
    | "-"                                            {MINUS}
    | "/"                                            {DIFFERENCE}
    | "!"                                            {COMPLEMENT}
    | "^"                                            {EXPONENT}
    | "."                                            {POINT}
    | empty                                          {VOID(extract_binary_word(Lexing.lexeme lexbuf))}
    | "up."                                          {UPWARD}
    | "do."                                          {DOWNWARD}
    | "in."                                          {INTERIOR}
    | "cl."                                          {CLOSURE}
    | "cu."                                          {CUBE}
    | "or."                                          {ORDER_CONVEX}
    | "bo."                                          {BOUNDARY}
    | "no."                                          {NORMALIZE}
    | "co."                                          {COMPRESS}
    | "uc."                                          {UPPER_CORNERS}
    | "dl."                                          {DEADLOCKS}
    | "da."                                          {DEADLOCK_ATTRACTOR}
    | "ia."                                          {INFINITY_ATTRACTOR}
    | "re."                                          {REACHABLE}
    | "mgd."                                         {MCTD}
    | "mgi."                                         {MCTI}
    | "gnz."                                         {GINZU}
    | "mbd."                                         {EMBED}
    | "<-"                                           {PAST_CONE}
    | "->"                                           {FUTURE_CONE}
    | eof                                            {END}
	  
