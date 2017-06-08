{
  open Parser_arc ;;
}

let chiffre = ['0'-'9'] (* Un chiffre est l'un des symboles 0 1 2 3 4 5 6 7 8 9. *)
let entier = chiffre+

  rule entry_point = parse
      " "                                            {entry_point lexbuf}  
    | "\t"                                           {entry_point lexbuf}  
    | "\n"                                           {entry_point lexbuf}  
    | "\r"                                           {entry_point lexbuf}  
    | entier                                         {INT (int_of_string (Lexing.lexeme lexbuf))}
    | "["                                            {OSQB}
    | "]"                                            {CSQB}
    | "{"                                            {OCRB}
    | "}"                                            {CCRB}
    | ","                                            {COMMA}
    | "-"                                            {UNRCHBL}
    | eof                                            {END}
