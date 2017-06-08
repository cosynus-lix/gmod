{
  open BuiltInWithoutParsers
  open Parser_dgrog_sheet
}

let chiffre = ['0'-'9']
let alphabetique_minuscule = ['a'-'z']
let alphabetique_majuscule = ['A'-'Z']
let blank = ' '*
let alphabetique_special = ['_']
let alphabetique = alphabetique_minuscule | alphabetique_majuscule | alphabetique_special
let alphanumerique = alphabetique | chiffre
let ident_admis = alphabetique + (alphanumerique*)
let entier = (chiffre+)
let union = "|" | "∪" | "⋃" | "⋁"
let intersection = "&" | "∩" | "⋂" | "⋀"
let closure = "cl."
let interior = "in."
let future = ">>" | "»" | "—>"
let past = "<<" | "«" | "<—"
let future_closure = "fcl."
let past_closure = "pcl."
let lsleq = "⩽" | "≤"
let gsleq = "⩾" | "≥"
let stless = "<"
let stgreater = ">"
let complement = "¬" | "!"
let difference = "\\"
let times = "*"
let quoted = "\""+([^ '\"']*)+"\""

  rule entry_point = parse
    | " "          {entry_point lexbuf}
    | "\t"         {entry_point lexbuf}
    | "\n"         {entry_point lexbuf}
    | "\r"         {entry_point lexbuf}
    | ident_admis  {IDENT(Lexing.lexeme lexbuf)}
    | quoted       {QUOTED(let aux = Lexing.lexeme lexbuf in (if false then print_endline (Message.red aux));String.sub aux 1 ((String.length aux)-2))}
    | "="          {EQUAL}
    | "("          {LPAR}
    | ")"          {RPAR}
    | intersection {INTERSECTION}
    | union        {UNION}
    | complement   {COMPLEMENT}
    | difference   {DIFFERENCE}
    | lsleq        {LESS_OR_EQUAL}
    | gsleq        {GREATER_OR_EQUAL}
    | stless       {STRICTLY_LESS}
    | stgreater    {STRICTLY_GREATER}
    | closure      {CLOSURE}
    | interior     {INTERIOR}
    | future       {FUTURE}
    | past         {PAST}
    | future_closure {FUTURE_CLOSURE}
    | past_closure   {PAST_CLOSURE}
    | ";"          {SEMICOLON}
    | eof          {END}
    | _            {ERROR(let x = Lexing.lexeme lexbuf in Printf.printf "Illegal character '%s'\n" (Message.red x);x)}
