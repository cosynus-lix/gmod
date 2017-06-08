{
  open BuiltInWithoutParsers
  open Parser_cpodgrog_sheet

  let show aux = if false then print_endline (Message.red aux)

  let error c = 
    let s = Char.escaped c in
    Printf.printf "Illegal character '%s'\n" (Message.red s);s

  let identifier id = IDENT id
    (*match id with
      | "cmp" -> COMPARE 
      | "fac" -> FACTORIZE
      | _     -> IDENT id*)

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
let complement = "¬"
let difference = "\\"
let closure = "c."
let interior = "i."
let compare = "cmp."
let factorize = "fac."
let future = ">>" | "»" | "—>"
let past = "<<" | "«" | "<—"
let quoted = "\""+([^ '\"']*)+"\""
let times = "*" | "×" | "⋅" | "⨉"

  rule entry_point = parse
    | " "              {entry_point lexbuf}
    | "\t"             {entry_point lexbuf}
    | "\n"             {entry_point lexbuf}
    | "\r"             {entry_point lexbuf}
    | ident_admis as i {IDENT i}
    | entier as e      {INT (int_of_string e)}
    | quoted as q      {QUOTED(show q;String.sub q 1 ((String.length q)-2))}
    | "="              {EQUAL}
    | ";"              {SEMICOLON}
    | "("              {LPAR}
    | ")"              {RPAR}
    | intersection     {INTERSECTION}
    | union            {UNION}
    | difference       {DIFFERENCE}
    | complement       {COMPLEMENT}
    | closure          {CLOSURE}
    | interior         {INTERIOR}
    | future           {FUTURE}
    | past             {PAST}
    | times            {TIMES}
    | compare          {COMPARE}
    | factorize        {FACTORIZE}
    | eof              {END}
    | _ as any         {ERROR(error any)}	  
