{

  open Parser

  let show aux = if false then print_endline aux

  let error c = 
    let s = Char.escaped c in
    Printf.printf "Illegal character '%s'\n" s;s

  let identifier id = 
    match id with
      | "oo" -> INFTY
      | "join" -> JOIN 
      | "meet" -> MEET
      | "diff" -> DIFFERENCE
      | "comp" -> COMPLEMENT
      | "full" -> FULL
      | "void" -> EMPTY
      | "interior" -> INTERIOR
      | "closure" -> CLOSURE
      | "future_ext" -> FUTURE_EXTENSION
      | "past_ext" -> PAST_EXTENSION
      | "future_clo" -> FUTURE_CLOSURE
      | "past_clo" -> PAST_CLOSURE
      | _     -> ID id

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

  rule output = parse
    | " "              {output lexbuf}
    | "\t"             {output lexbuf}
    | "\n"             {output lexbuf}
    | "\r"             {output lexbuf}
    | ident_admis as i {identifier i}
    | entier as e      {INT (int_of_string e)}
    | "="              {EQUAL}
    | ":"              {COLON}
    | ";"              {SEMICOLON}
    | "("              {OPAR}
    | ")"              {CPAR}
    | "["              {OBRA}
    | "]"              {CBRA}
    | "oo"             {INFTY}
    | eof              {END}
    | _ as any         {ERROR(error any)}	  
