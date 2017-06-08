{
  open BuiltInWithoutParsers
  open Parser_cpodgrog

  let area = ()

}

let chiffre = ['0'-'9'] (* Un chiffre est l'un des symboles 0 1 2 3 4 5 6 7 8 9. *)
let alphabetique_minuscule = ['a'-'z']
let alphabetique_majuscule = ['A'-'Z']
let blank = ' '*
let alphabetique_special = ['_']
let alphabetique = alphabetique_minuscule | alphabetique_majuscule | alphabetique_special
let alphanumerique = alphabetique | chiffre
let ident_admis = alphabetique + (alphanumerique*)
let entier = (chiffre+)
let square_brackets = '['|']'

let operators = ['|' '&' '\\']
let parenthesis = ['(' ')']
let constant = "S'" | "R+" | "KR+" | "S¹" | "βR+" | "β" (*if utf-8 is supported*)
let interval = square_brackets+blank+entier+blank+','+blank+(entier|"+oo")+blank+square_brackets
let singleton = '{'+blank+(entier|"+oo")+blank+'}'  
let brick = (interval|singleton|operators|constant|parenthesis)
let area = ((blank+brick+blank)+)

  rule entry_point = parse
    | " "         {entry_point lexbuf}
    | "\t"        {entry_point lexbuf}
    | "\n"        {entry_point lexbuf}
    | "\r"        {entry_point lexbuf}
    | area as a
	{AREA (let _,ex = Parser_oda.output Lexer_oda.entry_point (Lexing.from_string a) in ex)}
    | ident_admis {IDENT(Lexing.lexeme lexbuf)}
    | "-"         {MINUS}
    | ";"         {SEMICOLON}
    | "⨉" | "*"   {TIMES}
    | eof         {END}
    | _           {ERROR(let x = Lexing.lexeme lexbuf in Printf.printf"Illegal character '%s'\n" x;x)}
