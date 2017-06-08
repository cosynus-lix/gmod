{
  open BuiltInWithoutParsers
  open Parser_oda
}

let support_halfline = "#halfline" | "#R+" | "#ℝ+"
let support_circle = "#circle" | "#S'" | "#S¹"
let support_compacthalfline = "#compact_halfline" | "#KR+" | "#βR+" | "#Kℝ+" | "#βℝ+" | "#β"
let circle = "S'" | "S¹"
let halfline = "R+" | "ℝ+"
let compacthalfline = "KR+" | "βR+" | "Kℝ+" | "βℝ+" | "β"
let chiffre = ['0'-'9'] 
let alphabetique_minuscule = ['a'-'z']
let alphabetique_majuscule = ['A'-'Z']
let alphabetique_special = ['_']
let alphabetique = alphabetique_minuscule | alphabetique_majuscule | alphabetique_special
let alphanumerique = alphabetique | chiffre
let ident_admis = alphabetique + (alphanumerique*)
let entier = (chiffre+)
let infinity = "oo" | "+oo" | "∞" | "+∞" | "ꝏ" | "+ꝏ"
let union = "|" | "∪" | "⋃" | "⋁"
let intersection = "&" | "∩" | "⋂" | "⋀"
let complement = "¬"
let difference = "\\"
let closure = "c."
let interior = "i."
let future = ">>" | "»" | "—>"
let past = "<<" | "«" | "<—"

    rule entry_point = parse
      | " "  {entry_point lexbuf}
      | "\t" {entry_point lexbuf}
      | "\n" {entry_point lexbuf}
      | "\r" {entry_point lexbuf}
      | ","  {COMMA}
      | "["  {LSQBR}
      | "]"  {RSQBR}
      | "{"  {LCURL}
      | "}"  {RCURL}
      | "("  {LPAR}
      | ")"  {RPAR}
	  
      | support_halfline        {SUPPORT (Some HalfLine)}
      | support_circle          {SUPPORT (Some Circle)}
      | support_compacthalfline {SUPPORT (Some CompactHalfLine)}

      | ident_admis {IDENT (Lexing.lexeme lexbuf)}
	  
      | entier   {INTEGER(int_of_string (Lexing.lexeme lexbuf))}
      | infinity {INFINITY}

      | complement   {COMPLEMENT}
      | difference   {DIFFERENCE}
      | union        {UNION}
      | intersection {INTERSECTION}

      | circle          {CIRCLE}
      | halfline        {HALFLINE}
      | compacthalfline {COMPACTHALFLINE}

      | closure  {CLOSURE}
      | interior {INTERIOR}

      | future   {FUTURE}
      | past     {PAST}

      | eof {END}
      | _   {ERROR(let x = Lexing.lexeme lexbuf in Printf.printf"Illegal character '%s'\n" x;x)}
