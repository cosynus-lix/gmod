{
  open Parser_oda_sheet

  let value v = 
    try
      let last = (String.length v)-1 in
      if v.[last]=';' 
      then v.[last] <- ' ' 
      ;
      Parser_oda.output Lexer_oda.entry_point 
	(Lexing.from_string v) (*remove semicolon*)
    with
      | Parsing.Parse_error -> (Printf.printf "%S\n%!" v;raise Parsing.Parse_error)

  let identifier id =
    match id with
      | "cmp" -> COMPARE 
      | _     -> IDENT id
}

let support_halfline = "#halfline" | "#R+" | "#ℝ+"
let support_circle = "#circle" | "#S'" | "#S¹"
let support_compacthalfline = "#compact_halfline" | "#KR+" | "#βR+" | "#Kℝ+" | "#βℝ+" | "#β"
let circle = "S'" | "S¹"
let halfline = "R+" | "ℝ+"
let compacthalfline = "KR+" | "βR+" | "Kℝ+" | "βℝ+" | "β"
let union = "|" | "∪" | "⋃" | "⋁"
let intersection = "&" | "∩" | "⋂" | "⋀"
let comma = ","
let semicolon = ";"
let lpar = "("
let rpar = ")"
let lcurl = "{"
let rcurl = "}"
let lsqbr = "["
let rsqbr = "]"
let infinity = "oo" | "+oo" | "∞" | "+∞"
let closure = "c."
let interior = "i."
let future = ">>" | "»" | "—>"
let past = "<<" | "«" | "<—"
let lsleq = "⩽" | "≤"
let gsleq = "⩾" | "≥"
let stless = "<"
let stgreater = ">"
let complement = "¬"
let difference = "\\"
let void = [' ' '\n' '\t' 'r']*
let chiffre = ['0'-'9'] (*digits 0 1 2 3 4 5 6 7 8 9*)
let alphabetique_minuscule = ['a'-'z']
let alphabetique_majuscule = ['A'-'Z']
let alphabetique_special = ['_']
let alphabetique = alphabetique_minuscule | alphabetique_majuscule | alphabetique_special
let alphanumerique = alphabetique | chiffre
let ident_admis = alphabetique + (alphanumerique*)
let support =  support_halfline|support_circle|support_compacthalfline
let value = support?void([^ '=' ';']+)(';'|eof) (*takes everything but the characters '=' and ';'*)
let entier = (chiffre+)

  rule entry_point = parse
    | " "  {entry_point lexbuf}
    | "\t" {entry_point lexbuf}
    | "\n" {entry_point lexbuf}
    | "\r" {entry_point lexbuf}
    | "="  {EQUAL}
    | ";"  {SEMICOLON}
    | ","  {COMMA}
    | "["  {LSQBR}
    | "]"  {RSQBR}
    | "{"  {LCURL}
    | "}"  {RCURL}
    | "("  {LPAR}
    | ")"  {RPAR}

    | support_halfline        {SUPPORT BuiltInWithoutParsers.HalfLine}
    | support_circle          {SUPPORT BuiltInWithoutParsers.Circle}
    | support_compacthalfline {SUPPORT BuiltInWithoutParsers.CompactHalfLine}

    | ident_admis {identifier (Lexing.lexeme lexbuf)}

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

    | lsleq     {LESS_OR_EQUAL}
    | gsleq     {GREATER_OR_EQUAL}
    | stless    {STRICTLY_LESS}
    | stgreater {STRICTLY_GREATER}

    | halfline        {HALFLINE}
    | circle          {CIRCLE}
    | compacthalfline {COMPACTHALFLINE}

    (*| value as v  {VALUE (value v)}  *)
    | eof         {END}
    | _           {ERROR(let x = Lexing.lexeme lexbuf in Printf.printf"[Lexer_oda_sheet] Illegal character '%s'\n" x;x)}

