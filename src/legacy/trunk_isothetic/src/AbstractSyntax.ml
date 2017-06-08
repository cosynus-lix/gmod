include AbstractSyntax_without_parser

let from_channel c = Parser_pv.program Lexer_pv.entry_point (Lexing.from_channel c)

let from_string s = Parser_pv.program Lexer_pv.entry_point (Lexing.from_string s)

let from_filename f =
  let return = ref None in
  let c = open_in f in
  let () = return :=
    try Some (from_channel c)
    with ex -> close_in c ; raise ex in
  let () = close_in c in
  Common.get_some !return
