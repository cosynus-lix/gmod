let parse lexbuf =
  try
    Parser.decls Lexer.token lexbuf
  with
  | Failure s when s = "lexing: empty token" ->
     failwith "Lexing error" 
  | Parsing.Parse_error ->
     failwith "Parsing error"

let () =
  let fname = ref "" in
  Arg.parse [] (fun s -> fname := s) "gmod [options] file";
  if !fname = "" then (Printf.printf "Please provide an input file.\n%!"; exit 1);
  let fin = open_in !fname in
  let lexbuf = Lexing.from_channel fin in
  let prog = parse lexbuf in
  ignore prog;
  close_in fin
