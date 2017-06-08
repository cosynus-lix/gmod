  (* Intermediate files containing some of_string like functions. Mainly
     used in parsers and included in file builtIn.ml *)

module DGROG =
struct
  module OverInteger =
  struct
    let of_string s =
      try Parser_dgrog.output Lexer_dgrog.entry_point (Lexing.from_string s)
      with Parsing.Parse_error -> (
        print_endline "BuiltIn.Dgrog unable to parse" ; failwith (
          Printf.sprintf "%s unable to parse the given string [BuiltIn.Dgrog.of_string]"
          Message.error) )
  end(*OverInteger*)
end(*Dgrog*)

module CPODGROG =
struct
  module OverInteger =
  struct
    let of_string s =
      try Parser_cpodgrog.output Lexer_cpodgrog.entry_point (Lexing.from_string s)
      with Parsing.Parse_error ->
	(
	  print_endline "BuiltIn.Area unable to parse";
	  failwith
	    (
	      Printf.sprintf
		"%s unable to parse the given string [BuiltIn.Area.of_string]"
		Message.error
	    )
	)
  end(*OverInteger*)
end(*Cpodgrog*)
