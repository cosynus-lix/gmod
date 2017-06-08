module V = BuiltInWithoutParsers.V
module E = BuiltInWithoutParsers.E
module B = BuiltInWithoutParsers.B

module DGROG =
struct

  module OverInteger =
  struct

  include BuiltInWithoutParsers.DGROG

  let of_file filename =
    let input_channel = open_in filename in
    let answer = Parser_dgrog.output Lexer_dgrog.entry_point (Lexing.from_channel input_channel) in
    close_in input_channel ; answer

  end(*OverInteger*)

end (*Dgrog*)

module Brick =
struct

  module OverInteger = BuiltInWithoutParsers.Brick

end(*Brick*)


module Area =
struct

  module OverInteger =
  struct

    include BuiltInWithoutParsers.Area

  (*

    let of_string s =
    try Parser_cpodgrog.output Lexer_cpodgrog.entry_point (Lexing.from_string s)
    with Parsing.Parse_error -> (print_endline "BuiltIn.Area unable to parse" ; failwith
    (Printf.sprintf "%s unable to parse the given string [BuiltIn.Area.of_string]" Message.error))

  *)

  (* TODO: find a way to exploit parser_cpodgrog_sheet in this file... *)

    let of_string s =
      let (_,toto) =
	(
	  Parser_cpodgrog_sheet.output Lexer_cpodgrog_sheet.entry_point
	    (Lexing.from_string ("x = "^s))
	) in
      let toto = Expression.find "x" toto in

      match Sheet_solver_cpodgrog.calculation toto Expression.empty with
	| Sheet_solver_cpodgrog.Cpodgrog ar -> ar
	| _ -> raise (Invalid_argument "halfline_subset_of_string")

    let of_file filename =
      let input_channel = open_in filename in
      let answer = Parser_cpodgrog.output Lexer_cpodgrog.entry_point (Lexing.from_channel input_channel) in
      close_in input_channel ; answer

  end(*OverInteger*)

end (*Area*)


module ODA =
struct

  module OverInteger =
  struct

    module Sh = BuiltInWithoutParsers.ODA.OverInteger.Sh

    let context_of_string s = Parser_oda_sheet.output Lexer_oda_sheet.entry_point (Lexing.from_string s)

    let of_string s = Parser_oda.output Lexer_oda.entry_point (Lexing.from_string s)

    let of_file filename =
      let input_channel = open_in filename in
      let answer = Parser_oda.output Lexer_oda.entry_point (Lexing.from_channel input_channel)
      in
      close_in input_channel ; answer


    module HL =
    struct

      include BuiltInWithoutParsers.ODA.OverInteger.HL

      let of_string s =
	match Sheet_solver_oda.calculation (of_string ("#halfline "^s)) Expression.empty with
	  | Sheet_solver_oda.Area (_,ar,_) -> ar
	  | _ -> raise (Invalid_argument "halfline_subset_of_string")

      let context_of_string s =
	Expression.map
	  (
	    fun b -> match b with
	      | (Sheet_solver_oda.Area (_,ar,_)) -> ar
	      | _ -> failwith "builtIn.solved_context_of_string"
	  )
	  (Sheet_solver_oda.solve (context_of_string ("#halfline"^s)))

    let concat s0 s1 = failwith "NIY [BuiltIn.ODA.HL.concat]" (*TODO*)

    end(*HL*)


    module Ci =
    struct

      include BuiltInWithoutParsers.ODA.OverInteger.Ci

      let of_string s =
	match Sheet_solver_oda.calculation (of_string ("#circle "^s)) Expression.empty with
	  | Sheet_solver_oda.Area (_,ar,_) -> ar
	  | _ -> raise (Invalid_argument "halfline_subset_of_string")
      let context_of_string s =
	Expression.map
	  (
	    fun b -> match b with
	      | (Sheet_solver_oda.Area (_,ar,_)) -> ar
	      | _ -> failwith "builtIn.solved_context_of_string"
	  )
	  (Sheet_solver_oda.solve (context_of_string ("#circle"^s)))

    end(*Ci*)

  end(*OverInteger*)

end (*Oda*)

(*
module D = DGROG.Make(V)(E)(B)
*)
