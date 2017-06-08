open Type
open AbstractSyntax
open Color

let name_of_file_to_analyze = ref ""

(* Extract the syntactical semantics form a program given as a string. *)

let semantics_from_c_file = ref ((fun _ -> failwith "Not compiled with C files support.") : string -> AbstractSyntax.t)

let semantics_from_string ?(fmt=`PV) p =
  match fmt with
  | `PV ->
    let semantics = Parser_pv.program Lexer_pv.entry_point (Lexing.from_string p) in
    Interpreter.init_runpro semantics;
    semantics
  | `C ->
    let fname, oc = Filename.open_temp_file "alcool" ".c" in
    output_string oc p;
    close_out oc;
    let ans = !semantics_from_c_file fname in
    Sys.remove fname;
    ans

let semantics_from_pv_file f =
  let c =
    try
      open_in_bin f
    with
    | Sys_error _ ->
      raise (Failure (Printf.sprintf "%s unable to open file %s.\n" (red ~bold:true "Error:") (blue ~bold:true f)))
  in
  let ans =
    let lexbuf = Lexing.from_channel c in
    try
      let semantics = Parser_pv.program Lexer_pv.entry_point lexbuf in
      Interpreter.init_runpro semantics;
      semantics
    with
    | Failure "lexing: empty token" ->
      let pos = (Lexing.lexeme_end_p lexbuf) in
      raise
	(Failure
	   (Printf.sprintf "%s lexing failure in file %s at line %d charater %d\n"
	      (red ~bold:true "Error:")
	      f
	      pos.Lexing.pos_lnum
	      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol - 1)
	   ))
    | Parsing.Parse_error ->
      let pos = (Lexing.lexeme_end_p lexbuf) in
      raise
	(Failure
	   (Printf.sprintf "%s unable to parse the content of the file %s. Invalid word %s at line %d character %d\n"
	      (red ~bold:true "Error:")
	      (blue ~bold:true f)
	      (Lexing.lexeme lexbuf)
	      pos.Lexing.pos_lnum
	      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol - 1)
	   ))
  in
  close_in c;
  ans

let semantics_from_file fname =
  if Filename.check_suffix fname ".c" then
    !semantics_from_c_file fname
  else
    semantics_from_pv_file fname

let run_down_resources semantics =
  print_endline "mutex switch";
  let mtx = AbstractSyntax.mutex `Default semantics in
  if mtx = [] then print_endline "No switch mutex";
  List.iter print_endline mtx;
  print_endline "semaphore switch";
  let sems = AbstractSyntax.semaphore `Default semantics in
  List.iter (fun (s,_) -> print_endline s) sems;
  print_endline "semaphore quantitative";
  let semq = AbstractSyntax.semaphore `Quantitative semantics in
  List.iter (fun (s,_) -> print_endline s) semq

let trace = Trace_space.analyze
