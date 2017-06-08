(* Returns an execution trace of a PV program *)

let disable_color () = AbstractSyntax.Highlight.color_enabled := false

let speclist = [ "-disable-color" , Arg.Unit disable_color , "no color highlightning on output" ]

let scenario n = Random.int n

let anon_fun pv_source_file =
  let module Interpreter = Interpreter.Make (struct let program = AbstractSyntax.from_filename pv_source_file end) in
  Interpreter.play scenario

let usage_msg = "Interpret PV programs"

let () =
  Random.self_init () ;
  Arg.parse speclist anon_fun usage_msg
