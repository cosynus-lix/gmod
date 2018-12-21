module HL = GraphOverInteger.HL

module G = GraphOverInteger.G

module R = GraphOverInteger.R

let of_string s =
  try Parser.output Lexer.output (Lexing.from_string s)
  with Parsing.Parse_error -> (
    print_endline "Unable to parse" ; failwith 
      "Error unable to parse the given string" )

let of_file f =
  let c = open_in f in
  let output = 
    try Parser.output Lexer.output (Lexing.from_channel c)
    with Parsing.Parse_error -> (
      let () = close_in c in
      print_endline "Unable to parse" ; failwith 
        "Error unable to parse the given string") in
  let () = close_in c in
  output

module M = Map.Make(String)

let show_endpoints = ref false

let print_arrow arrow src tgt dd =
  if !show_endpoints
  then Printf.printf "  %i: %i –> %i: %s\n" 
        arrow src tgt (HL.string_of dd)
  else Printf.printf "  %i: %s\n" arrow (HL.string_of dd)
  
let print g key r =
  let vertices = not (R.VSet.is_empty r.R.vertices) in
  let arrows = not (R.AMap.is_empty r.arrows) in
  Printf.printf "%s = %s" key (if R.is_empty r then "Ø\n\n" else "");
  if vertices then print_string "{ ";
  R.VSet.iter (fun v -> Printf.printf "%i " v) r.R.vertices;
  (if vertices then print_endline "}" else (if arrows then print_endline ""));
  R.AMap.iter 
    (fun a dd -> 
      if HL.contains_more_than_zero dd 
      then print_arrow a (G.src a g) (G.tgt a g) dd)
    r.arrows;
  if arrows then print_endline ""

let usage_msg =
"Usage: main <file name>

<file name> being the name of the text file containing the computations to 
perform.

The first part of the file is a list of entries of the following form:
  – <vertex>
  – <arrow> : <src> <tgt>
where <vertex>, <arrow>, <src> and <tgt> should be decimal representations of 
non-negative integers.
  
An entry of the first kind add a <vertex> to the underlying graph.
 
An entry of the second kind add an <arrow> from <src> to <tgt> to the 
underlying graph.

The second part of the file is a sequence of entries of the form
  <var> = <addenda> list
where <var> is any C-like identifier and <addenda> has the following form
  – <vertex>
  – <arrow> : <interval> list
  
An interval is either
  – [a b], [a b[, ]a b], ]a b[
  – [a oo[, ]a oo[ 
  – {a}
where a and b are decimal representations of non-negative integers. 
The intervals have to be disconnected and the list has to be sorted.

Options are:"

let anon_fun filename =
  let (g,r) = of_file filename in
  G.print_isolated_vertices g;
  G.print_arrows g;
  print_endline "";
  M.iter (print g) r

let opt_list = 
  [
    "--endpoints", Arg.Set show_endpoints, "show endpoints (hidden by default)";
  ]

let () = Arg.parse opt_list anon_fun usage_msg

(* Tests *)

(*
let x1 = 
"
0 1:;
1 2:;
1:*
"

let x1 = GR.of_string x1

let x1 = GR.zero_normalize x1

let () = print_endline "x1"; GR.print x1

let x2 = 
"
0 1:[0 oo[;
1 2:;
1:
"

let x2 = GR.of_string x2

let x2 = GR.zero_normalize x2

let () = print_endline "x2"; GR.print x2

let () = print_endline "calcul de x3"

let x3 = 
  let g = fst x1 in
  let r = R.past_extension g (snd x1) (snd x2) in 
  (g,r)

let () =
  print_endline "x3";
  GR.print x3

(*
let x = "
0 1:; 
0:*;1:*;2:*
"

let x = GR.of_string x

let (g,r) = zero_normalize x

let () = print_endline "x"; print (g,r)

let x =  g, GR.interior g r

let () = print_endline "interior x"; print x
*)

*)
