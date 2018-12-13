module HL = GraphRegionOverInteger.HL

module G = GraphRegionOverInteger.G

module R = GraphRegionOverInteger.R

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

let (g,r) = of_file "data"

module M = Map.Make(String)

let print key r =
  Printf.printf "%s =%s\n" key (if R.is_empty r then " Ø\n" else "");
  if not (R.VSet.is_empty r.R.vertices) then print_string "  ";
  R.VSet.iter (fun v -> Printf.printf "%i " v) r.R.vertices;
  if not (R.VSet.is_empty r.R.vertices) then print_endline "";
  R.AMap.iter (fun a dd -> Printf.printf "  %i: %i –> %i: %s\n" 
    a (G.src a g) (G.tgt a g) 
    (HL.string_of dd)) 
    r.arrows;
  if not (R.AMap.is_empty r.arrows) then print_endline ""

let () =
  G.print_vertices g;
  G.print_arrows g;
  print_endline "";
  M.iter print r
  

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
