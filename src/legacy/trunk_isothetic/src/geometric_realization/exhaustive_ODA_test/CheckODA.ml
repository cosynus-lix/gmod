
let hl s = Common.Terminal.rgb ~bold:true 2 1 3 s

let usage_msg = "Exhaustive testing of all functions provided by the library ODA by comparing its results with the ones provided by the library Area"

let all () =
  TestODA.Circle.full_testing_future_extension ();
  print_endline "";
  TestODA.Circle.full_testing_past_extension ();
  print_endline "";
  TestODA.Circle.full_testing_interior ();
  print_endline "";
  TestODA.Circle.full_testing_closure ();
  print_endline "";
  TestODA.HalfLine.full_testing_future_extension ();
  print_endline "";
  TestODA.HalfLine.full_testing_past_extension ();
  print_endline "";
  TestODA.HalfLine.full_testing_interior ();
  print_endline "";
  TestODA.HalfLine.full_testing_closure ();
  print_endline "";
  TestODA.HalfLine.full_testing_difference ();
  print_endline "";
  TestODA.HalfLine.full_testing_union ();
  print_endline "";
  TestODA.HalfLine.full_testing_intersection ();
  print_endline "";
  TestODA.HalfLine.full_testing_is_included ();
  print_endline "";
  TestODA.HalfLine.full_testing_future_extension_unbounded_flag ()

let rec list_of_options = 
  [
    "-all",Arg.Unit all," Testing everything";
    "-circle-future_extension",Arg.Unit (fun () -> TestODA.Circle.full_testing_future_extension ())," Testing the function future_extension from the module Oda.Circle";
    "-circle-past_extension",Arg.Unit (fun () -> TestODA.Circle.full_testing_past_extension ())," Testing the function past_extension from the module Oda.Circle";
    "-circle-interior",Arg.Unit (fun () -> TestODA.Circle.full_testing_interior ())," Testing the function interior from the module Oda.Circle";
    "-circle-closure",Arg.Unit (fun () -> TestODA.Circle.full_testing_closure ())," Testing the function closure from the module Oda.Circle";
    "-circle-string_of",Arg.Unit (fun () -> TestODA.Circle.full_testing_string_of ())," Testing the function string_of from the module Oda.Circle";
    "-halfline-future_extension",Arg.Unit (fun () -> TestODA.HalfLine.full_testing_future_extension ())," Testing the function future_extension from the module Oda.HalfLine";
    "-halfline-future_extension_unbounded_flag",Arg.Unit (fun () -> TestODA.HalfLine.full_testing_future_extension_unbounded_flag ())," Testing the unbounded flag option from the function future_extension from the module Oda.HalfLine";
    "-halfline-past_extension",Arg.Unit (fun () -> TestODA.HalfLine.full_testing_past_extension ())," Testing the function past_extension from the module Oda.HalfLine";
    "-halfline-interior",Arg.Unit (fun () -> TestODA.HalfLine.full_testing_interior ())," Testing the function interior from the module Oda.HalfLine";
    "-halfline-closure",Arg.Unit (fun () -> TestODA.HalfLine.full_testing_closure ())," Testing the function closure from the module Oda.HalfLine";
    "-difference",Arg.Unit (fun () -> TestODA.HalfLine.full_testing_difference ())," Testing the function difference from the module Oda.Shared";
    "-union",Arg.Unit (fun () -> TestODA.HalfLine.full_testing_union ())," Testing the function union from the module Oda.Shared";
    "-intersection",Arg.Unit (fun () -> TestODA.HalfLine.full_testing_intersection ())," Testing the function intersection from the module Oda.Shared";
    "-is_included",Arg.Unit (fun () -> TestODA.HalfLine.full_testing_is_included ())," Testing the function is_included from the module Oda.Shared";
    "-help",Arg.Unit help_message," Display this list of options";
    "--help",Arg.Unit help_message," Display this list of options"
  ]
and help_message () =
  print_endline usage_msg;
  List.iter 
    (fun (key,_,doc) -> 
      if doc <> "" 
      then Printf.printf "%s%s\n" (hl key) doc)
    list_of_options

let anon_fun filename = Printf.printf "%s Don't know what to do with file %S located in directory %S\n" 
	Message.warning (Filename.basename filename) (Filename.dirname filename) 

(*Entry point of the program. The failures are caught and their
  corresponding error messages are dislpayed.*)

let () =
  try Arg.parse list_of_options anon_fun usage_msg
  with Failure s -> print_endline s
