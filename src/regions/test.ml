module HL_legacy = ODA.RawHalfLine(Integer)

module Ci_legacy = ODA.RawCircle(Integer)

module DD = DashDot.Raw(Integer)

module HL = DD.HalfLine

module Ci = DD.Circle

let hl_to_legacy b = match b with 
  | DD.Iso x -> HL_legacy.Iso x
  | DD.Pun x -> HL_legacy.Pun x
  | DD.Opn x -> HL_legacy.Opn x
  | DD.Cls x -> HL_legacy.Cls x

let hl_to_legacy dd = List.map hl_to_legacy dd

let hl_of_legacy b = match b with 
  | HL_legacy.Iso x -> DD.Iso x
  | HL_legacy.Pun x -> DD.Pun x
  | HL_legacy.Opn x -> DD.Opn x
  | HL_legacy.Cls x -> DD.Cls x

let hl_of_legacy oda = List.map hl_of_legacy oda

(* of_string does not check the format of the argument *)

let rec of_string tl = Str.(
  match tl with 
    | [] -> (DD.empty)
    | [Delim l ; Text a] -> (
        let a = int_of_string a in
        let l = (l = "[") in
        DD.final l a  )
    | Delim "[" :: Text a :: Delim "]" :: tl -> (
        DD.(join (atom (int_of_string a))  (of_string tl)) )
    | Delim l :: Text a :: Text b :: Delim r :: tl -> (
        let a = int_of_string a in
        let b = int_of_string b in
        let () = assert (a < b) in
        let l = (l = "[") in
        let r = (r = "]") in
        DD.(join (interval l r a b ) (of_string tl)))
    | _ -> assert false)

let of_string s = 
  let tokens = Str.(full_split (regexp "[][{} \t]") s) in
  let tokens = 
    List.filter 
      (fun x -> match x with 
        | Str.Text _ -> true 
        | Str.Delim s -> s = "[" || s = "]")
      tokens in
  of_string tokens

type operator = 
  | Unary of (DD.t -> DD.t)
  | Binary of (DD.t -> DD.t -> DD.t)
  
let operator_of_string s = match s with
  | "meet" -> Binary DD.meet
  | "join" -> Binary DD.join
  | "hl_future" -> Binary DD.HalfLine.future_extension
  | "hl_past" -> Binary DD.HalfLine.past_extension
  | "ci_future" -> Binary DD.Circle.future_extension
  | "ci_past" -> Binary DD.Circle.past_extension
  | "complement" -> Unary DD.complement
  | "hl_interior" -> Unary DD.HalfLine.interior
  | "ci_interior" -> Unary DD.Circle.interior
  | "hl_closure" -> Unary DD.HalfLine.closure
  | "ci_closure" -> Unary DD.Circle.closure
  | _ -> failwith ("Unknown operator " ^ s)

let test_unary op_name operator operand expected_result =
  let result = operator operand in
  let expected_result = expected_result in
  if result <> expected_result
  then Printf.printf "%s\n %s\n= %s\nbut\n %s\nwas expected.\n%!"
    op_name
    (HL.string_of operand)
    (HL.string_of result)
    (HL.string_of expected_result)

let test_binary op_name operator operand1 operand2 expected_result =
  let result = operator operand1 operand2 in
  let expected_result = expected_result in
  if result <> expected_result
  then Printf.printf "%s\n %s\n %s\n= %s\nbut\n %s\nwas expected.\n%!"
    op_name
    (HL.string_of operand1)
    (HL.string_of operand2)
    (HL.string_of result)
    (HL.string_of expected_result)

let operator_name = ref ""

let operator = ref (Unary (fun x -> x)) (*dummy default value*)

let anon_fun s =
  let chan = open_in s in
  let iterator = 
    fun () -> 
      try 
        let s = ref (input_line chan) in
        while !s = "" || String.get (!s) 0 = '%' do
          s := input_line chan
        done;
        !s
      with End_of_file -> (
        close_in chan;
        raise Exit) in
  let operator = operator_of_string !operator_name in
  match operator with 
    | Unary operator -> (
        try
          while true do
            let operand = of_string (iterator ()) in
            let expected = of_string (iterator ()) in
            test_unary !operator_name operator 
              operand
              expected
          done
        with Exit -> print_endline "End of test")
    | Binary operator -> (
        try
          while true do
            let operand1 = of_string (iterator ()) in
            let operand2 = of_string (iterator ()) in
            let expected = of_string (iterator ()) in
            test_binary !operator_name  operator
              operand1 operand2 expected
          done
        with Exit -> print_endline "End of test")

let preparing op_name () = 
  operator_name := op_name ;
  operator := operator_of_string op_name


let perform_all_tests dir =
  preparing "hl_future" (); anon_fun (dir ^ "hl_future.test");
  preparing "ci_future" (); anon_fun (dir ^ "ci_future.test");
  preparing "hl_past" (); anon_fun (dir ^ "hl_past.test");
  preparing "ci_past" (); anon_fun (dir ^ "ci_past.test");
  preparing "hl_closure" (); anon_fun (dir ^ "hl_closure.test");
  preparing "ci_closure" (); anon_fun (dir ^ "ci_closure.test");
  preparing "hl_interior" (); anon_fun (dir ^ "hl_interior.test");
  preparing "ci_interior" (); anon_fun (dir ^ "ci_interior.test");
  preparing "meet" (); anon_fun (dir ^ "meet.test");
  preparing "join" (); anon_fun (dir ^ "join.test");
  preparing "complement" (); anon_fun (dir ^ "complement.test")

let exhaustive_intervals max = 
  let next n = if n < max then n + 1 else raise Exit in
  let next = DD.next_interval next in
  let x = ref (DD.atom 0) in
  try
    while true do
      print_string (HL.string_of !x);
      x := next !x;
      print_endline ""
    done
  with Exit -> print_endline ""

(* based on dashdot *)
let exhaustive_regions max = 
  let next n = if n < max then n + 1 else raise Exit in
  let next = DD.next next in
  let x = ref DD.empty in
  try
    while true do 
      print_endline (HL.string_of !x);
      x := next !x
    done
  with Exit -> ()

let exhaustive_future_extension_on_half_line max =
  let next n = if n < max then n + 1 else raise Exit in
  let next = DD.next next in
  let at1 = ref DD.empty in
  let at2 = ref DD.empty in
  let fe1 = ref DD.empty in
  let fe2 = ref DD.empty in
  let ok = ref true in
  let nb_of_tests = Int64.(shift_left 2L (2*max+1)) in
  let nb_of_tests = Int64.mul nb_of_tests nb_of_tests in
  let one_percent = Int64.(to_int (div nb_of_tests 100L)) in
  let () = Printf.printf "There are %s tests to perform\n" 
    (Int64.to_string nb_of_tests) in 
  let percent = ref 0 in
  let counter = ref 0 in
  while !ok do
    let at1' = hl_to_legacy !at1 in
    let at2' = hl_to_legacy !at2 in
    let at3  = hl_of_legacy (HL_legacy.future_extension at1' at2') in
    fe1 := DD.join !at1 at3;
    fe2 := HL.future_extension !at1 !at2;
    ok  := !fe1 = !fe2;
    incr counter; 
    if not !ok then (
      print_endline "Mismatch:";
      Printf.printf "at1 = %s\n" (HL.string_of !at1);
      Printf.printf "at2 = %s\n" (HL.string_of !at2);
      Printf.printf "fe1 = %s\n" (HL.string_of !fe1);
      Printf.printf "fe2 = %s\n" (HL.string_of !fe2));
    begin
      try at2 := next !at2
      with Exit -> (
        try at1 := next !at1; at2 := DD.empty 
        with Exit -> ok := false);
    end;
    if !counter = one_percent 
    then (counter := 0; incr percent; if !percent < 100 then Printf.printf "%i%%\r%!" !percent else print_endline "100%")
  done
  
let exhaustive_past_extension_on_half_line max =
  let next n = if n < max then n + 1 else raise Exit in
  let next = DD.next next in
  let at1 = ref DD.empty in
  let at2 = ref DD.empty in
  let fe1 = ref DD.empty in
  let fe2 = ref DD.empty in
  let ok = ref true in
  let nb_of_tests = Int64.(shift_left 2L (2*max+1)) in
  let nb_of_tests = Int64.mul nb_of_tests nb_of_tests in
  let one_percent = Int64.(to_int (div nb_of_tests 100L)) in
  let () = Printf.printf "There are %s tests to perform\n" 
    (Int64.to_string nb_of_tests) in 
  let percent = ref 0 in
  let counter = ref 0 in
  while !ok do
    let at1' = hl_to_legacy !at1 in
    let at2' = hl_to_legacy !at2 in
    let at3  = hl_of_legacy (HL_legacy.past_extension at1' at2') in
    fe1 := DD.join !at1 at3;
    fe2 := HL.past_extension !at1 !at2;
    ok  := !fe1 = !fe2;
    incr counter; 
    if not !ok then (
      print_endline "Mismatch:";
      Printf.printf "at1 = %s\n" (HL.string_of !at1);
      Printf.printf "at2 = %s\n" (HL.string_of !at2);
      Printf.printf "fe1 = %s\n" (HL.string_of !fe1);
      Printf.printf "fe2 = %s\n" (HL.string_of !fe2));
    begin
      try at2 := next !at2
      with Exit -> (
        try at1 := next !at1; at2 := DD.empty 
        with Exit -> ok := false);
    end;
    if !counter = one_percent 
    then (counter := 0; incr percent; if !percent < 100 then Printf.printf "%i%%\r%!" !percent else print_endline "100%")
  done

let exhaustive_future_extension_on_circle max =
  let next n = if n < max then n + 1 else raise Exit in
  let next = DD.next next in
  let at1 = ref DD.empty in
  let at2 = ref DD.empty in
  let fe1 = ref DD.empty in
  let fe2 = ref DD.empty in
  let ok = ref true in
  let nb_of_tests = Int64.(shift_left 2L (2*max+1)) in
  let nb_of_tests = Int64.mul nb_of_tests nb_of_tests in
  let one_percent = Int64.(to_int (div nb_of_tests 100L)) in
  let () = Printf.printf "There are %s tests to perform\n" 
    (Int64.to_string nb_of_tests) in 
  let percent = ref 0 in
  let counter = ref 0 in 
  while !ok do  
    let at1' = hl_to_legacy !at1 in
    let at2' = hl_to_legacy !at2 in
    let at3  = hl_of_legacy (Ci_legacy.future_extension at1' at2')  in
    fe1 := DD.join !at1 at3;
    fe2 := Ci.future_extension !at1 !at2;
    ok  := !fe1 = !fe2;
    incr counter; 
    if not !ok then (
      print_endline "Mismatch:";
      Printf.printf "at1 = %s\n" (HL.string_of !at1);
      Printf.printf "at2 = %s\n" (HL.string_of !at2);
      Printf.printf "fe1 = %s\n" (HL.string_of !fe1);
      Printf.printf "fe2 = %s\n" (HL.string_of !fe2));
    begin
      try at2 := next !at2
      with Exit -> (
        try at1 := next !at1; at2 := DD.empty 
        with Exit -> ok := false);
    end;
    if !counter = one_percent 
    then (counter := 0; incr percent; if !percent < 100 then Printf.printf "%i%%\r%!" !percent else print_endline "100%")
  done
  
let exhaustive_past_extension_on_circle max =
  let next n = if n < max then n + 1 else raise Exit in
  let next = DD.next next in
  let at1 = ref DD.empty in
  let at2 = ref DD.empty in
  let fe1 = ref DD.empty in
  let fe2 = ref DD.empty in
  let ok = ref true in
  let nb_of_tests = Int64.(shift_left 2L (2*max+1)) in
  let nb_of_tests = Int64.mul nb_of_tests nb_of_tests in
  let one_percent = Int64.(to_int (div nb_of_tests 100L)) in
  let () = Printf.printf "There are %s tests to perform\n" 
    (Int64.to_string nb_of_tests) in 
  let percent = ref 0 in
  let counter = ref 0 in 
  while !ok do  
    let at1' = hl_to_legacy !at1 in
    let at2' = hl_to_legacy !at2 in
    let at3  = hl_of_legacy (Ci_legacy.past_extension at1' at2')  in
    fe1 := DD.join !at1 at3;
    fe2 := Ci.past_extension !at1 !at2;
    ok  := !fe1 = !fe2;
    incr counter; 
    if not !ok then (
      print_endline "Mismatch:";
      Printf.printf "at1 = %s\n" (HL.string_of !at1);
      Printf.printf "at2 = %s\n" (HL.string_of !at2);
      Printf.printf "fe1 = %s\n" (HL.string_of !fe1);
      Printf.printf "fe2 = %s\n" (HL.string_of !fe2));
    begin
      try at2 := next !at2
      with Exit -> (
        try at1 := next !at1; at2 := DD.empty 
        with Exit -> ok := false);
    end;
    if !counter = one_percent 
    then (counter := 0; incr percent; if !percent < 100 then Printf.printf "%i%%\r%!" !percent else print_endline "100%")
  done
  

  
let command_line_options = [
  "-all", Arg.String perform_all_tests, "Perform all tests in the specified directory." ;
  "-future-extension-on-half-line", Arg.Unit (preparing "hl_future"), "Test future_extension on the half-line" ;
  "-past-extension-on-half-line", Arg.Unit (preparing "hl_past"), "Test past_extension on the half-line" ;
  "-future-extension-on-circle", Arg.Unit (preparing "ci_future"), "Test ci_future_extension" ;
  "-past-extension-on-circle", Arg.Unit (preparing "ci_past"), "Test ci_past_extension" ;
  "-meet", Arg.Unit (preparing "meet"), "Test meet" ;
  "-join", Arg.Unit (preparing "join"), "Test join" ;
  "-complement", Arg.Unit (preparing "complement"), "Test complement" ;
  "-interior-on-half-line", Arg.Unit (preparing "hl_interior"), "Test interior on half-line" ;
  "-closure-on-half-line", Arg.Unit (preparing "hl_closure"), "Test closure on half-line" ;
  "-interior-on-circle", Arg.Unit (preparing "hl_interior"), "Test interior on circle" ;
  "-closure-on-circle", Arg.Unit (preparing "hl_closure"), "Test closure on circle" ;
  "-exhaustive-intervals",Arg.Int (exhaustive_intervals), "Compare the results of the current implementation with a previous one, on all possible intervals up to some extent.";
  "-exhaustive-regions",Arg.Int (exhaustive_regions), "Compare the results of the current implementation with a previous one, on all possible regions up to some extent.";
  "-exhaustive-future-extension-on-half-line",Arg.Int (exhaustive_future_extension_on_half_line),"Compare the new implementation of future_extension on half-line (~ 60 LoC) with the current one (~ 500 LoC)";
  "-exhaustive-future-extension-on-circle",Arg.Int (exhaustive_future_extension_on_circle),"Compare the new implementation of future_extension on circle (~ 60 LoC) with the current one (~ 1000 LoC)";
  "-exhaustive-past-extension-on-half-line",Arg.Int (exhaustive_past_extension_on_half_line),"Compare the new implementation of past_extension on half-line (~ 60 LoC) with the current one (~ 500 LoC)";
  "-exhaustive-past-extension-on-circle",Arg.Int (exhaustive_past_extension_on_circle),"Compare the new implementation of past_extension on circle (~ 60 LoC) with the current one (~ 1000 LoC)";
(*
  "-exhaustive-meet",Arg.Int (exhaustive_meet), "Compare the results of the current implementation of meet with a previous one, on all possible regions up to some extent.";
*)
]

let msg = "This tool tests the DashDot library, which implements boolean, topological, 
and order theoretic operations on the finite union of intervals 
(respectively on finite unions of arcs). 

The tests to perform are stored in files.

In case of a unary operator, each test is given by two lines, the first 
one being the operand, the second one being the expected result.

In case of a binary operator, each test is given by three lines, the first 
two of them being the operands, the third one being the expected result.

A value is described by a sequence of disconnected intervals such that if I 
appears before J in the description, then I < J.

Intervals are 
  [x] : singlton
  [x y] : closed interval
  ]x y[ : open interval
  [x y[ : left-closed right-open interval
  ]x y] : left-open right-closed interval
  [x    : closed terminal segment
  ]x    : open terminal segment

E.g.: [0] ]1 3[ [3 represents {0} U ]1,3[ U [3,+oo[.
  
Choose an option, and a file or a directory.

Lines starting with % are comments. Empty lines are ignored.
Options are:"

let () = Arg.parse command_line_options anon_fun msg
