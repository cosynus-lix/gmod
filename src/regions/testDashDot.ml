module I = NonEmptyInterval.Raw(Integer)

module Legacy = ODA.BooleanAlgebra(Integer)

module HL_legacy = ODA.RawHalfLine(Integer)

module Ci_legacy = ODA.RawCircle(Integer)

module DD = DashDot.Raw(I)

let interval_to_legacy b = 
  match b with 
  | I.Si x -> Legacy.atom x
  | I.Bn (a,x,y,b) -> Legacy.interval a b x y
  | I.Te (a,x) -> HL_legacy.final a x

let rec normalize hl =
  match hl with
    | b1 :: ((b2 :: hl) as hl') -> (
        let v = Legacy.rvb b1 in
        if v = Legacy.rvb b2 then (Legacy.Pun v) :: normalize hl
        else b1 :: normalize hl') 
    | _ -> hl

let dd2_to_legacy dd2 = 
  let x = List.map interval_to_legacy dd2 in
  let x = List.concat x in
  normalize x 

let rec dd2_of_legacy hl =
  match hl with
  |  Legacy.Opn x :: Legacy.Opn y :: hl -> (I.bounded false x y false) :: dd2_of_legacy hl
  |  Legacy.Opn x :: Legacy.Cls y :: hl -> (I.bounded false x y true) :: dd2_of_legacy hl
  |  Legacy.Cls x :: Legacy.Opn y :: hl -> (I.bounded true x y false) :: dd2_of_legacy hl
  |  Legacy.Cls x :: Legacy.Cls y :: hl -> (I.bounded true x y true) :: dd2_of_legacy hl
  |  Legacy.Cls x :: (Legacy.Pun y :: hl) -> (I.bounded true x y false) :: dd2_of_legacy (Legacy.Opn y :: hl)
  |  Legacy.Opn x :: (Legacy.Pun y :: hl) -> (I.bounded false x y false) :: dd2_of_legacy (Legacy.Opn y :: hl)
  |  Legacy.Iso x :: hl -> (I.atom x) :: dd2_of_legacy hl
  | [Legacy.Opn x] -> [I.Te (false,x)]
  | [Legacy.Cls x] -> [I.Te (true,x)]
  | [] -> []
  | _ -> assert false

let test_unary op_name operator operand expected_result =
  let result = operator operand in
  let expected_result = expected_result in
  if result <> expected_result
  then Printf.printf "%s\n %s\n= %s\nbut\n %s\nwas expected.\n%!"
    op_name
    (DD.HalfLine.string_of operand)
    (DD.HalfLine.string_of result)
    (DD.HalfLine.string_of expected_result)

let test_binary op_name operator operand1 operand2 expected_result =
  let result = operator operand1 operand2 in
  let expected_result = expected_result in
  if result <> expected_result
  then Printf.printf "%s\n %s\n %s\n= %s\nbut\n %s\nwas expected.\n%!"
    op_name
    (DD.HalfLine.string_of operand1)
    (DD.HalfLine.string_of operand2)
    (DD.HalfLine.string_of result)
    (DD.HalfLine.string_of expected_result)

let exhaustive_test_binary oracle bin_op max dummy string_of_operand string_of_result =
  let next n = if n < max then n + 1 else raise Exit in
  let next = DD.next next in
  let at1 = ref DD.empty in
  let at2 = ref DD.empty in
  let fe1 = ref dummy in
  let fe2 = ref dummy in
  let ok = ref true in
  let nb_of_tests = Int64.(shift_left 2L (2*max+1)) in
  let nb_of_tests = Int64.mul nb_of_tests nb_of_tests in
  let one_percent = Int64.(to_int (div nb_of_tests 100L)) in
  let () = Printf.printf "There are %s tests to perform\n" 
    (Int64.to_string nb_of_tests) in 
  let percent = ref 0 in
  let counter = ref 0 in
  while !ok do
    fe1 := oracle !at1 !at2;
    fe2 := bin_op !at1 !at2;
    ok  := !fe1 = !fe2;
    incr counter;
    if not !ok then (
      print_endline "Mismatch:";
      Printf.printf "operand_1 = %s\n" (string_of_operand !at1);
      Printf.printf "operand_2 = %s\n" (string_of_operand !at2);
      Printf.printf "oracle    = %s\n" (string_of_result !fe1);
      Printf.printf "candidate = %s\n" (string_of_result !fe2));
    begin
      try at2 := next !at2
      with Exit -> (
        try at1 := next !at1; at2 := DD.empty 
        with Exit -> ok := false);
    end;
    if !counter = one_percent 
    then (counter := 0; incr percent; if !percent < 100 then Printf.printf "%i%%\r%!" !percent)
  done;
  print_endline "100%"

let wrapper legacy_bin_op = 
  fun at1 at2 ->
    let at1' = dd2_to_legacy at1 in
    let at2' = dd2_to_legacy at2 in
    let at3' = legacy_bin_op at1' at2' in
    dd2_of_legacy at3'

let exhaustive_join max = 
  let oracle = wrapper Legacy.union in
  let bin_op = DD.join in
  print_endline "Testing DashDot2.join";
  exhaustive_test_binary oracle bin_op max DD.empty DD.HalfLine.string_of DD.HalfLine.string_of

let exhaustive_meet max = 
  let oracle = wrapper Legacy.intersection in
  let bin_op = DD.meet in
  print_endline "Testing DashDot2.meet";
  exhaustive_test_binary oracle bin_op max DD.empty DD.HalfLine.string_of DD.HalfLine.string_of

let exhaustive_difference max = 
  let oracle = wrapper Legacy.difference in
  let bin_op = DD.difference in
  print_endline "Testing DashDot2.difference";
  exhaustive_test_binary oracle bin_op max DD.empty DD.HalfLine.string_of DD.HalfLine.string_of

let exhaustive_future_extension_on_half_line max = 
  let oracle = wrapper (fun x y -> Legacy.union x (HL_legacy.future_extension x y)) in
  let bin_op = DD.HalfLine.future_extension in
  print_endline "Testing DashDot2.HalfLine.future_extension";
  exhaustive_test_binary oracle bin_op max DD.empty DD.HalfLine.string_of DD.HalfLine.string_of

let exhaustive_past_extension_on_half_line max = 
  let oracle = wrapper (fun x y -> Legacy.union x (HL_legacy.past_extension x y)) in
  let bin_op = DD.HalfLine.past_extension in
  print_endline "Testing DashDot2.HalfLine.past_extension";
  exhaustive_test_binary oracle bin_op max DD.empty DD.HalfLine.string_of DD.HalfLine.string_of

let exhaustive_future_extension_on_circle max =   
  let oracle = wrapper (fun x y -> Legacy.union x (Ci_legacy.future_extension x y)) in
  let bin_op = DD.Circle.future_extension in
  print_endline "Testing DashDot2.Circle.future_extension";
  exhaustive_test_binary oracle bin_op max DD.empty DD.HalfLine.string_of DD.HalfLine.string_of

let exhaustive_past_extension_on_circle max = 
  let oracle = wrapper (fun x y -> Legacy.union x (Ci_legacy.past_extension x y)) in
  let bin_op = DD.Circle.past_extension in
  print_endline "Testing DashDot2.Circle.past_extension";
  exhaustive_test_binary oracle bin_op max DD.empty DD.HalfLine.string_of DD.HalfLine.string_of

let exhaustive_is_included max = 
  let oracle at1 at2 = Legacy.is_included (dd2_to_legacy at1) (dd2_to_legacy at2) in
  let bin_op = DD.is_included in
  print_endline "Testing DashDot2.is_included";
  exhaustive_test_binary oracle bin_op max false DD.HalfLine.string_of string_of_bool 

let exhaustive_test_unary oracle un_op max dummy string_of_operand string_of_result =
  let next n = if n < max then n + 1 else raise Exit in
  let next = DD.next next in
  let at = ref DD.empty in
  let fe1 = ref dummy in
  let fe2 = ref dummy in
  let ok = ref true in
  let nb_of_tests = Int64.(shift_left 2L (2*max+1)) in
  let one_percent = Int64.(to_int (div nb_of_tests 100L)) in
  let () = Printf.printf "There are %s tests to perform\n" 
    (Int64.to_string nb_of_tests) in 
  let percent = ref 0 in
  let counter = ref 0 in
  while !ok do
    fe1 := oracle !at;
    fe2 := un_op !at;
    ok  := !fe1 = !fe2;
    incr counter;
    if not !ok then (
      print_endline "Mismatch:";
      Printf.printf "operand   = %s\n" (string_of_operand !at);
      Printf.printf "oracle    = %s\n" (string_of_result !fe1);
      Printf.printf "candidate = %s\n" (string_of_result !fe2));
    begin
        try at := next !at 
        with Exit -> ok := false
    end;
    if !counter = one_percent 
    then (counter := 0; incr percent; if !percent < 100 then Printf.printf "%i%%\r%!" !percent)
  done;
  print_endline "100%"
  
let wrapper legacy_un_op = 
  fun at1 -> dd2_of_legacy (legacy_un_op (dd2_to_legacy at1))

let exhaustive_complement max = 
  let oracle = wrapper Legacy.complement in
  let un_op = DD.complement in
  print_endline "Testing DashDot2.complement";
  exhaustive_test_unary oracle un_op max DD.empty DD.HalfLine.string_of DD.HalfLine.string_of

let exhaustive_closure_on_half_line max = 
  let oracle = wrapper HL_legacy.closure in
  let un_op = DD.HalfLine.closure in
  print_endline "Testing DashDot2.closure on half-line";
  exhaustive_test_unary oracle un_op max DD.empty DD.HalfLine.string_of DD.HalfLine.string_of

let exhaustive_closure_on_circle max = 
  let oracle = wrapper Ci_legacy.closure in
  let un_op = DD.Circle.closure in
  print_endline "Testing DashDot2.closure on circle";
  exhaustive_test_unary oracle un_op max DD.empty DD.HalfLine.string_of DD.HalfLine.string_of

let exhaustive_interior_on_half_line max = 
  let oracle = wrapper HL_legacy.interior in
  let un_op = DD.HalfLine.interior in
  print_endline "Testing DashDot2.interior on half-line";
  exhaustive_test_unary oracle un_op max DD.empty DD.HalfLine.string_of DD.HalfLine.string_of

let exhaustive_interior_on_circle max = 
  let oracle = wrapper Ci_legacy.interior in
  let un_op = DD.Circle.interior in
  print_endline "Testing DashDot2.interior on circle";
  exhaustive_test_unary oracle un_op max DD.empty DD.HalfLine.string_of DD.HalfLine.string_of

let exhaustive_interior_on_half_line max = 
  let oracle = wrapper HL_legacy.interior in
  let un_op = DD.HalfLine.interior in
  print_endline "Testing DashDot2.interior on half-line";
  exhaustive_test_unary oracle un_op max DD.empty DD.HalfLine.string_of DD.HalfLine.string_of

let exhaustive_mem max =
  let next_value n = if n < max then n + 1 else raise Exit in
  let next = DD.next next_value in
  let p = ref I.zero in
  let at = ref DD.empty in
  let fe1 = ref false in
  let fe2 = ref false in
  let ok = ref true in
  let nb_of_tests = Int64.(shift_left 2L (2*max+1)) in
  let nb_of_tests = Int64.mul (Int64.of_int (max+1)) nb_of_tests in
  let one_percent = Int64.(to_int (div nb_of_tests 100L)) in
  let () = Printf.printf "Testing DashDot2.mem\nThere are %s tests to perform\n" 
    (Int64.to_string nb_of_tests) in 
  let percent = ref 0 in
  let counter = ref 0 in
  while !ok do
    fe1 := Legacy.belongs_to !p (dd2_to_legacy !at);
    fe2 := DD.mem !p !at;
    ok  := !fe1 = !fe2;
    incr counter; 
    if not !ok then (
      print_endline "Mismatch:";
      Printf.printf "operand_1 = %s\n" (string_of_int !p);
      Printf.printf "operand_2 = %s\n" (DD.HalfLine.string_of !at);
      Printf.printf "oracle    = %s\n" (string_of_bool !fe1);
      Printf.printf "candidate = %s\n" (string_of_bool !fe2));
    begin
      try p := next_value !p
      with Exit -> (
        try at := next !at; p := I.zero 
        with Exit -> ok := false);
    end;
    if !counter = one_percent 
    then (counter := 0; incr percent; if !percent < 100 then Printf.printf "%i%%\r%!" !percent else print_endline "100%")
  done

let exhaustive_regions max = 
  let next n = if n < max then n + 1 else raise Exit in
  let next = DD.next next in
  let x = ref DD.empty in
  try
    while true do 
      print_endline (DD.HalfLine.string_of !x);
      print_endline (DD.Circle.string_of !x);
      print_endline "";
      x := next !x
    done
  with Exit -> ()

let exhaustive_all max =
  exhaustive_mem (2 * max);
  exhaustive_is_included max;
  exhaustive_complement (2 * max + 1);
  exhaustive_meet max;
  exhaustive_join max;
  exhaustive_difference max;
  exhaustive_future_extension_on_half_line max;
  exhaustive_future_extension_on_circle max;
  exhaustive_past_extension_on_half_line max;
  exhaustive_past_extension_on_circle max;
  exhaustive_interior_on_half_line (2 * max + 1);
  exhaustive_interior_on_circle (2 * max + 1);
  exhaustive_closure_on_half_line (2 * max + 1);
  exhaustive_closure_on_circle (2 * max + 1)
  
let rec of_string tl = Str.(
  match tl with 
    | [] | [Delim "["; Delim "]"] | [Delim "{"; Delim "}"] -> (DD.empty)
    | [Delim l ; Text a ; Text "oo" ; Delim "["] 
    | [Delim l ; Text a] -> (
        let a = int_of_string a in
        let l = (l = "[") in
        DD.of_interval (I.terminal l a))
    | Delim l :: Text a :: Text b :: Delim r :: tl -> (
        let a = int_of_string a in
        let b = int_of_string b in
        let () = assert (a < b) in
        let l = (l = "[") in
        let r = (r = "]") in
        DD.(join (of_interval (I.bounded l a b r)) (of_string tl)))
    | Delim "[" :: Text a :: Delim "]" :: tl 
    | Delim "{" :: Text a :: Delim "}" :: tl 
    | Text a :: tl -> (
        DD.(join (of_interval (I.atom (int_of_string a)))  (of_string tl)) )
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
    (DD.HalfLine.string_of operand)
    (DD.HalfLine.string_of result)
    (DD.HalfLine.string_of expected_result)

let test_binary op_name operator operand1 operand2 expected_result =
  let result = operator operand1 operand2 in
  let expected_result = expected_result in
  if result <> expected_result
  then Printf.printf "%s\n %s\n %s\n= %s\nbut\n %s\nwas expected.\n%!"
    op_name
    (DD.HalfLine.string_of operand1)
    (DD.HalfLine.string_of operand2)
    (DD.HalfLine.string_of result)
    (DD.HalfLine.string_of expected_result)

let operator_name = ref ""

let file_name = ref ""

let operator = ref (Unary (fun x -> x)) (*dummy default value*)

let preparing op_name () = 
  operator_name := op_name ;
  operator := operator_of_string op_name

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

let exhaustive_intervals max = 
  let next n = if n < max then n + 1 else raise Exit in
  let next = I.next next in
  let x = ref (I.atom 0) in
  try
    while true do
      print_string (I.string_of "[" "]" "{" "}" "+oo"  !x);
      x := next !x;
      print_endline ""
    done
  with Exit -> print_endline ""

let exhaustive_all_in_parallel max = 
  let command = Printf.sprintf    
  "./main --exhaustively-testing-future-extension-on-half-line %i &\ 
   ./main --exhaustively-testing-future-extension-on-circle %i &\ 
   ./main --exhaustively-testing-past-extension-on-half-line %i &\ 
   ./main --exhaustively-testing-past-extension-on-circle %i &\
   ./main --exhaustively-testing-join %i &\
   ./main --exhaustively-testing-meet %i" 
   max max max max max max in 
  ignore (Sys.command command)


let command_line_options = [
  "--future-extension-on-half-line", Arg.Unit (preparing "hl_future"), "Test future_extension on the half-line" ;
  "--past-extension-on-half-line", Arg.Unit (preparing "hl_past"), "Test past_extension on the half-line" ;
  "--future-extension-on-circle", Arg.Unit (preparing "ci_future"), "Test ci_future_extension" ;
  "--past-extension-on-circle", Arg.Unit (preparing "ci_past"), "Test ci_past_extension" ;
  "--meet", Arg.Unit (preparing "meet"), "Test meet" ;
  "--join", Arg.Unit (preparing "join"), "Test join" ;
  "--complement", Arg.Unit (preparing "complement"), "Test complement" ;
  "--interior-on-half-line", Arg.Unit (preparing "hl_interior"), "Test interior on half-line" ;
  "--closure-on-half-line", Arg.Unit (preparing "hl_closure"), "Test closure on half-line" ;
  "--interior-on-circle", Arg.Unit (preparing "hl_interior"), "Test interior on circle" ;
  "--closure-on-circle", Arg.Unit (preparing "hl_closure"), "Test closure on circle" ;
  "--enumerate-intervals",Arg.Int (exhaustive_intervals), "Compare the results of the current implementation with a previous one, on all possible intervals up to some extent.";
  "--enumerate-regions",Arg.Int (exhaustive_regions), "Compare the results of the current implementation with a previous one, on all possible regions up to some extent.";
  "--exhaustively-testing-all",Arg.Int (exhaustive_all),"Compare the new implementation of all operators on half-line and circle with the current one";
  "--exhaustively-testing-all-in-parallel",Arg.Int (exhaustive_all_in_parallel),"Compare the new implementation of all operators on half-line and circle with the current one";
  "--exhaustively-testing-future-extension-on-half-line",Arg.Int (exhaustive_future_extension_on_half_line),"Compare the new implementation of future_extension on half-line (~ 60 LoC) with the current one (~ 500 LoC)";
  "--exhaustively-testing-future-extension-on-circle",Arg.Int (exhaustive_future_extension_on_circle),"Compare the new implementation of future_extension on circle (~ 60 LoC) with the current one (~ 1000 LoC)";
  "--exhaustively-testing-past-extension-on-half-line",Arg.Int (exhaustive_past_extension_on_half_line),"Compare the new implementation of past_extension on half-line (~ 60 LoC) with the current one (~ 500 LoC)";
  "--exhaustively-testing-past-extension-on-circle",Arg.Int (exhaustive_past_extension_on_circle),"Compare the new implementation of past_extension on circle (~ 60 LoC) with the current one (~ 1000 LoC)";
  "--exhaustively-testing-join",Arg.Int (exhaustive_join),"Compare the new implementation of join with the current one)";
  "--exhaustively-testing-meet",Arg.Int (exhaustive_meet),"Compare the new implementation of meet with the current one";
  "--exhaustively-testing-difference",Arg.Int (exhaustive_difference),"Compare the new implementation of difference with the current one";
  "--exhaustively-testing-complement",Arg.Int (exhaustive_complement),"Compare the new implementation of complement with the current one";
  "--exhaustively-testing-interior-on-half-line",Arg.Int (exhaustive_interior_on_half_line),"Compare the new implementation of interior on half-line with the current one";
  "--exhaustively-testing-interior-on-circle",Arg.Int (exhaustive_interior_on_circle),"Compare the new implementation of interior on circle with the current one";
  "--exhaustively-testing-closure-on-half-line",Arg.Int (exhaustive_closure_on_half_line),"Compare the new implementation of closure on half-line with the current one";
  "--exhaustively-testing-closure-on-circle",Arg.Int (exhaustive_closure_on_circle),"Compare the new implementation of closure on circle with the current one";
  "--exhaustively-testing-is_included",Arg.Int (exhaustive_is_included),"Compare the new implementation of is_included with the current one";
  "--exhaustively-testing-mem",Arg.Int (exhaustive_mem),"Compare the new implementation of mem with the current one";
]

let msg = "This tool tests the DashDot library, which implements boolean, topological, \n\
    and order theoretic operations on the finite union of intervals 
    (respectively on finite unions of arcs).\n
\
    The tests to perform are stored in a file given as an argument.\n
\
    In case of a unary operator, each test is given by two lines, the first one \n\
    being the operand, the second one being the expected result.\n
\
    In case of a binary operator, each test is given by three lines, the first \n\
    two of them being the operands, the third one being the expected result.\n
\
    A value is described by a sequence of disconnected intervals such that if I \n\
    appears before J in the description, then I < J.\n
\
    NonEmptyIntervals are 
\  [x] : singleton
\  [x y] : closed interval
\  ]x y[ : open interval
\  [x y[ : left-closed right-open interval
\  ]x y] : left-open right-closed interval
\  [x    : closed terminal segment
\  ]x    : open terminal segment

E.g.: [0] ]1 3[ [3 represents {0} U ]1,3[ U [3,+oo[.
  
Lines starting with % are comments. Empty lines are ignored. A line made of 
blanks represents the empty set.\n\ 
The empty set can also be represented by {} or [].

Options are:"

let () = Arg.parse command_line_options anon_fun msg