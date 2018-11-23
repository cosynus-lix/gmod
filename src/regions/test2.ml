module I = Interval2.Raw(Integer)

module Legacy = ODA.BooleanAlgebra(Integer)

module HL_legacy = ODA.RawHalfLine(Integer)

module Ci_legacy = ODA.RawCircle(Integer)

module DD2 = DashDot2.Raw(I)

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
  |  Legacy.Iso x :: hl -> (I.singleton x) :: dd2_of_legacy hl
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
    (DD2.string_of operand)
    (DD2.string_of result)
    (DD2.string_of expected_result)

let test_binary op_name operator operand1 operand2 expected_result =
  let result = operator operand1 operand2 in
  let expected_result = expected_result in
  if result <> expected_result
  then Printf.printf "%s\n %s\n %s\n= %s\nbut\n %s\nwas expected.\n%!"
    op_name
    (DD2.string_of operand1)
    (DD2.string_of operand2)
    (DD2.string_of result)
    (DD2.string_of expected_result)

let exhaustive_test_binary oracle bin_op max dummy string_of_operand string_of_result =
  let next n = if n < max then n + 1 else raise Exit in
  let next = DD2.next next in
  let at1 = ref DD2.empty in
  let at2 = ref DD2.empty in
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
        try at1 := next !at1; at2 := DD2.empty 
        with Exit -> ok := false);
    end;
    if !counter = one_percent 
    then (counter := 0; incr percent; if !percent < 100 then Printf.printf "%i%%\r%!" !percent else print_endline "100%")
  done

let wrapper legacy_bin_op = 
  fun at1 at2 ->
    let at1' = dd2_to_legacy at1 in
    let at2' = dd2_to_legacy at2 in
    let at3' = legacy_bin_op at1' at2' in
    dd2_of_legacy at3'

let exhaustive_join max = 
  let oracle = wrapper Legacy.union in
  let bin_op = DD2.join in
  print_endline "Testing DashDot2.join";
  exhaustive_test_binary oracle bin_op max DD2.empty DD2.string_of DD2.string_of

let exhaustive_meet max = 
  let oracle = wrapper Legacy.intersection in
  let bin_op = DD2.meet in
  print_endline "Testing DashDot2.meet";
  exhaustive_test_binary oracle bin_op max DD2.empty DD2.string_of DD2.string_of

let exhaustive_future_extension_on_half_line max = 
  let oracle = wrapper (fun x y -> Legacy.union x (HL_legacy.future_extension x y)) in
  let bin_op = DD2.HalfLine.future_extension in
  print_endline "Testing DashDot2.HalfLine.future_extension";
  exhaustive_test_binary oracle bin_op max DD2.empty DD2.string_of DD2.string_of

let exhaustive_past_extension_on_half_line max = 
  let oracle = wrapper (fun x y -> Legacy.union x (HL_legacy.past_extension x y)) in
  let bin_op = DD2.HalfLine.past_extension in
  print_endline "Testing DashDot2.HalfLine.past_extension";
  exhaustive_test_binary oracle bin_op max DD2.empty DD2.string_of DD2.string_of

let exhaustive_future_extension_on_circle max = 
  let oracle = wrapper (fun x y -> Legacy.union x (Ci_legacy.future_extension x y)) in
  let bin_op = DD2.Circle.future_extension in
  print_endline "Testing DashDot2.Circle.future_extension";
  exhaustive_test_binary oracle bin_op max DD2.empty DD2.string_of DD2.string_of

let exhaustive_past_extension_on_circle max = 
  let oracle = wrapper (fun x y -> Legacy.union x (Ci_legacy.past_extension x y)) in
  let bin_op = DD2.Circle.past_extension in
  print_endline "Testing DashDot2.Circle.past_extension";
  exhaustive_test_binary oracle bin_op max DD2.empty DD2.string_of DD2.string_of

let exhaustive_test_unary oracle un_op max dummy string_of_operand string_of_result =
  let next n = if n < max then n + 1 else raise Exit in
  let next = DD2.next next in
  let at = ref DD2.empty in
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
    then (counter := 0; incr percent; if !percent < 100 then Printf.printf "%i%%\r%!" !percent else print_endline "100%")
  done

let wrapper legacy_un_op = 
  fun at1 -> dd2_of_legacy (legacy_un_op (dd2_to_legacy at1))

let exhaustive_complement max = 
  let oracle = wrapper Legacy.complement in
  let un_op = DD2.complement in
  print_endline "Testing complement";
  exhaustive_test_unary oracle un_op max DD2.empty DD2.string_of DD2.string_of


let exhaustive_regions max = 
  let next n = if n < max then n + 1 else raise Exit in
  let next = DD2.next next in
  let x = ref DD2.empty in
  try
    while true do 
      print_endline (DD2.string_of !x);
      x := next !x
    done
  with Exit -> ()


let () = exhaustive_past_extension_on_circle 4
let () = exhaustive_future_extension_on_circle 4
let () = exhaustive_past_extension_on_half_line 4
let () = exhaustive_future_extension_on_half_line 4
let () = exhaustive_complement 9
let () = exhaustive_join 4
let () = exhaustive_meet 4
