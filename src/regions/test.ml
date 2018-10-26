module DD = DashDot.Make(Integer)

module HL = DD.HalfLine

module Ci = DD.Circle

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
  | "hl_past" -> Binary DD.hl_past_extension
  | "ci_future" -> Binary DD.Circle.future_extension
  | "ci_past" -> Binary DD.ci_past_extension
  | "compl" -> Unary DD.complement
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

let perform_all_tests dir =
  anon_fun (dir ^ "hl_future.test");
  anon_fun (dir ^ "ci_future.test");
  anon_fun (dir ^ "hl_past.test");
  anon_fun (dir ^ "ci_past.test");
  anon_fun (dir ^ "hl_closure.test");
  anon_fun (dir ^ "ci_closure.test");
  anon_fun (dir ^ "hl_interior.test");
  anon_fun (dir ^ "ci_interior.test");
  anon_fun (dir ^ "meet.test");
  anon_fun (dir ^ "join.test");
  anon_fun (dir ^ "complement.test")
  
let preparing op_name () = 
  operator_name := op_name ;
  operator := operator_of_string op_name

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
  [x y[ : right-closed left-open interval
  ]x y] : right-open left-closed interval
  [x    : closed terminal segment
  ]x    : open terminal segment

E.g.: [0] ]1 3[ [3 represents {0} U ]1,3[ U [3,+oo[.
  
Choose an option, and a file or a directory.

Lines starting with % are comments. Empty lines are ignored.
Options are:"

let () = Arg.parse command_line_options anon_fun msg
