module DD = DashDot.Make(Integer)

module HL = DD.HalfLine

module Ci = DD.Circle

(* of_string attend une forme correcte et ne fait pas de vérifications. *)

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

let arity_of_string s = match s with 
  | "meet" 
  | "join" 
  | "hl_future" 
  | "hl_past" 
  | "ci_future" 
  | "ci_past" -> 2
  | "compl"
  | "hl_interior"
  | "ci_interior"
  | "hl_closure"
  | "ci_closure" -> 1
  | _ -> failwith ("Unknown operator " ^ s)

type operator = Unary of (DD.t -> DD.t) | Binary of (DD.t -> DD.t -> DD.t)
  
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
  
(*
let hl_future_extension_tests = [
  ("[3]","]3","]3");
  ("[0]","]0","]0");
  ("[3]","[0]","");
  ("[3]","[0 3[","");
  ("[3]","[0 3]","[3]");
  ("[3]","[0 2[","");
  ("[3]","[0 2]","");
]
*)

(*
let testing_hl_future_extension () = 
  List.iter 
    (fun (a,b,c) -> test_binary "hl_future" a b c)
    hl_future_extension_tests
*)
    
    
(*
let hl_past_extension_tests = [
  ("[3]","]3","");
  ("[0]","]0","");
  ("[0]","]3","");
  ("[4]","[2 4[","[2 4[");
  ("[5 8[","[0] ]2 5[ [8 9]","]2 5[ ")
]
*)

(*
let testing_hl_past_extension () = 
  List.iter 
    (fun (a,b,c) -> test_binary "hl_past" a b c)
    hl_past_extension_tests
*)

let operator_name = ref ""

let operator = ref (Unary (fun x -> x)) (*dummy default value*)

let preparing op_name () = 
  operator_name := op_name ;
  operator := operator_of_string op_name
  

let command_line_options = [
  "-hlf",Arg.Unit (preparing "hl_future"),"Test hl_future_extension" ;
  "-hlp",Arg.Unit (preparing "hl_past"),"Test hl_past_extension" ;
  "-cif",Arg.Unit (preparing "ci_future"),"Test ci_future_extension" ;
  "-cip",Arg.Unit (preparing "ci_past"),"Test ci_future_extension" ;
  "-meet",Arg.Unit (preparing "meet"),"Test meet" ;
  "-join",Arg.Unit (preparing "join"),"Test join" ;
  "-complement",Arg.Unit (preparing "complement"),"Test complement" ;
]



let anon_fun s = 
  let chan = open_in s in
  let iterator = 
    fun () -> 
      try input_line chan 
      with End_of_file -> (
        close_in chan;
        raise Exit) in
  let operator = operator_of_string !operator_name in
  match operator with 
    | Unary operator -> (
        try
          while true do
            test_unary !operator_name operator 
              (of_string (iterator ()))
              (of_string (iterator ()))
          done
        with Exit -> print_endline "End of test")
    | Binary operator -> (
        try
          while true do
            test_binary !operator_name  operator
              (of_string (iterator ()))
              (of_string (iterator ()))
              (of_string (iterator ()))
          done
        with Exit -> print_endline "End of test")
  
  
  
  
  
(*
let testing_hl_future_extension () =
  let () = print_endline "Testing hl_future_extension" in 
  try
    while true do
      let op1 = of_string (iterator ()) in
      let op2 = of_string (iterator ()) in
      let expected = of_string (iterator ()) in
      test_binary "hl_future"
    done
  with Exit -> print_endline "End of test"
*)


let msg = ""

let () = Arg.parse command_line_options anon_fun msg
