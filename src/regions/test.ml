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

let bin_op s = match s with
  | "meet" -> DD.meet
  | "join" -> DD.join
  | "hl_future" -> DD.HalfLine.future_extension
  | "hl_past" -> DD.hl_past_extension
  | "ci_future" -> DD.Circle.future_extension
  | "ci_past" -> DD.ci_past_extension
  | _ -> failwith ("Unknown operator " ^ s)

let un_op s = match s with
  | "compl" -> DD.complement
  | "hl_interior" -> DD.HalfLine.interior
  | "ci_interior" -> DD.Circle.interior
  | "hl_closure" -> DD.HalfLine.closure
  | "ci_closure" -> DD.Circle.closure
  | _ -> failwith ("Unknown operator " ^ s)

let test_binary operator operand1 operand2 expected_result =
  let op_name = operator in
  let operator = bin_op operator in
  let operand1 = of_string operand1 in
  let operand2 = of_string operand2 in
  let result = operator operand1 operand2 in
  let expected_result = of_string expected_result in
  if result <> expected_result
  then Printf.printf "%s\n %s\n %s\n= %s\nbut %s was expected\n%!"
    op_name
    (HL.string_of operand1)
    (HL.string_of operand2)
    (HL.string_of result)
    (HL.string_of expected_result)
  
let hl_future_extension_tests = [
  ("[3]","]3","]3");
  ("[0]","]0","]0");
  
]

let testing_hl_future_extension = 
  List.iter 
    (fun (a,b,c) -> test_binary "hl_future" a b c)
    hl_future_extension_tests
    
    
let hl_past_extension_tests = [
  ("[3]","]3","");
  ("[0]","]0","");
  ("[0]","]3","");
  ("[4]","[2 4[","[2 4[");
  ("[5 8[","[0] ]2 5[ [8 9]","]2 5[ ")
]

let testing_hl_past_extension = 
  List.iter 
    (fun (a,b,c) -> test_binary "hl_past" a b c)
    hl_past_extension_tests
    
    
