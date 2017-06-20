module DD = DashDot.Make(Integer)

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
  | "hl_past" -> DD.HalfLine.past_extension
  | "ci_future" -> DD.Circle.future_extension
  | "ci_past" -> DD.Circle.past_extension

let un_op s = match s with
  | "compl" -> DD.complement
  | "hl_interior" -> DD.HalfLine.interior
  | "ci_interior" -> DD.Circle.interior
  | "hl_closure" -> DD.HalfLine.closure
  | "ci_closure" -> DD.Circle.closure

let test_binary op s1 s2 =
  


  

let i = of_string "[3 4] ]6 7[]7 8] [7]"
let () = print_endline ("i = " ^ (DD.HalfLine.string_of i ))
  
let i1 = DD.interval true true 0 1
let i2 = DD.interval true true 1 2
let i3 = DD.meet i1 i2

let i1 = DD.interval true true 0 1
let i2 = DD.interval false true 1 2
let i3 = DD.join i1 i2

let i1 = DD.interval true false 0 1
let i2 = DD.interval true true 1 2
let i3 = DD.join i1 i2

let i1 = DD.interval true false 0 1
let i2 = DD.interval false true 1 2
let i3 = DD.join i1 i2

let i1 = DD.interval true true 0 1
let i2 = DD.interval false true 1 2
let i3 = DD.meet i1 i2

let i1 = DD.interval true true 1 4
let i2 = DD.interval true true 2 3
let i3 = DD.meet i1 i2

let i1 = DD.interval false false 1 7
let i2 = DD.interval true true 1 7
let i3 = DD.difference i2 i1

let i1 = DD.final false 1
let i2 = DD.interval true false 0 1
let i3 = DD.hl_past_extension i1 i2

let i1 = DD.atom 7
let i2 = DD.coatom 7
let i3 = DD.hl_past_extension i1 i2

let i1 = DD.interval true true 2 3
let i2 = DD.final true 3
let i3 = DD.hl_past_extension i1 i2

let i1 = DD.atom 3
let i2 = DD.coatom 2
let i3 = DD.hl_past_extension i1 i2

let () = 
  print_endline ("i1 = " ^ (DD.HalfLine.string_of i1));
  print_endline ("i2 = " ^ (DD.HalfLine.string_of i2));
  print_endline ("i3 = " ^ (DD.HalfLine.string_of i3))
