module I = NonEmptyInterval.Raw(Integer)

module DD = DashDot.Raw(I)

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
