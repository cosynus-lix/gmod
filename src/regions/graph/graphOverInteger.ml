let normalize_string s =
  let s = Str.global_replace (Str.regexp "[\n\t]") "" s in
  Str.global_replace (Str.regexp "\\([ ]\\)+") " " s

let semicolon_split s = Str.split (Str.regexp ";") s

module A = struct

  type t = int
  let compare = compare
  let string_of = string_of_int

end (* A *)

module V = A

module G = Graph.Raw(A)(V)

let of_list l =
  let counter = ref 0 in
  let add_an_arrow a (src,tgt) = 
    let arrow = !counter in
    let () = incr counter in
    G.AM.add arrow (src,tgt) a in
  let endpoints = List.fold_left add_an_arrow G.AM.empty l in
  let neighbors = G.neighbors_from_endpoints endpoints in
  {G.endpoints;G.neighbors}

let of_string s =
  let s = normalize_string s in
  let l = semicolon_split s in
  let l = List.map (fun a -> 
    let a = Str.split (Str.regexp " ") a in
    match a with
    | [src;tgt] -> (int_of_string src,int_of_string tgt)
    | _ -> assert false)
    l in
  of_list l

