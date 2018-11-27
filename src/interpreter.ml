open Mystdlib
open Lang

(** State of the interpreter. *)
module State = struct
  type t = ((string * E.value) list) list

  let empty : t = [[]]

  let push (s:t) : t = []::s

  let pop (s:t) : t = List.tl s

  let add (s:t) x : t =
    let s0, s = List.hd s, List.tl s in
    let s0 = (x,E.Bot)::s0 in
    s0::s

  let get (s:t) x =
    let ans = ref E.Bot in
    try
      List.iter
        (List.iter (fun (y,v) -> if x = y then (ans := v; raise Exit)))
        s;
      raise Not_found
    with
    | Exit -> !ans

  let rec set (s:t) x v : t =
    let rec aux = function
      | (y,_)::s when x=y -> Some ((x,v)::s)
      | (y,v)::s ->
         Option.bind (fun s -> Some ((y,v)::s)) (aux s)
      | [] -> None
    in
    match s with
    | s0::s ->
       (
         match aux s0 with
         | Some s0 -> s0::s
         | None -> s0::(set s x v)
       )
    | [] -> []
end

(* let eval = function *)
