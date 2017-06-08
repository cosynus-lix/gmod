(*
open Common
*)

module Gui_parameters =
struct
  let width  = ref 800
  let height = ref 600
  let compute_forbidden = ref false
  let compute_state = ref false
  let compute_deadlock = ref false
  let compute_hopeful = ref false
  let compute_hazardous = ref false
  let compute_safe = ref false
  let compute_reachable = ref false
  let compute_unreachable = ref false
  let compute_tide = ref false
  let compute_factorization = ref false
end

(* Global variables for analyzer *)

let name_of_the_input_file = ref (None:string option)

let have_gtk = ref false

let display_with_gtk_gui =
  ref (fun () -> Printf.printf "Oplate was not compiled with GTK support!\n%!")

let name_of_file_to_analyze = ref ""

let output_scale = ref 0

let proj0 (a,b,c) = a
let proj1 (a,b,c) = b
let proj2 (a,b,c) = c

exception Dead_end

let rec map_n_sweep ?(accu=[]) f l =
  match l with
    | x::l ->
      let y = f x in
	      if not(List.mem y accu) 
	      then map_n_sweep ~accu:(y::accu) f l
	      else map_n_sweep ~accu:accu f l
    | [] -> accu


let rec segment_initial_croistart k start =
  if k = 0 then [] else ((segment_initial_croistart (k-1) start)@[k+start])

(* La fontion (cartesian_next) renvoie la suite qui succède à (l) dans
   l'ensemble, ordonné lexicographiquement, des suites finies d'entiers
   de longeur (List.length l) dont tous les termes sont inférieurs ou
   égaux à (n). Cet ensemble est fini et si (next) est appelée avec pour
   argument le dernier élément de la liste, elle lève l'exception
   (Exit) *)

let cartesian_next l n =
  let rec aux l = match l with
    | []   -> raise Exit (*Dead_end*)
    | k::l -> if k < n then (k+1)::l else (0::(aux l))
  in
  aux l

let cartesian_next_array a n =
  let pos = ref 0 in
  while n <= a.(!pos) do
    pos := !pos + 1
  done;
  let answer = Array.sub a !pos ((Array.length a)-(!pos)) in
  answer.(0) <- answer.(0)+1;
  Array.append (Array.make (!pos) 0) answer


let change_extension file_name new_extension =
  try Filename.chop_extension file_name ^ new_extension
  with Invalid_argument _ -> file_name ^ new_extension

let unknown_instruction () = Glyph.end_of_proof ()
