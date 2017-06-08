open Common
open Type
open AbstractSyntax


(* Attention, le point "origine" i.e. le tableau rempli de 0 ne
   correspond pas à l'état initial... *)

let generic ?runpro f max_coord cdh (* a dihomotopy class given as an area whose regular values are integers *) =
  let max_coord = match runpro with
    | None -> max_coord
    | Some rp -> Array.init (Array.length rp) (fun k -> Array.length rp.(k).code + 1)
  in
  let d = AC.dimension cdh in
  let path = ref [] in
  let ip = C.origin d in (* Il faudrait une fonction qui donne le point de départ de la classe de dihomotopie *)
  let not_finished () =
    let ans = ref false in
      for i = 0 to Array.length ip - 1 do
	if ip.(i) < max_coord.(i) 
	then ans := true
      done;
      !ans
  in
  let rec find_next_move k = (* Cette fonction devrait pouvoir prendre en compte les multi-instructions *)
    (* elle devrait donc prendre un tableau d'entier en entrée  *)
    ip.(k) <- ip.(k)+1 ;
    if AC.belongs_to ip cdh && ip.(k) < max_coord.(k) then
      (
        try
          path := (f ?runpro k ip)::(!path) 
        with
          | Invalid_argument "index out of bounds" -> 
	      failwith "Path_extract (debug): the value of runpro may not match max_coord"
      )
    else
      (
        ip.(k) <- (ip.(k)-1);
        find_next_move (k+1)
      )
  in
    (
      try
        (
          while not_finished () do
            find_next_move 0
          done;
          List.rev !path
        )
      with
        | Invalid_argument "index out of bounds" -> List.rev !path (* cas d'un chemin qui aboutit à un deadlock *)
    )

  (* Chaque entrée de max_coord contient la valeur à atteindre *)

let pid ?runpro k ip = k

let pid max_coord cdh = generic pid max_coord cdh

let action ?runpro k ip = fst (get_some runpro).(k).code.(ip.(k)-1)

let action runpro max_coord cdh = generic ~runpro action max_coord cdh

let instruction runpro k ip = ((get_some runpro).(k)).(ip.(k)-1)

let point ?runpro k ip = ip.(k)

let point max_coord cdh = generic point max_coord cdh

let tag ?runpro k ip = snd (get_some runpro).(k).code.(ip.(k)-1)

let tag runpro max_coord cdh = generic ~runpro tag max_coord cdh
