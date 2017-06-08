(* Torus.ml *)

module type In =
sig
  module IntSet:
    sig 
      include Set.S
      val segment_initial: ?start:t -> int -> t
    end with type elt = int
  module Array:
  sig
    val cartesian_next_array_consumer: int array -> int -> unit
    val cartesian_next_array_consumer2: int array -> int array -> unit
  end
end with type IntSet.elt = int

module type S =
sig
type underlying

  exception Undefined
  type arc
  type point 
  type regular_value
  type ints
  type t
  val of_arc: arc -> t
  val compare_points: point -> point -> int
  val affine_dimension: t -> int
  val atom: point -> t
  val base_coordinates: t -> ints
  val base_dimension: t -> int
  val belongs_to: point -> t -> bool 
  val belongs_to_boundary: point -> t -> bool 
  val bounded: bool -> bool -> regular_value -> regular_value -> t 
  val closure: t -> t 
  val compare: t -> t -> int 
  val compare_arc: arc -> arc -> int
  val complement: t -> (t list) 
  val cylinder: int -> ints -> t -> t
  val dimension: t -> int
  val degenerate_coordinates: t -> ints
  val empty: int -> t
  val in_the_future_of: t -> t -> t
  val in_the_past_of: t -> t -> t
  val intersection: t -> t -> (t list)
  val full: int -> t 
  val glb: t -> point
  val in_touch: t -> t -> bool
  val initial: bool -> regular_value -> t
  val interior: t -> t
  val is_included: t -> t -> bool
  val is_empty: t -> bool 
  val is_not_empty: t -> bool 
  val is_full: t -> bool 
  val is_not_full: t -> bool 
  val lub: t -> point 
  val normalize: t -> t 
  val of_list: (arc list) -> t
  val of_array: (arc array) -> t
  val origin: int -> point 
  val product: t -> t -> t 
  val projection: ints -> t -> t
  val string_of: t -> string
  val string_of_arc: arc -> string
  val terminal: bool -> regular_value -> t
  val to_list: t -> (arc list)
  val to_array: t -> (arc array)
  val underlying: t -> underlying
end

module Make(Overall:In)(A:Arc.S):
(S with type regular_value = A.regular_value
  and type point = A.regular_value array
  and type arc = A.t
  and type ints = Overall.IntSet.t)
=
struct
  exception Undefined
  exception Empty_or_Full_Arc
  exception Exit_compare of int
  type regular_value = A.regular_value
  type point = regular_value array
  type arc = A.t
  type ints = Overall.IntSet.t
  let compare_arc = A.compare
  let string_of_arc a = A.string_of a
  type underlying = A.underlying array
  let origin d = Array.make d (A.least_regular_value)
  
  
  let compare_points p1 p2 =
    let n = Array.length p1 in
    let () = assert (n = Array.length p2) in
    let delta = ref 0 in
    let k = ref 0 in
    while !delta = 0 && !k < n do
      delta := A.compare_regular_values p1.(!k) p2.(!k);
      incr k
    done;
    !delta



  (* To([||]) is the neutral element of the cartesian product in the graded pseudo lattice of tori. *)

  type t =
    | To of arc array
    | Et of int

  let of_arc arc = 
    if A.is_not_empty arc
    then To [|arc|] 
    else Et 1     

  let to_array = function
    | To ara -> ara
    | Et _   -> raise Undefined
	
  let to_list = function 
    | To ara -> Array.fold_right (fun a accu -> a::accu) ara []
    | Et _   -> raise Undefined

  (* exception Split of (t list) *)

  let affine_dimension = function
    | To ara ->
        Array.fold_left
          (fun accu it -> if (A.is_atomic it) then accu else (accu+1))
          1
          ara
    | Et n -> 0

  let dimension = function
    | To ara -> Array.length ara
    | Et n   -> n

  let underlying c = Array.make (dimension c) (A.underlying A.empty)

  let test_dimension c1 c2 str =
    let	d1 = dimension c1 in
    let	d2 = dimension c2 in
      (* failwith ("The arguments of " ^ str ^ " are heterogeneous.") *)
      assert (d1 = d2);
      d1

  let string_of c =
    match c with 
      | To(ai) -> (
        if ai <> [||] then
          let first = A.string_of ai.(0) in 
          Array.fold_left (fun s i -> s^"*"^(A.string_of i)) first (Array.sub ai 1 ((Array.length ai)-1))
        else "Zero dimension non empty torus" (* full torus of dimension 0 *))
      | Et(n)  -> "@("^(string_of_int n)^")"


  let empty n = Et n 

  let full n = To (Array.make n A.full)

  let normalize c =
    match c with
      | To(ara) -> (
        let n = Array.length ara in
        try 
          for i = 0 to n - 1 do
            let current = A.normalize (ara.(i)) in
            if A.is_empty current then raise Exit;
            ara.(i) <- current
          done;
          c
        with Exit -> Et n)
      | _ -> c



  (* Cette fonction est peut-être buggée *)

  let complement_version_1 c = match c with
    | To(ara) ->
	let d = dimension c
	in
	  (
	    if d <> 0
	    then
	      let rec aux pos =
		(
		  if (pos < d)
		  then
		    let answer = (Array.make d A.full)
		    in
		    let carc = (A.complement (Array.get ara pos))
		    in
		      if carc = A.empty
		      then
			Et(d)::(aux (pos+1))
		      else
			(
			  (Array.set answer pos carc) ;
			  To(answer)::(aux (pos+1))
			)
		  else
		    []
		)
	      in
		aux 0
	    else
              (
		if c <> Et(0)
		then
		  [ To([||]) ]
		else
		  [ Et(0) ]
	      )
	  )
    | Et(n) -> [full n]

  let complement_version_2 c = match c with
    | To(ara) ->
        let d = dimension c in
        let rec aux pos =
          if (pos < d)
          then (
            let carc = A.complement (Array.get ara pos) in
            if carc = A.empty
            then aux (pos+1)
            else (To(Array.init d (fun k -> if k=pos then carc else A.full)))::(aux (pos+1)))
          else [] in
        let answer = aux 0 in
          if answer = [] then [ Et(d) ] else answer
    | Et(n) -> [full n]

  let complement = complement_version_2

  let complement c = 
    let output = complement c in
    let () = if false then (
      Printf.printf "Torus.complement %s = \n%!" (string_of c);
      List.iter (fun x -> print_endline (" "^(string_of x))) output) in
    output

  let degenerate_coordinates c =
    match (normalize c) with
      | To(ara) ->
	  (
	    let answer = ref Overall.IntSet.empty
	    in
	      Array.iteri
		(fun k a -> (if (A.is_atomic a) then (answer := Overall.IntSet.add k !answer) else ()))
		ara
	      ;
	      !answer
	  )
      | Et(_) -> raise Undefined

  (*--------------------------------------------------------------------------------------*)
  (*                                       compare                                        *)
  (*--------------------------------------------------------------------------------------*)



(*
  31/05/2017 : suppression de la normalisation des arguments dans la fonction 
  compare. Les arguments doivent donc être normalisés.
*)

  let compare c1 c2 =
    match (c1,c2) with 
      | (Et(d),Et(d'))  -> d - d'      
      | (Et(d),To(ara)) -> 
        let d' = Array.length ara in
        if (d > d') then d else -(d' + 1) (* On ajoute 1 pour le cas où l'on est en dimension 0. *)
      | (To(ara),Et(d)) -> 
        let d' = Array.length ara in
        if d > d' then -d else d' + 1 (* On ajoute 1 pour le cas où l'on est en dimension 0. *)
      | (To(ara1),To(ara2)) ->
        let k_max = pred (min (Array.length ara1) (Array.length ara2)) in
        let difference = ref 0 in (
        try
          for k = 0 to k_max do
            difference := A.compare ara1.(k) ara2.(k);
            if !difference <> 0 then raise Exit
          done;
          Array.length ara1 - Array.length ara2
        with Exit -> !difference
        )

  let of_list il =
    let rec aux il' answer =
      match il' with
	| i::il'' ->
	    (
	      if (i <> A.empty)
	      then
		aux il'' (Array.append answer [|i|]) (* la concaténation doit de faire dans cet ordre *)
	      else
		Et(List.length il)
	    )
	| [] ->
	    To(answer)
    in
      aux (List.map A.normalize il) [||]
	
(* La fonction of_array n'a pas été testée *)

  let of_array ar =
    let pos = ref 0
    in
      (
	try
	  while ar.(!pos) <> A.empty
	  do
	    pos := !pos+1
	  done
	  ;
	  Et(Array.length ar)
	with
	  | _ -> To(ar)
      )

  let atom p = To(Array.map (A.atom) p)

  let bounded lf rf lb rb = of_list [A.bounded lf rf lb rb]

  let initial rf rb = of_list [A.initial rf rb]

  let terminal lf lb = of_list [A.terminal lf lb]

  (*--------------------------------------------------------------------------------------*)
  (*                                                                                      *)
  (*--------------------------------------------------------------------------------------*)

  (* safe (and slow) version *)

  let is_not_empty_version_1 c = ((normalize c) <> Et(dimension c))

  let is_empty_version_1 c = ((normalize c) = Et(dimension c))

  (* quick (and unsafe) version *)

  let is_not_empty_version_2 c =
    match c with
      | Et(_) -> false
      | _     -> true

  let is_empty_version_2 c =
    match c with
      | Et(_) -> true
      | _     -> false

  let is_not_empty = is_not_empty_version_2

  let is_empty = is_empty_version_2

  let is_full c = match c with
    | Et(_)   -> false
    | To(ara) -> Array.fold_left (fun accu ar -> (accu && (ar = A.full))) true ara

  let is_not_full c = match c with
    | Et(_)   -> true
    | To(ara) -> Array.fold_left (fun accu ar -> (accu || (ar <> A.full))) false ara

  (* Requires OCaml 4.03 of higher *)

  let is_included c1 c2 = 
    let _ = test_dimension c1 c2 "(is included)" in
      match c1,c2 with 
        | To(ara1),To(ara2) -> (
          try 
            Array.iter2 (
              fun it1 it2 -> if not (A.is_included it1 it2) then raise Exit) 
              ara1 
              ara2;
            true
          with Exit -> false)
        | Et(_),_ -> true
        | _ -> false

  let is_included c1 c2 = 
    let output = is_included c1 c2 in
    let () = if false then Printf.printf "Torus.is_included %s %s = %b\n%!"
      (string_of c1)
      (string_of c2)
      output in
    output

  let interior c =
    match c with
      | To(ara) -> normalize (To(Array.map A.interior ara))
      | Et(n) -> Et(n)

  let closure c =
    match c with
      | To(ara) -> To(Array.map A.closure ara)
      | Et(n)   -> Et(n)

  let product c1 c2 =
    match (c1,c2) with
      | (To(ara1),To(ara2)) -> To(Array.append ara1 ara2)
      | (Et(n1)  ,To(ara2)) -> Et(n1 + (Array.length ara2))
      | (To(ara1),Et(n2)  ) -> Et((Array.length ara1) + n2)
      | (Et(n1)  ,Et(n2)  ) -> Et(n1 + n2)

  let intersection c1 c2 =
    match c1,c2 with
      | Et n,_
      | _,Et n -> [Et n]
      | To ara1,To ara2 -> 
        try
          let a = 
            Array.map2 (fun a1 a2 -> 
              try
                let it = A.intersection a1 a2 in
                if A.is_not_empty it then [|it|]
                else raise Exit
              with A.Split(a3,a4) -> [|a3;a4|]) 
            ara1 ara2 in
          let b = Array.map (fun x -> pred (Array.length x)) a in
          let current = Array.make (Array.length a) 0 in
          let answer = ref [] in
          try
            while true do
(*
              print_string "current = [| ";
              Array.iter (fun i -> Printf.printf "%d " i) current;
              print_endline "|]";
*)
              answer := To(Array.map2 (fun i a -> a.(i)) current a) :: !answer;
(*
              print_string "answer = [ ";
              List.iter (fun c -> Printf.printf "%s " (string_of c)) !answer;
              print_endline "]"
              ;
*)
              Overall.Array.cartesian_next_array_consumer2 current b 
            done;
            [] (*dead code*)
          with Common.Enum.End -> !answer
        with Exit -> [Et (dimension c1)]


  let intersection c1 c2 = 
    let output = intersection c1 c2 in
    let () = if false then (Printf.printf "Torus.intersection %s %s =\n%!" (string_of c1) (string_of c1);
      print_string "[ ";
      List.iter (fun c -> Printf.printf "%s " (string_of c)) output;
      print_endline "]") in
    output


  (*

     Attention : pour que la fonction ci-dessous soit valide, il faut
     que la fonction intersection, dont elle dépend, vérifie la
     propriété suivante : lorsqu'une intersection est vide, la valeur
     renvoyée doit être la liste dont l'unique élément est l'ensemble
     vide de dimension d, où d est la dimension commune des cubes dont
     on cherche à calculer l'intersection.

  *)

  let in_touch c1 c2 =
    let emp = empty (dimension c1)
    in
      (
	((intersection (closure c1) c2) <> [emp]) || ((intersection c1 (closure c2)) <> [emp])
      )

  let glb c =
    let d = dimension c
    in
      (
	match c with
	  | To(ara) ->
	      (
		let p = Array.make d A.least_regular_value
		in
		  (
		    try
		      for k = 0 to (d - 1)
		      do
			p.(k) <- A.glb ara.(k)
		      done
		    with
		      | A.Undefined -> raise Undefined
		  )
		  ;
		  p
	      )
	  | _ -> raise Undefined
      )

  let lub c =
    let d = dimension c
    in
      (
	match c with
	  | To(ara) ->
	      (
		let p = Array.make d A.least_regular_value
		in
		  try
		    (
		      for k = 0 to (d - 1)
		      do
			p.(k) <- A.lub ara.(k)
		      done
		      ;
		      p
		    )
		  with
		    | A.Undefined -> raise Undefined (* Array.make d I.min_regular_value *)
	      )
	  | _ -> raise Undefined (* Le cercle dirigé n'a ni plus petit ni plus grand élément *)
      )


  let lub c =
    match c with
      | To(ara) ->
	  (
	    try
	      Array.init (dimension c) (fun k -> (A.lub ara.(k)))
	    with
	      | A.Undefined -> raise Undefined (* Array.make d I.min_regular_value *)
	  )
      | _ -> raise Undefined (* Le cercle dirigé n'a ni plus petit ni plus grand élément *)


  let glb c =
    match c with
      | To(ara) ->
	  (
	    try
	      Array.init (dimension c) (fun k -> (A.glb ara.(k)))
	    with
	      | A.Undefined -> raise Undefined (* Array.make d I.min_regular_value *)
	  )
      | _ -> raise Undefined (* Le cercle dirigé n'a ni plus petit ni plus grand élément *)


  let belongs_to p c =
    match c with
      | To(ara) -> (
            let n = Array.length ara in
            let k = ref 0 in
            while !k < n  && (A.belongs_to p.(!k) ara.(!k)) do
              incr k
            done;
            !k = n)
      | Et(_) -> false


  let belongs_to_boundary_version_1 p c =
    ((belongs_to p (closure c)) && (not(belongs_to p (interior c))))

  let belongs_to_boundary p c =
    let () = assert (Array.length p = dimension c) in
    let not_in_interior = ref false in
      match c with 
        | To(ara) -> (
          if p <> [||] then (
            let k = ref 0 in
            let current_point = ref p.(0) in
            let current_closure = ref (A.closure ara.(0)) in
              while (!k < Array.length p && A.belongs_to !current_point !current_closure) do
                not_in_interior := !not_in_interior || not (A.belongs_to !current_point (A.interior !current_closure));
                incr k;
                current_point := p.(!k) ; 
                current_closure := (A.closure ara.(!k))
              done;
              !not_in_interior)
          else false)
        | Et(_) -> false

  let in_the_future_of c1 c2 =
    match c1,c2 with
      | To ara1,To ara2 -> (
          let dim = Array.length ara1 in
          try To (
            Array.init dim (fun i -> 
              let answer = A.in_the_future_of ara1.(i) ara2.(i) in
              if A.is_not_empty answer 
              then answer 
              else raise Exit))
          with Exit -> Et dim)
      | _ -> c1

  
  let in_the_future_of c1 c2 =
    let output = in_the_future_of c1 c2 in
    let () = if false then Printf.printf "Torus.in_the_future_of %s %s = %s\n%!" 
      (string_of c1) 
      (string_of c2)
      (string_of output);
      flush stdout in
    output


  let in_the_past_of c1 c2 =
    match c1,c2 with
      | To ara1,To ara2 -> (
          let dim = Array.length ara1 in
          try To (
            Array.init dim (fun i -> 
              let answer = A.in_the_past_of ara1.(i) ara2.(i) in
              if A.is_not_empty answer 
              then answer 
              else raise Exit))
          with Exit -> Et dim)
      | _ -> c1

  
  let in_the_past_of c1 c2 =
    let output = in_the_past_of c1 c2 in
    let () = if false then Printf.printf "Torus.in_the_past_of %s %s = %s\n%!" 
      (string_of c1) 
      (string_of c2)
      (string_of output);
      flush stdout in
    output

  let projection soi c =
    match c with
      | To(ara) -> (
	      (* Array.iteri (fun k i -> if (Overall.IntSet.mem k soi) then (answer.(k) <- i) else ()) ari *)
	      (normalize (To(Overall.IntSet.fold (fun k accu -> (Array.append accu [|ara.(k)|])) soi [||]))))
      | Et(n) -> Et(Overall.IntSet.cardinal soi)

  let cylinder d soi c =
    match c with
      | To(ara) -> (
	    let answer = Array.make d A.full in
          Array.iteri (fun k i -> if (Overall.IntSet.mem k soi) then (answer.(k) <- i) else ()) ara;
          (normalize (To(answer))))
      | Et(n) -> Et(d)

  let base_coordinates c =
    (* let soi = *)
    match (normalize c) with
      | To(ara) ->
	  (
	    let answer = ref Overall.IntSet.empty
	    in
	      Array.iteri
		(fun k i -> (if (i <> A.full) then (answer := Overall.IntSet.add k !answer) else ()))
		ara
	      ;
	      !answer
	  )
      | Et(_) -> raise Undefined

  let base_dimension c =
    Overall.IntSet.cardinal (base_coordinates c)

end

(*

  module Make(A:(*Connected_subsets_of_the.Circle*)Sig.Arc) = (
  (Raw(A)) : ( Sig.Tore with type regular_value = A.regular_value
  and type point = A.regular_value array and type arc = A.t ) )

*)
