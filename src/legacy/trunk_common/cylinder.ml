(* Cylinder.ml *)

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
 type regular_value
  type point
  type generator
  type underlying
  type ints
  type t 
  exception Undefined
  val sub: int -> int -> underlying -> underlying
  val add: underlying -> underlying -> underlying
  val zero: underlying
  val compare_points: point -> point -> int
  val compare: t -> t -> int
  val compare_generator: generator -> generator -> int
  val string_of_generator: generator -> string
  val dimension: t -> int 
  val atom: underlying -> point -> t
  val of_list: (generator list) -> t
  val of_array: (generator array) -> t
  val to_list:  t -> (generator list)
  val to_array: t -> (generator array)
  val normalize:t -> t
  val complement: t -> (t list)
  val product: t -> t -> t
  val intersection: t -> t -> (t list)
  val full: underlying -> t
  val empty: underlying -> t
  val belongs_to: point -> t -> bool
  val interior: t -> t
  val closure: t -> t
  val is_empty: t -> bool
  val is_not_empty: t -> bool
  val is_full: t -> bool
  val is_not_full: t -> bool
  val is_included: t -> t -> bool
  val in_the_future_of: t -> t -> t
  val in_the_past_of: t -> t -> t
  val string_of: t -> string
  val in_touch: t -> t -> bool
  val lub: t -> point
  val glb: t -> point
  val underlying: t -> underlying
end

module Make(Overall:In)(M:Mix.S):
(S with type regular_value = M.regular_value
  and type point = M.regular_value array
  and type underlying = (M.underlying) array 
  and type generator = M.t
  and type ints = Overall.IntSet.t)
=
struct

  exception Unbounded
  exception Undefined
  exception Empty

  type ints = Overall.IntSet.t

  type underlying = M.underlying array

  let from_underlying_array ua = ua

  let sub s l u = Array.sub u s l

  let zero = [||]

  let add u1 u2 = Array.append u1 u2

  type regular_value = M.regular_value

  type point = regular_value array

  let compare_points p1 p2 =
    let n = Array.length p1 in
    let () = assert (n = Array.length p2) in
    let delta = ref 0 in
    let k = ref 0 in
    while !delta = 0 && !k < n do
      delta := M.compare_regular_values p1.(!k) p2.(!k);
      incr k
    done;
    !delta

  type generator = M.t

  let compare_generator = M.compare

  let string_of_generator g = M.string_of g

  (* Cy([||]) représente la structure pleine de dimension nulle mais a
     priori et sa structure sous-jacente est [||] *)

  type t =
    | Cy of (generator array)
    | Ey of underlying

  let equal g1 g2 = (g1=g2)

  let underlying m = match m with
    | Cy(arx) -> Array.map (fun g -> M.underlying g) arx
    | Ey(und) -> und

  let dimension m = Array.length (underlying m)

  let normalize g =
    match g with
      | Cy(ara) -> (
        let n = Array.length ara in
        try 
          for i = 0 to n - 1 do
            let current = M.normalize (ara.(i)) in
            if M.is_empty current then raise Exit;
            ara.(i) <- current
          done;
          g
        with Exit -> Ey (underlying g))
      | _ -> g

  exception Exit_compare of int

(*
  31/05/2017 : suppression de la normalisation des arguments dans la fonction 
  compare. Les arguments doivent donc être normalisés.
*)

  let compare c1 c2 =
    let d1 = dimension c1 in
    let d2 = dimension c2 in
    match c1,c2 with 
      | Ey _,Ey _ -> d1 - d2
      | Ey _,Cy arx -> 
        if (d1 > d2) then d1 else -(d2 + 1) (* On ajoute 1 pour le cas où l'on est en dimension 0. *)
      | Cy arx, Ey _ ->
        if d1 > d2 then -d1 else d2 + 1 (* On ajoute 1 pour le cas où l'on est en dimension 0. *)
      | Cb arx1,Cb arx2  ->
        let k_max = pred (min d1 d2) in
        let difference = ref 0 in (
        try
          for k = 0 to k_max do
            difference := I.compare ara1.(k) ara2.(k);
            if !difference <> 0 then raise Exit
          done;
          Array.length ara1 - Array.length ara2
        with Exit -> !difference
        )



  let of_list gl =
    let rec aux gl' answer =
      match gl' with
	| g::gl'' ->
	    (
	      if (M.is_not_empty g)
	      then
		aux gl'' (Array.append answer [|g|]) (* la concaténation doit de faire dans cet ordre *)
	      else
		Ey(Array.of_list (List.map (fun g -> M.underlying g) gl))
	    )
	| [] ->
	    Cy(answer)
    in
      aux (List.map M.normalize gl) [||]

  let of_array ar =
    let pos = ref 0
    in
      (
	try
	  while M.is_not_empty ar.(!pos)
	  do
	    pos := !pos+1
	  done
	  ;
          Ey(Array.map (fun g -> M.underlying g) ar)
	with
	  | _ -> Cy(ar)
      )

  let to_array x = match x with 
    | Cy(arx) -> arx
    | Ey(_)   -> raise Undefined
	
  let to_list x = 
    match x with 
      | Cy(arx) -> Array.fold_right (fun x accu -> x::accu) arx []
      | Ey(_)   -> raise Undefined


  let product m1 m2 =
    match (m1,m2) with
      | (Cy(arx1),Cy(arx2)) -> normalize (Cy(Array.append arx1 arx2))
      | (Ey(und1),Cy(arx2)) -> Ey(Array.append und1 (Array.map (fun g -> M.underlying g) arx2))
      | (Cy(arx1),Ey(und2)) -> Ey(Array.append (Array.map (fun g -> M.underlying g) arx1) und2)
      | (Ey(und1),Ey(und2)) -> Ey(Array.append und1 und2)


  let intersection c1 c2 =
    match (c1,c2) with
      | Ey g,_
      | _,Ey g -> [Ey g]
      | Cy ara1,Cy ara2 -> 
        try
          let a = 
            Array.map2 (fun a1 a2 -> 
              try
                let it = M.intersection a1 a2 in
                if M.is_not_empty it then [|it|]
                else raise Exit
              with M.Split(a3,a4) -> [|a3;a4|]) 
            ara1 ara2 in
          let b = Array.map Array.length a in
          let current = Array.make (Array.length a) 0 in
          let answer = ref [] in
          try
            while true do
              answer := Cy(Array.map2 (fun i a -> a.(i)) current a) :: !answer;
              Overall.Array.cartesian_next_array_consumer2 current b 
            done;
            [] (*dead code*)
          with Common.Enum.End -> !answer
        with Exit -> [Ey (underlying c1)]

  let full und = Cy(Array.map (fun u -> (M.full u)) und)
  and empty und = Ey(und) (* Bugfix : on avait full=empty ! *)

  let is_empty c = match c with
    | Ey(_) -> true
    | _     -> false

  let is_not_empty c = match c with
    | Ey(_) -> false
    | _     -> true

  let is_full c = match c with
    | Ey(_) -> false
    | Cy(arx) -> Array.fold_left (fun accu m -> (accu && (m = (M.full (M.underlying m))))) true arx


  let is_not_full c = match c with
    | Ey(_) -> true
    | Cy(arx) -> Array.fold_left (fun accu m -> (accu || (m <> (M.full (M.underlying m))))) false arx


  (* Les fonctions after et before peuvent lever une exception dans le
     cas où l'indice spécifié est celui d'un arc. *)

  let after k c =
    match (normalize c) with
      | Cy(arx) ->
	  (
	    if arx <> [||]
	    then
	      let arx' = Array.map M.full (underlying c)
	      in
		((arx'.(k) <- M.after arx.(k)) ; (normalize (Cy(arx'))))
	    else
	      Ey([||])
	  )
      | Ey(und) -> (full und)

  let before k c =
    match (normalize c) with
      | Cy(arx) ->
	  (
	    if arx <> [||]
	    then
	      let arx' = Array.map M.full (underlying c)
	      in
		((arx'.(k) <- M.before arx.(k)) ; (normalize (Cy(arx'))))
	    else
	      Ey([||])
	  )
      | Ey(und) -> (full und)

  let slice i m = 
    let n = dimension m in
    match m with
      | Cy(arx) -> (
        if i < n then arx.(i)
        else raise Undefined)
      | Ey(und) -> (
        if i < n then M.empty und.(i)
        else raise Undefined)

  let complement_version_1 m =
    let answer = ref []
    in
      (
	for i=0 to (dimension m)-1
	do
	  (
	    try
	      answer := (before i m)::(after i m)::!answer
	    with
	      | M.Undefined ->
		  let aux = Array.map M.full (underlying m)
		  in
		    (
		      aux.(i) <- M.complement (slice i m)
		      ;
		      answer := (Cy(aux)::!answer)
		    )
	  )
	done
	;
	!answer
      )

  let complement_version_2 m =
    let dim = underlying m
    and answer = ref []
    in
      (
	for i=0 to (Array.length dim)-1
	do
	  (
	    try
	      answer :=
		(
		  let aux = M.complement (slice i m)
		  in
		    if aux <> M.empty (M.underlying aux)
		    then
		      (Cy(Array.mapi (fun j u -> if i<>j then (M.full u) else aux) dim)::!answer)
		    else
		      !answer
		)
	    with
	      | M.Split(x1,x2) -> let lim = [x1;x2] in 
		  List.iter
		    (fun g -> 
          answer := (Cy(Array.mapi 
            (fun j u -> 
              if i<>j then (M.full u) 
              else g) dim)::!answer))
		    lim
	  )
	done
	;
	!answer
      )

  let complement = complement_version_2

  let belongs_to p c =
    match c with
      | Cy arx -> (
          let n = Array.length arx in
          let k = ref 0 in
          while !k < n  && (M.belongs_to p.(!k) arx.(!k)) do
            incr k
          done;
          !k = n)
      | Ey _ -> false



  let interior_version_1 m =
    match m with
      | Cy(arx) -> normalize (Cy(Array.map M.interior arx))
      | Ey(und) -> Ey(und)

  let interior_version_2 m =
    match m with
      | Cy(arx) ->
	  (
	    try
	      Cy
		(
		  Array.map
		    (
		      fun g ->
			let ig = M.interior g
			in
			  if ig <> M.empty (M.underlying g)
			  then
			    ig
			  else
			    raise Empty
		    ) arx
		)
	    with
	      | Empty -> Ey(underlying m)
	  )
      | Ey(und) -> Ey(und)

  let interior = interior_version_2

  let closure m =
    match m with
      | Cy(arx) -> Cy(Array.map M.interior arx)
      | Ey(und) -> Ey(und)

  let in_touch m1 m2 =
    let emp = empty (underlying m1)
    in
      (((intersection (closure m1) m2) <> [emp]) || ((intersection m1 (closure m2)) <> [emp]))

  let atom u p =
    let dim = Array.length p
    in
      if dim = Array.length u
      then 
	Cy(Array.mapi (fun i x -> M.atom u.(i) x) p)
      else
	raise Undefined

  let in_the_future_of m1 m2 =
    match m1 with
      | Cy(arx1) ->
	  (
	    match m2 with
	      | Cy(arx2) ->
		  (
		    let dim = underlying m1 (* Array.length arx1 *) (* i.e. dimension m1 *)
		    in
		    let answer = Array.map (fun u -> (M.empty u)) dim
		    in
		      (
			try
			  let i = ref 0
			  in
			  let future = ref (M.in_the_future_of arx1.(0) arx2.(0))
			  in
			    (
			      while !future <> (M.empty (M.underlying !future))
			      do
				answer.(!i) <- !future ;
				i := !i + 1 ;
				future := M.in_the_future_of arx1.(!i) arx2.(!i)
			      done
			      ;
			      Ey(dim)
			    )
			with
			  | _ -> Cy(answer)
		      )
		  )
	      | Ey(n) -> Ey(n)
	  )
      | Ey(n) -> Ey(n)

  let in_the_past_of m1 m2 =
    match m1 with
      | Cy(arx1) ->
	  (
	    match m2 with
	      | Cy(arx2) ->
		  (
		    let dim = underlying m1 (* Array.length arx1 *) 
		    in 
		    let answer = Array.map (fun u -> (M.empty u)) dim (* Array.make dim M.empty *)
		    in
		      (
			try
			  let i = ref 0
			  in
			  let past = ref (M.in_the_past_of arx1.(0) arx2.(0))
			  in
			    (
			      while !past <> (M.empty (M.underlying !past))
			      do
				answer.(!i) <- !past ;
				i := !i + 1 ;
				past := M.in_the_past_of arx1.(!i) arx2.(!i)
			      done
			      ;
			      Ey(dim)
			    )
			with
			  | _ -> Cy(answer)
		      )
		  )
	      | Ey(n) -> Ey(n)
	  )
      | Ey(n) -> Ey(n)

  let step_forward = in_the_future_of
  and step_backward = in_the_past_of

  let glb m =
    let intd = dimension m
    in
      (
	match m with
	  | Cy(arx) ->
	      (
		let p = Array.make intd M.least_regular_value
		in
		  (
		    try
		      for k = 0 to (intd - 1)
		      do
			p.(k) <- M.glb arx.(k)
		      done
		    with
		      | M.Undefined -> raise Undefined
		      | M.Unbounded -> raise Undefined
		  )
		  ;
		  p
	      )
	  | _ -> raise Undefined
      )

  let lub m =
    let intd = dimension m
    in
      (
	match m with
	  | Cy(arx) ->
	      (
		let p = Array.make intd M.least_regular_value
		in
		  try
		    (
		      for k = 0 to (intd - 1)
		      do
			p.(k) <- M.lub arx.(k)
		      done
		      ;
		      p
		    )
		  with
		    | M.Undefined -> raise Undefined
		    | M.Unbounded -> raise Undefined (* Array.make d I.min_regular_value *)
	      )
	  | _ -> raise Undefined (* Le cercle dirigé n'a ni plus petit ni plus grand élément *)
      )

  let test_dimension c1 c2 str =
    let	d1 = dimension c1 in
    let	d2 = dimension c2 in
      (* failwith ("The arguments of " ^ str ^ " are heterogeneous.") *)
      assert (d1 = d2);
      d1

  let is_included c1 c2 = 
    let _ = test_dimension c1 c2 "(is included)" in
      match c1,c2 with 
        | Cy(arx1),Cy(arx2) -> (
          try 
            Array.iter2 (fun it1 it2 -> if not (M.is_included it1 it2) then raise Exit) arx1 arx2;
            true
          with Exit -> false)
        | Ey _,_ -> true
        | _ -> false

 let string_of_und u =
   let lu = (Array.length u)
   in
   let answer = Bytes.create lu
   in
     for i=0 to (lu-1)
     do
      Bytes.set answer i (if u.(i) = M.circle then '0' else '1')
     done
     ;
     answer

  let string_of c =
    match c with 
      | Cy(ai) -> (
        if ai <> [||] then
          let first = M.string_of ai.(0) in 
          Array.fold_left (fun s i -> s^"*"^(M.string_of i)) first (Array.sub ai 1 ((Array.length ai)-1))
        else "Zero dimension non empty cylinder")
      | Ey(und)  -> "@("^(string_of_und und)^")"

end
