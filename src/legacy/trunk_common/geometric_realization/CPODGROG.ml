(* Cartesian Product of Directed Geometric Realization of Graphs *)
(* CPODGROG.ml *)
(* CEA, LIST *)
(* Part of the ALCOOL project *)

module Make(*Cartesian Products of Directed Geometric Realisations of Graphs*)
  (
    G:(*Directed Geometric Realisations of Graphs*)
    sig
      type point
      type t
      (*display*)
      val string_of: ?endline:bool -> ?shift:int -> ?hollow:bool -> ?verbose:bool -> ?pretty:bool -> t -> string
      (*constructors*)
      val of_string: t -> string
      (*tests*)
      val point_mem: point -> t -> bool
      val is_empty: t -> bool
      val is_not_empty: t -> bool
      val is_full: t -> bool
      val is_not_full: t -> bool
      val is_included: t -> t -> bool
      (*boolean structure*)
      val empty: t -> t
      val full: t -> t
      val complement: t -> t
      val intersection: t -> t -> t
      val union: t -> t -> t
      val difference: t -> t -> t
      (*topological structure*)
      val interior: t -> t
      val closure: t -> t
      (*directed structure*)
      val future_extension: t -> t -> t
      val past_extension: t -> t -> t
      val future_closure: t -> t
      val past_closure: t -> t
      (*administrativia*)
      val compare: t -> t -> int
    end
  )
  =
struct

  type point = G.point array

  type brick =
    | Cp of G.t array (*cartesian product*)
    | Em of G.t array (*empty*)

  (*The compare function makes use of G.compare*)

  let compare p1 p2 =
    match p1,p2 with
      | Cp arg1,Cp arg2 -> (
          let delta = ref 0 in
          try
            let f k g1 =
              let d = G.compare g1 arg2.(k) in
              if d <> 0 then (delta:=d ; raise Exit) in
            Array.iteri f arg1 ; 0
          with Exit -> !delta)
      | Cp _,Em _ ->  1
      | Em _,Cp _ -> -1
      | _ -> 0

  module Cover = Set.Make
    (struct
      type t = brick
      let compare = compare
     end)

  type t = Cover.t

  module Brick =

  struct

    (*the underlying graph in each component has to be kept so we have a
      sound notion of complement*)

    (*display*)

    let string_of ?(endline=true) ?(shift=2) ?(hollow=false) ?(verbose=false) ?(pretty=true) p =
      let display ?(hollow=false) arg =
      let f accu g =
        Printf.sprintf "%s%s"
          (if accu <> "" then Printf.sprintf "%s\n%s\n" accu ((String.make (shift-1) ' ')^Message.cartesian_product) else "")
          (G.string_of ~endline ~shift ~hollow ~verbose ~pretty g) in
				Array.fold_left f "" arg in
      match p with
				| Em arg ->
            if hollow
            then display arg
            else
              if verbose
              then display arg
              else ""
				| Cp arg -> display arg

    let stringlist_of ?verbose ?pretty p = match p with
      | Em  _ -> []
      | Cp arg -> Array.fold_left (fun accu g -> (G.string_of ?verbose ?pretty g)::accu) [] arg

    let stringarray_of ?verbose ?pretty p = match p with
      | Em  _ -> [||]
      | Cp arg -> Array.fold_left (fun accu g -> Array.append [|G.string_of ?verbose ?pretty g|] accu) [||] arg

    (*administrativia*)

    let dimension c = match c with
      | Cp arg
      | Em arg -> Array.length arg

    let argument c = match c with
      | Cp arg
      | Em arg -> arg

    (*tests*)

    let test_dimension p1 p2 str =
      let d1 = dimension p1 in
      let d2 = dimension p2 in
      if d1 = d2
      then d1
      else failwith (Printf.sprintf "The arguments of %s are heterogeneous" str)

    let make_point p = p

    let point_dim p = Array.length p

    let point_mem p c = match c with
      | Cp arg -> (try
              (Array.iteri (fun i g ->  if not (G.point_mem p.(i) g) then raise Exit else ()) arg) ;
              true
          with Exit -> false)
      | Em _ -> false

    let is_empty p = match p with
      | Cp _ -> false
      | Em _ -> true

    let is_not_empty p = match p with
      | Cp _ -> true
      | Em _ -> false

    let is_included p1 p2 =
      match p1,p2 with
				| Cp arg1,Cp arg2 ->
          let d = Array.length arg1 in
				  let pos = ref 0 in (
          try
            while !pos < d do
              if not (G.is_included arg1.(!pos) arg2.(!pos)) then raise Exit ;
              incr pos done ;
            true
          with Exit -> false)
				| Em _, _ -> true
				| _ -> false

    let is_not_included p1 p2 =
      match p1,p2 with
				| Cp arg1,Cp arg2 ->
          let d = Array.length arg1 in
				  let pos = ref 0 in (
          try
            while !pos < d do
              if not (G.is_included arg1.(!pos) arg2.(!pos)) then raise Exit ;
              incr pos done ;
            false
          with Exit -> true)
				| Em _, _ -> false
				| _ -> true

    (*is_included_in_area needs its argument to be a normal form*)

    let is_included_in_area p a = Cover.exists (fun p2 -> is_included p p2) a

    (*constructors*)

		let one = Cp [||]

		let zero = Em [||]

    (*fc, lc, and cs respectively stand for "first coordinate", "last
      coordinate", and "coordinate set"*)

    let underlying ?fc ?lc ?ca ?(mp=(fun x -> x)) p =
      match p with
				| Em arg
				| Cp arg ->
				  match fc,lc,ca with
				    | _,_,Some inds -> let f i = arg.(inds.(i)) in (Array.init (Array.length inds) f)
				    | a,b,_ ->
				      let a = match a with | None -> 0                | Some a -> a   in
				      let b = match b with | None -> Array.length arg | Some b -> b + 1 in
				      Array.init (max 0 (b - a)) (fun i -> mp arg.(a + i))

    let empty ?fc ?lc ?ca p =
      match p with
				| Em arg
				| Cp arg -> Em (
				    match fc,lc,ca with
				      | _,_,Some inds -> let f i = G.empty (arg.(inds.(i))) in Array.init (Array.length inds) f
				      | a,b,_ ->
								let a = match a with | None -> 0                | Some a -> a   in
								let b = match b with | None -> Array.length arg | Some b -> b + 1 in
								Array.init (max 0 (b - a)) (fun i -> G.empty (arg.(a + i))))

    let full ?fc ?lc ?ca p =
      match p with
				| Em arg
				| Cp arg -> Cp (
				    match fc,lc,ca with
				      | _,_,Some inds -> let f i = G.full (arg.(inds.(i))) in Array.init (Array.length inds) f
				      | a,b,_ ->
								let a = match a with | None -> 0                | Some a -> a   in
								let b = match b with | None -> Array.length arg | Some b -> b + 1 in
								Array.init (max 0 (b - a)) (fun i -> G.full (arg.(a + i))))

    let of_list gl =
      let empty = ref false in
      let rec of_list gl' answer =
        match gl' with
          | g::gl'' ->
            if G.is_empty g
            then (empty:=true ; of_list gl'' (Array.append answer [|g|]))
            else of_list gl'' (Array.append answer [|g|])
          (* la concaténation doit se faire dans cet ordre *)
          | [] -> answer in
      let answer = of_list gl [||] in
      if !empty
      then Em answer
      else Cp answer

    (*of_array has not been tested yet – and was modified on thursday the 29th of august 2013*)

	let of_array arg =
      let d = Array.length arg in
      let pos = ref 0 in
      try
				let () = while !pos < d do
          if G.is_empty arg.(!pos)
          then raise Exit ;
          incr pos done in
				Cp arg
      with Exit -> Em arg

	let to_array p = match p with
		| Cp arg -> arg
		| Em _   -> raise (Invalid_argument "to_array [Cpodgrog]")

	let to_list p = match p with
		| Cp arg -> Array.to_list arg
		| Em _   -> raise (Invalid_argument "to_list [Cpodgrog]")

    (*boolean structure*)

  let complement p =
    let d = dimension p in
    match p with
      | Cp [||] -> Cover.singleton (Em [||])
      | Cp arg -> (
          let answer = ref Cover.empty in
          let f k g =
            let aux = G.complement g in
              if G.is_not_empty aux
              then answer :=
                Cover.add
                (Cp(Array.init d (fun x -> if x=k then aux else G.full arg.(x))))
                !answer in
          Array.iteri f arg ;
          if Cover.is_empty !answer
          then Cover.singleton (Em arg)
          else !answer)
      | Em arg -> Cover.singleton (Cp (Array.map G.full arg))

  let intersection p1 p2 =
    let d = test_dimension p1 p2 "(intersection)" in
    match p1,p2 with
    | Cp arg1,Cp arg2  ->
      (
        try Cp
        (
          Array.init d
            (
              fun k ->
                let aux = G.intersection arg1.(k) arg2.(k) in
                if G.is_not_empty aux
                then aux
                else raise Exit
            )
        )
        with
          | Exit -> Em arg1
      )
    | Em arg,_ -> Em arg
    | _,Em arg -> Em arg

  let difference p1 p2 =
    let d = dimension p1 in
    match p1,p2 with
    | Cp [||],_ -> complement p2
    | Cp arg1,Cp arg2 ->
      (
        try
          let answer = ref Cover.empty in
          Array.iteri
          (
            fun k g1 ->
              let aux = G.difference g1 arg2.(k) in
              if G.is_not_empty aux
              then answer :=
                Cover.add
                (Cp(Array.init d (fun x -> if x=k then aux else arg1.(x))))
                !answer
              else raise Exit
          )
          arg1
          ;
          !answer
        with
          | Exit -> Cover.singleton (Em arg1)
      )
    | Em _,_
    | _,Em _ -> Cover.singleton p1

    (*topological structure*)

    let closure p = match p with
			| Cp arg -> Cp(Array.map G.closure arg)
			| Em _ -> p

    let interior p =
      match p with
	| Cp arg  ->
	  (
	    try
	      Cp
		(
		  Array.map
		    (
		      fun g ->
			let aux = G.interior g in
			if G.is_not_empty aux
			then aux
			else raise Exit
		    ) arg
		)
	    with
	      | Exit -> Em arg
	  )
	| Em _ -> p

    (*directed structure*)

    (*To be tested. Actually I think there is a bug. Suppose p1 is ]0,1[x]0,1[x{0} and p2 is {0}x{0}x[0,1]. Then intersection
    (closure p1) p2 is the origin, i.e. {0}x{0}x{0} and thus the future extension is p2 while it should be empty – see
    future_extension_1.cpr*)

    (*In general, given an area x, future x (closure x) will be (closure x) ... it should not be so e.g. x = ]0,1[*)

    (*buggy*)

    let extension_pattern extension closure1 closure2 p1 p2 =

      let d = dimension p1 in

      let extension_closure closure_op p = match p with
        | Cp arg -> (
              try Cp (
                Array.init d
                  (fun k ->
                    let aux = closure_op arg.(k) in
                    if G.is_not_empty aux
                    then aux
                    else raise Exit))
              with Exit -> Em arg)
        | Em _ -> p in

      let aux p =
        match p1 , p2 with
          | Cp [||] , _ -> p2
          | Cp arg1,Cp arg2 -> (
              try Cp (
                Array.init d
                  (fun k ->
                    let aux = extension arg1.(k) arg2.(k) in
                    if G.is_not_empty aux
                    then aux
                    else raise Exit))
              with Exit -> Em arg1)
          | Em arg , _
          | _ , Em arg -> Em arg in

      [aux (intersection (extension_closure closure1 p1) p2) ; aux (intersection p1 (extension_closure closure2 p2))]

    let future_extension p1 p2 = extension_pattern G.future_extension G.future_closure G.past_closure p1 p2

    let past_extension p1 p2 = extension_pattern G.past_extension G.past_closure G.future_closure p1 p2

    (*graded structure*)

    let product p1 p2 = match p1,p2 with (*bugfix 21/12/2011 – rewrite 06.01.2012*)
      | Cp arg1,Cp arg2 -> Cp (Array.append arg1 arg2)
      | _ -> Em (Array.append (argument p1) (argument p2))

    let list_fold_product p_list =
      let is_empty = ref false in
      let f p = match p with
			  | Cp arg -> arg
			  | Em arg -> is_empty := true; arg in
      let answer = Array.concat (List.map f p_list) in
      if !is_empty
      then Em answer
      else Cp answer

    let array_fold_product p_array =
      let is_empty = ref false in
      let f p = match p with
			  | Cp arg -> arg
			  | Em arg -> is_empty := true; arg in
      let answer = Array.fold_left Array.append [||] (Array.map f p_array) in
      if !is_empty
      then Em answer
      else Cp answer

  end(*Brick*)

  module Area =
  struct

    type t = Cover.t

    let make_point = Brick.make_point

    let point_dim p = Brick.point_dim

    let point_mem p a = Cover.exists (fun c -> Brick.point_mem p c)

    (*comparison*)

		(*Cover.empty is the dummy value in the sense that it does not represent any directed space. Yet it is useful to
		initialize inductions or references*)

		let dummy = Cover.empty

    let compare = Cover.compare

		(*Convention: any covering containing the empty brick does not contain any other brick, and in this case it
		represents the empty area.*)

    let dimension a = Brick.dimension (Cover.choose a)

    (*tests*)

		let is_dummy = Cover.is_empty

		let is_not_dummy a = not (is_dummy a)

    let is_empty a = Brick.is_empty (Cover.choose a)

    let is_not_empty a = Brick.is_not_empty (Cover.choose a)

    (*is_included needs its argument to be a normal form*)

    let is_included a1 a2 = Cover.for_all (fun b1 -> Brick.is_included_in_area b1 a2) a1

    (*display*)

    let string_of ?hollow ?verbose ?pretty a =
	    let f p accu =
				Printf.sprintf "%s%s"
					(if accu<>"" then Printf.sprintf "%s%s" accu (Message.green ~active:true ~bold:true "\n ⋃\n") else "")
					(Brick.string_of ~endline:false ~shift:4 ?verbose ?pretty p) in
			let answer = Cover.fold f a "" in
			if answer <> ""
			then answer
			else (Brick.string_of ~shift:4 ?hollow ?verbose ?pretty (Cover.choose a))
    (*display bug empty case*)

    let stringlist_of ?verbose ?pretty a =
	    let f p accu = (Brick.string_of ?verbose ?pretty p)::accu
	    in Cover.fold f a []

    let stringarray_of ?verbose ?pretty a =
	    let f p accu = Array.append [|Brick.string_of ?verbose ?pretty p|] accu
	    in Cover.fold f a [||]

    (*constructors*)

    (*TODO*)

    let underlying ?fc ?lc ?ca ?mp a = Brick.underlying ?fc ?lc ?ca ?mp (Cover.choose a)

    (*fc, lc, and cs respectively stand for "first coordinate", "last
      coordinate", and "coordinate set"*)

		let empty ?fc ?lc ?ca a = (Brick.empty ?fc ?lc ?ca (Cover.choose a))

    let full ?fc ?lc ?ca a  = (Brick.full ?fc ?lc ?ca (Cover.choose a))

    let of_brick b = Cover.singleton b

		let one = of_brick Brick.one

		let zero = of_brick Brick.zero

    (*boolean structure*)

    (* Remove any brick form the covering which is already contained in
       another brick of this covering *)

    let clean_sweep a =
	    let f b b' = (b = b') || (Brick.is_not_included b b') in
			let g b = Cover.for_all (f b) a in
			let answer = Cover.filter g a in
			if is_not_dummy answer then answer else (print_endline "clean_sweep bad behaviour"; a)

    (* bug *) (*what bug? Was it fixed? Actually I do think so!*)

    let intersection a1 a2 =
	    let f b1 b2 accu2 =
		    let brick = Brick.intersection b1 b2 in
			    if Brick.is_not_empty brick
			    then Cover.add brick accu2
			    else accu2 in
	    let g b1 accu1 = Cover.fold (f b1) a2 accu1 in
      let result = Cover.fold g a1 Cover.empty in
      if Cover.is_empty result
      then of_brick (empty a1)
      else result

    (*a cleverer version that removes useless cubes on the fly*)

(* NOT VERY EFFICIENT I THINK
    let intersection a1 a2 =
	    let f b1 b2 accu2 =
		    let brick = Brick.intersection b1 b2 in
        let accu2 = Cover.filter (fun b -> Brick.is_not_included b brick) accu2 in
			    if Cover.exists (fun b -> Brick.is_included brick b) accu2
			    then accu2
			    else Cover.add brick accu2 in
	    let g b1 accu1 = Cover.fold (f b1) a2 accu1 in
      let result = Cover.fold g a1 Cover.empty in
      if Cover.is_empty result
      then of_brick (empty a1)
      else result
*)

    let complement a =
      let () = print_string "complement in progress..." ; flush stdout in
      let b = Cover.choose a in
      let a = Cover.remove b a in
      let b = Brick.complement b in
      let f b accu = intersection (Brick.complement b) accu in
      let answer = Cover.fold f a b in
      print_endline "done" ; answer

    let normalize a = complement (complement a)

    let union a1 a2 =
      if is_not_empty a1
      then
        if is_not_empty a2
        then Cover.union a1 a2 (*case a1 ≠ Ø ⋀ a2 ≠ Ø*)
        else a1 (*case a2 = Ø*)
      else a2 (*case a1 = Ø*)

    let rec union_list l = match l with
      | [x] -> x
      | x :: l -> union x (union_list l)
      | [] -> invalid_arg "union_list"

    let difference a1 a2 = intersection a1 (complement a2)

    (*topological structure*)

    (* Warning : the function interior requires its argument be a
       pre-normal form. *)

    let interior a =
			let f b accu = Cover.add (Brick.interior b) accu in
      let aux = Cover.fold f a Cover.empty in
      if Cover.is_empty aux
      then of_brick (empty a)
      else aux

    let closure a =
	    let f b accu = Cover.add (Brick.closure b) accu
	    in Cover.fold f a Cover.empty

    (*directed structure*)

    (*bug? future_extension and past_extension alters the second argument *)

    let extension_pattern extension_function a1 a2 =
			let answer = ref Cover.empty in
			let rec propagate b =
			  Cover.iter (fun b2 ->
							List.iter (fun b1 ->
                if not (Brick.is_included_in_area b1 !answer)
                then (answer := Cover.add b1 !answer ; propagate b1) )
				  (extension_function b b2) ) a2 in
      Cover.iter propagate (union (intersection a1 (closure a2)) (intersection a2 (closure a1))) ;
      if Cover.is_empty !answer
      then of_brick (empty a2)
      else !answer

    let future_extension = extension_pattern Brick.future_extension

    let past_extension = extension_pattern Brick.past_extension

    (*graded structure*)

    (*if both a1 and a2 are normal forms, then the resulting product
      is so*)

    let product a1 a2 =
	    let f p1 p2 accu2 =
			  let new_brick = Brick.product p1 p2 in
		    if Brick.is_not_empty new_brick
		    then Cover.add new_brick accu2
		    else accu2 in
	    let g p1 accu1 = Cover.fold (f p1) a2 accu1 in
      let answer = Cover.fold g a1 (Cover.empty) in
      if Cover.is_empty answer
      then of_brick (Brick.product (Cover.choose a1) (Cover.choose a2))
      else answer

    let rec product_list l = match l with
      | x :: l -> product x (product_list l)
      | [] -> one

    let product_array a = Array.fold_left (fun accu x -> product accu x) one a

    let rec product_fun f a b = if a > b then one else product (f a) (product_fun f (succ a) b)

		let generalized_product n aa =
			let full a = of_brick (full a) in
      Algebra.PCombinator.generalized_product ~zero ~full ~union ~product_fun n aa

		(*override the function full*)

		let full a = of_brick (full a)

    (* Successfully passed some tests.*)

    (* The array aaa represents a family of staircase functions in the
       following way: the kth term of the nth row is the locus of
       points x such that f_n(x) = k+1. The function returns the locus
       of points x such that f_0(x)+...+f_n(x) ⩾ level. If the
       argument level is set to 0, then the function returns the
       product of the union of each row. This function was originally
       dedicated to the calculation of the forbidden area generated by
       counting semaphores. *)

    (* The returned value might not be a normal form. *)

    module AC = Algebra.Combinator
    (struct

      type t = Cover.t
      let zero = zero
      let one = one
      let full a = full a
      let product = product
      let product_array = product_array
      let product_list = product_list
      let product_fun = product_fun
      let union = union
      let normalize = normalize
      let string_of x = string_of x
    end)

    let levelwise_product = AC.levelwise_product

    let locus_higher_than = AC.locus_higher_than

    let locus_lower_than = AC.locus_lower_than

    module FGSL = Algebra.Free.Graded.SemiLattice
      (
				struct
				  type t = G.t
				  let compare = G.compare
				  let string_of = fun g -> G.string_of g
				end
      )

    (*Warning: both functions factorize and fast_factorize need their argument
    to be given in normal form*)

    let factorize a =
      let degree = ref None in
      let f c accu =
	      try FGSL.add (Brick.to_array c) accu
		    with Invalid_argument _ -> degree := Some (Brick.dimension c); accu in
	    let a = Cover.fold f a FGSL.zero in
      FGSL.factorize ?degree:!degree ~unpacked:true a

    let fast_factorize a =
      let d = dimension a in
      let linked b = Array.map G.is_not_full (Brick.to_array b) in
      let current = ref (Cover.fold (fun b accu -> (linked b) :: accu) a []) in
      for i = 0 to pred d
      do
        let amalgam = ref (Array.make d false) in
        let () = current := List.fold_left
          (fun accu b ->
            if b.(i)
            then (amalgam := Array.init d (fun k -> b.(k) || (!amalgam).(k)) ; accu)
            else b :: accu) [] !current in
        current := !amalgam :: !current
      done ;
      !current

    (*Heuristique : on essaie de trouver un espace d'état "proche" et factorisable.*)

    let weak_factorize a = [] (*TODO*)

  end(*Area*)

end(*Make*)
