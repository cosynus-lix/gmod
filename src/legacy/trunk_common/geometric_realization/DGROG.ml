(* Directed Geometric Realization of Graphs *)
(* dgrog.ml *)
(* Part of the GMOD project *)

(*
   V stands for "vertex"
   E stands for "edges" though it should actually been thought of as "labels"
   B stands for "bounds" and is the type of values over which the geometry is defined
*)

let display_all = ref false

(*UTF8 characters are used by default*)

module Glyph =
struct
  let direct_sum = ref "⨁"
  let empty_set = ref "∅"
  let circle = ref "𝕊¹"
end

(*Default glyphs can be override*)

let define_glyph
    ?direct_sum
    ?empty_set
    ?circle
    ()
    =
	  (match direct_sum with
	    | Some s -> Glyph.direct_sum := s
	    | _ -> ());
	  (match empty_set with
	    | Some s -> Glyph.empty_set := s
	    | _ -> ());
	  (match circle with
	    | Some s -> Glyph.circle := s
	    | _ -> ())

module Make
  (V:sig include Graph.Sig.COMPARABLE       val string_of: t -> string end)
  (E:sig include Graph.Sig.ORDERED_TYPE_DFT val string_of: t -> string end)
  (B:sig include Sig.Bound                  val string_of: t -> string end) =

struct

  module Skeleton =
  struct

    let string_of_V = V.string_of
    let string_of_E = E.string_of

    include Graph.Imperative.Digraph.ConcreteLabeled(V)(E)

    let string_of_edge ?(shift=0) e =
      Printf.sprintf "%s%s %s %s"
        (String.make shift ' ')
        (string_of_V (E.src e))
        (string_of_E (E.label e))
        (string_of_V (E.dst e))

    let string_of ?(shift=0) g =
      let return = fold_edges_e (fun e accu -> (string_of_edge e) :: accu) g [] in
        Common.String.string_as_columns
        ~shift
        ~column_separator:(!Glyph.direct_sum ^ " ")
        (*Common.String.longest_length segments*) 180 (List.rev return)

    let find_vertex predicate g =
      let answer = ref None in
      try
        iter_vertex (fun v -> if predicate v then (answer := Some v; raise Exit)) g ;
        raise Exit
      with Exit ->
        (match !answer with Some v -> v | None -> raise Not_found)

  end (*Skeleton*)

  module Sh = ODA.BooleanAlgebra(B) (* TODO Sh -> BA *)
  module HL = ODA.HalfLine(B)
  module Ci = ODA.Circle(B)

  module Enriched =
  struct
    module V = (Set.Make(V):Set.S with type elt=V.t)
    module E = struct

    (*Same representation for halfline and circle. For each map, either all 
    values contain zero or none of them.*)

    type t = Sh.t Map.Make(E).t

    include (Map.Make(E):
			sig (*Ocaml 3.12.0 required*)
			  type key
		    (*type (+'a) t*)
			  val empty: Sh.t Map.Make(E).t
			  val is_empty: Sh.t Map.Make(E).t -> bool
			  val mem: key -> Sh.t Map.Make(E).t -> bool
			  val add: key -> Sh.t -> Sh.t Map.Make(E).t -> Sh.t Map.Make(E).t
			  val singleton: key -> Sh.t -> Sh.t Map.Make(E).t
			  val remove: key -> Sh.t Map.Make(E).t -> Sh.t Map.Make(E).t
			  val merge: (key -> 'a option -> 'b option -> 'c option) ->
						  'a Map.Make(E).t -> 'b Map.Make(E).t -> 'c Map.Make(E).t
			  val compare: (Sh.t -> Sh.t -> int) -> Sh.t Map.Make(E).t -> Sh.t Map.Make(E).t -> int
			  val equal: (Sh.t -> Sh.t -> bool) -> Sh.t Map.Make(E).t -> Sh.t Map.Make(E).t -> bool
			  val iter: (key -> Sh.t -> unit) -> Sh.t Map.Make(E).t -> unit
			  val fold: (key -> Sh.t -> 'b -> 'b) -> Sh.t Map.Make(E).t -> 'b -> 'b
			  val for_all: (key -> Sh.t -> bool) -> Sh.t Map.Make(E).t -> bool
			  val exists: (key -> Sh.t -> bool) -> Sh.t Map.Make(E).t -> bool
			  val filter: (key -> Sh.t -> bool) -> Sh.t Map.Make(E).t -> Sh.t Map.Make(E).t
			  val partition: (key -> Sh.t -> bool) -> Sh.t Map.Make(E).t -> Sh.t Map.Make(E).t * Sh.t Map.Make(E).t
			  val cardinal: Sh.t Map.Make(E).t -> int
			  val bindings: Sh.t Map.Make(E).t -> (key * Sh.t) list
			  val min_binding: Sh.t Map.Make(E).t -> (key * Sh.t)
			  val max_binding: Sh.t Map.Make(E).t -> (key * Sh.t)
			  val choose: Sh.t Map.Make(E).t -> (key * Sh.t)
			  val split: key -> Sh.t Map.Make(E).t -> Sh.t Map.Make(E).t * Sh.t option * Sh.t Map.Make(E).t
			  val find: key -> Sh.t Map.Make(E).t -> Sh.t
			  val map: (Sh.t -> 'a) -> Sh.t Map.Make(E).t -> 'a Map.Make(E).t
			  val mapi: (key -> Sh.t -> 'a) -> Sh.t Map.Make(E).t -> 'a Map.Make(E).t
			end with type key = E.t)

	let default = empty

	let compare = Pervasives.compare

	let size m = fold (fun _ _ accu -> succ accu) m 0

	let contains_a_single_arrow m =
	  try fold (fun _ _ accu -> if accu then raise Exit else true) m false
	  with Exit -> false

	(* Add zero to all the values of the map *)
	let add_zero m = map (fun x -> Sh.add_zero x) m

	(* Remove zero from all the values of the map *)
	let remove_zero m = map (fun x -> Sh.remove_zero x) m

  let atom_zero_map m = map (fun _ -> Sh.(atom zero)) m

	let all test m =
	  try iter (fun k x -> if not (test k x) then raise Exit) m ; true
	  with Exit -> false

	let some test m =
	  try iter (fun k x -> if test k x then raise Exit) m ; false
	  with Exit -> true

	(*If all the bindings of the map satisfy the test or all of them do not satisfy it, then the common result of the
	test is returned as Some b, otherwise None is returned.*)
	let share invariant m =
		try
			fold
				(
					fun k x accu -> match accu with
						| Some b -> if b = invariant k x then accu else raise Exit
						| None -> Some (invariant k x)
				) m None
		with Exit -> None

	let contains_source m =
	  try iter (fun _ x -> if Sh.contains_zero x then raise Exit) m ; false
	  with Exit -> true

	(*The function complement preserves the carriers.*)

	let complement m = map (fun ar -> Sh.complement ar) m

	(*Warning: if the option carrier_preserved is unset, the
	  function intersection returns a value whose carrier is
	  reduced when edges carrying an empty area is met.*)

	let intersection ?(carrier_preserved=true) m1 m2 =
		let f k x accu =
      try
        let answer = Sh.intersection x (find k m2) in
        if carrier_preserved || Sh.contains_more_than_zero answer
        then add k answer accu
        else accu
      with Not_found ->
        if carrier_preserved
        then add k Sh.empty accu
        else accu in
    fold f m1 empty

	(*Warning: the function union returns a value whose
	  carrier is the union of the carriers of the arguments.*)

	let union m1 m2 =
		let f k x accu =
			try add k (Sh.union x (find k m2)) accu
			with Not_found -> add k x accu
		in fold f m1 m2

	let difference ?(carrier_preserved=true) m1 m2 =
    let f k x accu =
			let answer =
			  try Sh.difference (find k m1) x
			  with Not_found -> Sh.empty in
			if carrier_preserved || Sh.contains_more_than_zero answer
			then add k answer accu
			else accu in
    fold f m2 m1

	let full m = map (fun _ -> Sh.full) m

	let drain m = map (fun _ -> Sh.empty) m

(*
  let propagate_from_zero ?flag m = map (fun ar ->
    let open Sh in
    if value_of_the_first_bound ar = zero
    then first_connected_component ?flag ar
    else empty) m
*)

(*
	let empty_bindings m = map (fun _ -> Sh.empty) m
*)

	module HalfLine =
	struct

	  (* To be tested *)

	  let closure ?unbounded_answer m =
	    match unbounded_answer with
	      | Some br ->
		      let f x =
				    let unbounded_answer = ref false in
				    let aux = HL.closure ~unbounded_answer x in
				    if !unbounded_answer then br:=true ; aux
					in map f m
	      | None -> map HL.closure m

	  let interior ?(add_zero=false) m = map
	    (fun ar -> if add_zero then HL.interior (HL.add_zero ar) else HL.interior ar) m

	  let future_extension ?flag ?(add_zero1=false) ?(add_zero2=false) m1 m2 = mapi (fun k ar2 ->
      let ar1 = (*rectification*)
        try
          let aux = find k m1 in
          if add_zero1
          then Sh.add_zero aux
          else Sh.remove_zero aux
        with Not_found ->
          if add_zero1
          then Sh.(atom zero)
          else Sh.empty in
      let ar2 = (*rectification*)
        if add_zero2
        then Sh.add_zero ar2
        else Sh.remove_zero ar2 in
      HL.future_extension ?flag ar1 ar2) m2 (*HL instead of Sh: fixed 11.09.2013 14:05:39 *)

    let future_closure m =
      let some_unbounded = ref false in
      let return = map (fun ar ->
        let ar , unbounded = HL.future_closure ar in
        let () = some_unbounded := !some_unbounded || unbounded in
        ar) m in
      return , !some_unbounded

	  let past_extension ~add_zero1 ~add_zero2 ~add_last_connected_component m1 m2 = mapi (fun k ar2 ->
      let ar1 = (*rectification*)
        let aux =
          try
            let aux = find k m1 in
            if add_zero1
            then Sh.add_zero aux
            else Sh.remove_zero aux
          with Not_found ->
            if add_zero1
            then Sh.atom Sh.zero
            else Sh.empty in
        if add_last_connected_component
        then
          let lcc = Sh.last_connected_component ar2 in
          if HL.is_not_bounded lcc
          then Sh.union aux lcc
          else aux
        else aux in
      let ar2 = (*rectification*)
        if add_zero2
        then Sh.add_zero ar2
        else Sh.remove_zero ar2 in
      HL.past_extension ar1 ar2) m2

    let past_closure m =
      let some_contains_zero = ref false in
      let return = map (fun ar ->
        let ar , unbounded = HL.past_closure ar in
        let () = some_contains_zero := !some_contains_zero || (try HL.glb ar = Sh.zero with HL.Undefined -> false) in
        ar) m in
      return , !some_contains_zero

	  let closure_contains_source m =
	    try iter (fun _ x -> if HL.closure_contains_zero x then raise Exit) m ; false
	    with Exit -> true

	  let closure_contains_target m =
	    try iter (fun _ x -> if HL.is_not_bounded x then raise Exit) m ; false
	    with Exit -> true

	end(*HalfLine*)

	module Circle =
	struct
	  let closure m = map Ci.closure m
	  let interior ?(add_zero=false) m = map
	    (fun ar -> if add_zero then Ci.interior (Sh.add_zero ar) else Ci.interior ar) m

	  let future_extension ?flag ?(add_zero1=false) ?(add_zero2=false) m1 m2 =
	    let aux_flag = ref false in
	    let first_round =
	      mapi
					(
					  fun k ar2 ->
					    let ar1 =
					      try
									let ar1 = find k m1 in
									if add_zero1 then Sh.add_zero ar1 else Sh.remove_zero ar1
					      with Not_found ->
						  if add_zero1 then Sh.atom Sh.zero else Sh.empty in
					    let ar2 =
					      if add_zero2
					      then Sh.add_zero ar2
					      else Sh.remove_zero ar2
					    in Ci.future_extension ~flag:aux_flag ar1 ar2
					) m2
	    in
	    if !aux_flag && add_zero2 (*Is the second round necessary?*)
	    then mapi
	      (
					fun k ar2 ->
					  let fcc = Sh.first_connected_component ar2 in
					  let fr = find k first_round in
					  if HL.glb fcc = Sh.zero
					  then Sh.add_zero (Sh.union fcc fr)
					  else Sh.add_zero fr
	      ) m2
	    else first_round

    let future_closure m =
      let some_unbounded = ref false in
      let return = map (fun ar ->
        let ar , unbounded = Ci.future_closure ar in
        let () = some_unbounded := !some_unbounded || unbounded in
        ar) m in
      return , !some_unbounded

	  let past_extension ~add_zero1 ~add_zero2 ~add_last_connected_component m1 m2 = mapi
	    (
	      fun k ar2 ->
		let ar2 = (*rectification*)
		  if add_zero2
		  then Sh.add_zero ar2
		  else Sh.remove_zero ar2
		in
		let ar1 = (*rectification*)
		  let aux =
		    try
		      let aux = find k m1 in
		      if add_zero1
		      then Sh.add_zero aux
		      else Sh.remove_zero aux
		    with
		      | Not_found ->
			if add_zero1
			then Sh.atom Sh.zero
			else Sh.empty
		  in
		  if add_last_connected_component
		  then
		    let lcc = Sh.last_connected_component ar2 in
		    if HL.is_not_bounded lcc
		    then Sh.union aux lcc
		    else aux
		  else aux
		in
		Ci.past_extension ar1 ar2
	    ) m2


    let past_closure m =
      let some_contains_zero = ref false in
      let return = map (fun ar ->
        let ar , unbounded = Ci.past_closure ar in
        let () = some_contains_zero := !some_contains_zero || HL.glb ar = Sh.zero in
        ar) m in
      return , !some_contains_zero


	  let closure_contains_origin m =
	    try
	      iter (fun _ x -> if Ci.closure_contains_zero x then raise Exit) m;false
	    with Exit -> true

	end(*Circle*)

    end(*E*)

  end(*Enriched*)

  module G = Graph.Imperative.Digraph.ConcreteLabeled(V)(Enriched.E)

  let to_skeleton g =
    let skeleton = Skeleton.create () in
    G.iter_edges_e (fun e ->
      let x = G.E.src e in
      let y = G.E.dst e in
      Enriched.E.iter
        (fun key _ ->
          let e = Skeleton.E.create x key y in
          Skeleton.add_edge_e skeleton e)
        (G.E.label e) ) g

  type t = { mutable vertices:Enriched.V.t ; edges:G.t }

  let find_vertex predicate {vertices=vs ; edges=es} =
    let answer = ref None in
    try
      Enriched.V.iter (
        fun v -> 
          if predicate v then 
          (answer := Some v; raise Exit)) vs ;
      raise Exit
    with Exit ->
      (match !answer with Some v -> v | None -> raise Not_found)

  type point = OnVertex of V.t | OnArrow of V.t * E.t * V.t * B.t

  let point_normal_form p = match p with
    | OnArrow (x,e,y,b) ->
        if b = B.least_regular_value then OnVertex x
        else p
    | _ -> p

  let make_point ?eyb x =
    match eyb with
      | None -> OnVertex x
      | Some (e,y,b) ->
          if b <> B.least_regular_value then OnArrow (x,e,y,b)
          else OnVertex x

  let auxiliary es x e b y =
    try Sh.belongs_to b (Enriched.E.find e (let _,m,_ = G.find_edge es x y in m))
    with Not_found -> false

  let point_mem p { vertices = vs ; edges = es } =
    match p with
    | OnVertex x -> Enriched.V.mem x vs
    | OnArrow (x,e,y,b) -> auxiliary es x e b y

  (* Semantics and Convention

   The underlying graph of some value g:t is called the carrier, it implicitly 
   describes the underlying topology of the directed space represented by g. In 
   particular, a vertex of the carrier which has neither outgoing nor ingoing 
   arrow is an isolated point of the underlying space. Given a value g:t, the 
   set g.vertices contains the vertices which actually belong to g. For example 
   if some arrows outgoing from x carry the area [0,1] while x does not belong 
   to g.vertices, then the directed space represented by g does not contain x. 
   The field vertices could be avoided but then the coherence of any value of 
   type t should be checked by all functions. That is to say either all the 
   arrows outgoing from x carry some area which contains zero or all the arrows 
   outgoing from x carry some area which does not contain zero. This approach 
   would require rather time demanding checks therefore we have chosen to 
   sacrifice some memory space instead.

   The collection of all values of type t sharing the same carrier forms a 
   boolean algebra, the functions complement, union, intersection and 
   difference are thus provided. As a consequence, the type t should be thought 
   as dependent of the underlying graph. Moreover, since the (geometric 
   realization of the) carrier admits a topology, we have the operators 
   interior and closure. It is important to keep in mind that the behavior of 
   these operators depend of the underlying graph. The directed structure of g 
   is provided by the operators future_extension and past_extension. *)

  (*to be tested*)

  let compare_points p1 p2 =
    match p1,p2 with
      | OnVertex x1,OnVertex x2 -> V.compare x1 x2
      | OnVertex x1,OnArrow (x2,_,_,b2) -> if x1=x2 && b2 = Sh.zero then 0 else  1
      | OnArrow (x2,_,_,b2),OnVertex x1 -> if x1=x2 && b2 = Sh.zero then 0 else -1
      | OnArrow (x1,e1,y1,b1),OnArrow (x2,e2,y2,b2) ->
					let dx = V.compare x1 x2 in
					if dx <> 0
					then dx
					else
					  let dy = V.compare y1 y2 in
					  if dy <> 0
					  then dy
					  else
					    let de = E.compare e1 e2 in
					    if de <> 0
					    then de
					    else B.compare b1 b2

  (*internal*)

  let choose es =
    let chosen = ref None in
    try
      G.iter_edges_e (fun (x,m,y) ->
        Enriched.E.iter (fun k ar ->
          match ar with
            | b::_ -> chosen := Some (x,k,Sh.rvb b,y);raise Exit
            | _ -> raise Exit) m) es ;
      !chosen (*es = Ø => nothing is returned (None)*)
    with Exit -> !chosen (*es = {p} => something is returned (Some p)*)

  let auxiliary_compare p1 p2 =
    match p1,p2 with
      | Some (x1,e1,b1,y1) , Some (x2,e2,b2,y2) -> (
          let dx = V.compare x1 x2 in
          if dx <> 0
          then dx
          else
            let dy = V.compare y1 y2 in
            if dy <> 0
            then dy
            else
              let de = E.compare e1 e2 in
              if de <> 0
              then de
              else B.compare b1 b2)
      | _ -> invalid_arg "compare_points [Dgrog]"

  let compare_singletons { vertices = vs1 ; edges = es1 } { vertices = vs2 ; edges = es2 } =
    try
      let x1 = Enriched.V.choose vs1 in
      try
        let x2 = Enriched.V.choose vs2 in
        V.compare x1 x2
      with Not_found -> -1 (* vs1 ≠ Ø ∧ vs2 = Ø *)
    with Not_found -> (* vs1 = Ø *)
      if not (Enriched.V.is_empty vs2)
      then 1 (* vs1 = Ø ∧ vs2 ≠ Ø *)
      else
        let p1 = choose es1 in
        let p2 = choose es2 in
        auxiliary_compare p1 p2

  (*tests*)

  let belongs_to ~x ?(e = E.default) ?b ?y { vertices = vs ; edges = es } =
    match b with
      | None -> Enriched.V.mem x vs
      | Some b -> 
          if b = B.least_regular_value
          then Enriched.V.mem x vs
          else match y with
            | None -> auxiliary es x e b x
            | Some y -> auxiliary es x e b y

  let is_empty {vertices=vs;edges=es} =
    Enriched.V.is_empty vs &&
			try
			  G.iter_edges_e (
          fun (_,m,_) ->
            if Enriched.E.some (fun _ ar -> Sh.contains_more_than_zero ar) m
            then raise Exit) es ;
        true
			with Exit -> false

  let is_not_empty { vertices = vs ; edges = es } =
    (not (Enriched.V.is_empty vs)) ||
			try
			  G.iter_edges_e (fun (_,m,_) ->
          if Enriched.E.some (fun _ ar -> Sh.contains_more_than_zero ar) m
          then raise Exit) es;
			  false
			with Exit -> true

  let is_full {vertices=vs;edges=es} = (
      try G.iter_vertex (fun x -> if not (Enriched.V.mem x vs) then raise Exit) es ; true
      with Exit -> false) &&
			try
			  G.iter_edges_e
			    (
			      fun (_,m,_) ->
							if Enriched.E.some (fun _ ar -> Sh.lacks_more_than_zero ar) m
							then raise Exit
			    ) es ;
			  true
			with Exit -> false

  let is_not_full {vertices=vs;edges=es} =
    (
      try
	G.iter_vertex
	  (
	    fun x ->
	      if not (Enriched.V.mem x vs)
	      then raise Exit
	  )
	  es;
	false
      with
	| Exit -> true
    ) &&
      (
	try
	  G.iter_edges_e
	    (
	      fun (_,m,_) ->
		if Enriched.E.some (fun _ ar -> Sh.lacks_more_than_zero ar) m
		then raise Exit
	    )
	    es;
	  false
	with
	  | Exit -> true
      )

  let is_included {vertices=vs1;edges=es1} {vertices=vs2;edges=es2} =
    (Enriched.V.subset vs1 vs2) &&
      (
				try
				  G.iter_edges_e
				    (
				      fun (x,m1,y) ->
								let (_,m2,_) = (G.find_edge es2 x y) in
								if Enriched.E.some
								  (
								    fun key ar1 ->
								      not (Sh.is_included (Sh.remove_zero ar1) (Enriched.E.find key m2))
								  ) m1
								then raise Exit
				    ) es1;
				  true
				with
				  | Exit -> false
				  | Not_found -> failwith "dgrog.ml: is_included"
      )

  (* Given g:t, <g> denotes the set represented by g. The function
     compare must satisfy the following properties:

     1 - it is a preorder ≼

     2 - if g0 ~ g1 then <g0> = <g1>

     3 - if <g0> ⊆ <g1> then <g0> ≼ <g1> (i.e. compare g0 g1 = -1)

     4 - the relation ≼ is total

     By convention, compare g0 g1 returns a nonpositive
     (resp. nonnegative) value if g0 ≺ g1 (resp. g0 ≻ g1), and 0 if
     and only if g0 ~ g1.

     Let (E,⩽) be a well ordered set. Writing A ⩽ B when min(B\A) ⩽
     min(A\B) with the convention that min(Ø) = +ꝏ, where +ꝏ is some
     value greater than any element of E, we define a well order over
     ℙ(E) which extends both ⩽ (up to the embedding e ↦ {e}) and the
     inclusion ⊆.

     The vertices come first they are ordered by the function
     V.compare.

     A and B stand for vs1 and vs2.

     Remark the following equivalence:
     min(A Δ B) ∈ A <=> min(B\A) ⩽ min(A\B) <=> A ⩽ B

     The relation ⩽ is reflexive: obvious.

     The relation ⩽ is antisymetric: remark B\A ∩ A\B = Ø hence min(B\A) =
     min(A\B) = x implies x = +ꝏ that is to say B\A = A\B = Ø.

     The relation ⩽ is transitive: suppose A ⩽ B ⩽ C which means that
     min(A Δ B) ∈ B and min(B Δ C) ∈ C, where A Δ B = B\A ∪ A\B.
     Without loss of generality We can suppose A ∩ B ∩ C = Ø. In
     particular we have min(B Δ C) ⩽ min(A Δ B), thus min(B Δ C) ∉ A.
     Given x ∈ A\C we have:
     case 1: x ∈ A ∩ B, hence x ∉ C therefore min(B Δ C) ⩽ x,
     case 2: x ∈ A\B, hence min(B Δ C) ⩽ min(A Δ B) ⩽ x,
     it follows min(A Δ C) ∈ C. Thus ⩽ is a partial order indeed.

     The soundness of the following function lies on the fact the
     "compare" function provided by the functor Set.Make is actually
     defined as above. *)

  let auxiliary_compare es1 es2 =
    let answer = ref None in
    let return x y d = answer := Some (x,y,d) ; raise Exit in
    (
      try
	(
	  G.iter_edges_e (*for all bundles of arrows from es1*)
	    (
	      fun (x1,m1,y1) ->
		try (*to find the corresponding bundle in es2*)
		  let (_,m2,_) = G.find_edge es2 x1 y1 in
		  (
		    Enriched.E.iter (*for all arrows of the bundle m1*)
		      (
			fun k1 ar1 ->
			  try (*to find the corresponding arrow in the bundle m2*)
			    let d = Sh.compare (*zeros are removed to normalize*)
			      (Sh.remove_zero ar1)
			      (Sh.remove_zero (Enriched.E.find k1 m2))
			    in
			    if d<>0 then return x1 y1 d
			  with (*there is no corresponding arrow in m2*)
			    | Not_found -> (*check whether m1 is empty...(1)*)
			      if Sh.contains_more_than_zero ar1
			      then return x1 y1 1 (*(1) if it is not so, then m1 is bigger*)
		      ) m1
		  )
		with
		  | Not_found -> (*there is no corresponding bundle in es2*)
		    (
		      G.iter_edges_e (*check whether es1 is empty...(1)*)
			(
			  fun (_,m,_) ->
			    if Enriched.E.some (fun _ ar -> Sh.contains_more_than_zero ar) m1
			    then return x1 y1 1 (*(1) if it is not so, then es1 is bigger*)
			)
			es1
		    )
	    ) es1
	)
      with
	| Exit -> ()
    )
    ;
    !answer

  let compare {vertices=vs1;edges=es1} {vertices=vs2;edges=es2} =
    let delta = Enriched.V.compare vs1 vs2 in
    if delta <> 0
    then delta
    else
      match auxiliary_compare es1 es2 with
	| Some (x1,y1,d1) ->
	  if d1<0
	  then d1
	  else
	    (
	      match auxiliary_compare es2 es1 with
		| Some (x2,y2,d2) -> -d2
		| None -> 0
	    )
	| None ->
	  match auxiliary_compare es2 es1 with
	    | Some (x2,y2,d2) -> -d2
	    | None -> 0

  let replace_edge_e g x y m = (try G.remove_edge g x y with Invalid_argument _ -> ()) ; G.add_edge_e g (x,m,y)

  let union_edge_e g x y m =
    let (_,m',_) = try G.find_edge g x y with Not_found -> x , Enriched.E.empty , y in
    replace_edge_e g x y (Enriched.E.union m m')

  (* The function coherent_form add or remove zero from the
     one-dimensional area carried by an edge when the source of the
     edge does or does not belong to the value (which is a set) of the
     field [vertices]. *)

  let coherent_form {vertices=vs;edges=es} =
	  let f x = List.iter
	    (
	      if Enriched.V.mem x vs
	      then fun (_,m,y) -> replace_edge_e es x y (Enriched.E.add_zero m)
	      else fun (_,m,y) -> replace_edge_e es x y (Enriched.E.remove_zero m)
	    )
	    (G.succ_e es x)
		in G.iter_vertex f es


  (* In the "parition" normal form, the one dimensional areas carried
     by the edges of the graph does not contain zero. This convention
     provides a partition of the geometric realization. *)

  let partition_form ({vertices=vs;edges=es} as z) =
    z.vertices <- G.fold_vertex
      (
	fun x accu ->
	  if List.exists (fun (_,m,_) -> Enriched.E.some (fun _ ar -> Sh.contains_zero ar) m) (G.succ_e es x)
	  then Enriched.V.add x accu
	  else accu
      ) es vs
    ;
    G.iter_edges_e (fun (x,m,y) -> replace_edge_e es x y (Enriched.E.remove_zero m)) es

  (* In the "overlap" normal form, the one dimensional areas carried
     by some edge contains zero iff its source belongs to the field
     vertices. This normal form if useful for computations of the
     topological structure and the directed structure. *)

  (*not coherent with the overall convention*)

  let overlap_form ({vertices=vs;edges=es} as z) =
    z.vertices <- G.fold_vertex
      (
	fun x accu ->
	  if List.exists (fun (_,m,_) -> Enriched.E.some (fun _ ar -> Sh.contains_zero ar) m) (G.succ_e es x)
	  then Enriched.V.add x accu
	  else accu
      ) es vs
    ;
    G.iter_vertex
      (
	fun x ->
	  List.iter
	    (
	      fun y -> match G.find_edge es x y with
		| _,m,_ ->
		  if Enriched.V.mem x z.vertices
		  then replace_edge_e es x y (Enriched.E.add_zero m)
		  else replace_edge_e es x y (Enriched.E.remove_zero m)
	    ) (G.succ es x)
      ) es

  (* Constructors *)

  let of_string s = failwith "of_string not provided [DGROG]"

  let full {vertices=vs;edges=es} =
    {
      vertices = G.fold_vertex (fun x accu -> Enriched.V.add x accu) es vs(*Enriched.V.empty*);
      edges =
				let answer = G.copy es in
				let () = G.iter_edges_e (fun (x,m,y) -> replace_edge_e answer x y (Enriched.E.full m)) es
				in answer
    }

  let empty {vertices=vs;edges=es} =
	    {
	      vertices = Enriched.V.empty;
	      edges =
		      let answer = G.copy es in
					let () = G.iter_edges_e (fun (x,m,y) -> replace_edge_e answer x y (Enriched.E.drain m)) es
					in answer
	    }

  let from_graph ?(with_vertices=true) g =
    {
      vertices = G.fold_vertex (fun x accu -> Enriched.V.add x accu) g Enriched.V.empty;
      edges = g
    }

  let vertices {vertices=x;edges=_} = x

  let edges {vertices=_;edges=x} = x

  (* Side effect warning: gather alters es *)

  let gather ?(normalize=false) vs es =
    if normalize then Enriched.V.iter (fun x -> G.add_vertex es x) vs ;
    { vertices = vs ; edges = es }

(*The vertices of the space represented by es should not lie on its boundary. As its name suggests this function is
dedicated to the contruction of mutex buzy sections*)

	let of_mutex_section es =
		let f (_,m,_) = Enriched.E.some (fun _ ar -> Sh.contains_zero ar) m in
		let g x accu =
			if List.exists f (G.succ_e es x)
		  then Enriched.V.add x accu
		  else accu in
		let vs = G.fold_vertex g es Enriched.V.empty
		in gather vs es

  let of_vertices vs =
    let g = G.create () in
    Enriched.V.iter (fun x -> G.add_vertex g x) vs; (*normal form*)
    {vertices=vs;edges=g}

  (* Returns a copy of the empty subspace of the empty space *)

  let create () = {vertices=Enriched.V.empty;edges=G.create ~size:0 ()}

  let copy x = {vertices=x.vertices;edges=G.copy x.edges}

  let count_edges {vertices=_;edges=es} x y =
    try
      let (_,m,_) = G.find_edge es x y in
      Enriched.E.fold (fun k x accu -> accu+1) m 0
    with Not_found -> 0

  let draw ?(add_end=false) ({vertices=vs;edges=es} as z) x s ar y =
    let m = try
				let (_,m,_) = G.find_edge es x y in
				let ar = try Sh.union (Enriched.E.find s m) ar
				  with Not_found -> ar
				in Enriched.E.add s ar m
      with Not_found -> Enriched.E.add s ar Enriched.E.empty in
    let () = replace_edge_e es x y m in (* update z[x,y] *)
    let aux = if Sh.contains_zero ar then Enriched.V.add x vs else vs
		in z.vertices <- if add_end then Enriched.V.add y aux else aux

  let from_skeleton s f =
    let g = create () in
    let () =
      Skeleton.(iter_edges_e
        E.(fun e ->
            let ar,add_end = f e in
            draw g ~add_end (src e) (label e) ar (dst e)) s) in
    g

  let empty_from_skeleton s = from_skeleton s (fun _ -> HL.empty , false)

  let full_from_skeleton s = from_skeleton s (fun _ -> HL.full , true)

  let make vl el =
    let es = G.create () in
    let vs = List.fold_left (fun accu x -> G.add_vertex es x;Enriched.V.add x accu) Enriched.V.empty vl in
    let return = {vertices=vs;edges=es} in
    List.iter (fun (x,s,ar,y) -> draw return x s ar y) el;
    return

  let of_point p =
    match p with
      | OnVertex x        -> gather (Enriched.V.singleton x) (G.create ())
      | OnArrow (x,e,y,b) -> let g = create () in draw g x e (Sh.atom b) y ; g

  let discrete pl =
    let return = create () in
    List.iter (* Warning: the graph structure upon which the library is built is imperative *)
      (
	fun p -> match p with
	  | OnVertex p -> (return.vertices <- Enriched.V.add p return.vertices ; G.add_vertex return.edges p)
	  | OnArrow (x,e,y,b) ->
	    if b <> Sh.zero
	    then draw return x e (Sh.atom b) y
	    else (return.vertices <- Enriched.V.add x return.vertices ; G.add_vertex return.edges x)
      )
      pl
    ; return

  (*to be tested*)

  (* TODO: provide semantics *)

  let glb {vertices=vs;edges=es} =
    try
      Some (gather (Enriched.V.singleton (Enriched.V.min_elt vs)) (G.create ~size:0 ()))
    with
      | Not_found ->
	let answer = ref None in
	try
	  G.iter_edges_e
	    (
	      fun (x,m,y) ->
		Enriched.E.iter
		  (
		    fun k ar ->
		      if Sh.contains_more_than_zero ar
		      then
			(
			  let glb = HL.glb (Sh.remove_zero ar) in
			  if glb <> Sh.zero
			  then answer :=
			    (let g = create () in draw g x k (Sh.atom glb) y ; Some g
			    )
			  ;
			  raise Exit
			)
		  ) m
	    ) es
	  ; !answer
	with
	  | Exit -> !answer

  (*The function erase alter its first argument z: imperative*)

  let erase ?(remove_end=false) ({vertices=vs;edges=es} as z) x s ar y b =
    try
      (
	let m =
	  let (_,m,_) = G.find_edge es x y in
	  try Enriched.E.add s (Sh.difference (Enriched.E.find s m) ar) m
	  with Not_found -> m
	in
	let () = (* update z[x,y] *)
	  (try G.remove_edge es x y with | _ -> ()) ;
	  G.add_edge_e es (x,m,y)
	in
	let aux =
	  if Sh.contains_zero ar
	  then Enriched.V.remove x vs
	  else vs
	in
	z.vertices <-
	  if remove_end
	  then Enriched.V.remove y aux
	  else vs
      )
    with
      | Not_found -> ()

  (*The function erase alter its first argument g: imperative*)

  let erase_vertex g v = g.vertices <- Enriched.V.remove v g.vertices

  let ghost vl el =
    let answer = create () in
    List.iter (fun (x,s,y) -> draw answer x s Sh.empty y) el;
    List.iter (fun x -> G.add_vertex answer.edges x) vl;
    answer

  let solid vl el =
    let answer = create () in
    List.iter (fun (x,s,y) -> draw ~add_end:true answer x s Sh.full y) el;
    List.iter (fun x -> G.add_vertex answer.edges x) vl;
    answer.vertices <- List.fold_left (fun accu x -> Enriched.V.add x accu) answer.vertices vl;
    answer

  let draw_edges_list el =
    let answer = create () in
    List.iter (fun (x,s,ar,y,add_end) -> draw ~add_end answer x s ar y) el;
    answer

  let draw_edges_array el =
    let answer = create () in
    Array.iter (fun (x,s,ar,y,add_end) -> draw ~add_end answer x s ar y) el;
    answer

  let isolated_points {vertices=vs;edges=es} = Enriched.V.filter
    (
      fun x -> (not(G.mem_vertex es x)) ||
        (
          try
            G.iter_succ_e (
              fun (_,m,y) ->
                (
                  if
                    (
                    Enriched.E.some
                      (
                        fun _ ar ->
                          try HL.glb ar = Sh.zero && (Sh.first_connected_component ar <> Sh.atom Sh.zero)
                          with | HL.Undefined -> false
                      )
                    ) m (*not isolated if satisfied*)
                  then raise Exit
                )
              ) es x;
            true
          with
            | Exit -> false
            (*| Invalid_argument _ -> true iter_succ_e raise Invalid_argument when the vertex x does not belongs to the graph.*)
        ) &&
        (
          try
            G.iter_pred_e
              (
                fun (y,m,_) -> 
                  (
                    if (Enriched.E.some (fun _ ar -> HL.is_not_bounded ar)) m (*not isolated if satisfied*)
                    then raise Exit
                  )
              ) es x
            ;
            true
          with
            | Exit -> false
            (*| Invalid_argument _ -> true*)
        )
    )
    vs

  (* display *)

  let terminal_width = 120

  let string_of
      ?(endline=true)
      ?(shift=0)
      ?(hollow=false)
      ?(verbose=false)
      ?(pretty=true)
      {vertices=vs;edges=es} =
    let segments =
      G.fold_edges_e
				(
				  fun (x,m,y) accu2 ->
				    (
				      Enriched.E.fold
								(
								  fun k ar accu ->
								  let source = V.string_of x in
								  let target = V.string_of y in
								  let instructions =
									  let ks = E.string_of k in
										  if pretty
										  then
										    if ks <> "" then " " ^ ks else ""
										  else (" \"" ^ ks ^ "\"") in
									let one_dimensional_area =
										let ar = if Enriched.V.mem x vs then ar else Sh.remove_zero ar in
										  if hollow
										  then ""
										  else
										    if pretty
										    then
										      (
														let aux =
														  if x<>y
														  then HL.string_of
														    ~empty_set_denotation:(!Glyph.empty_set)
														    ~open_infinity:(not (Enriched.V.mem y vs))
														    ar
														  else Ci.string_of
														    ~empty_set_denotation:(!Glyph.empty_set)
														    ~full_set_denotation:(!Glyph.circle)
														    ar
														in if aux <> "" then " " ^ aux else ""
										      )
										    else " [" ^ (Sh.string_of ar) ^ "]" in
							    let entry = Printf.sprintf "%s%s%s %s "
											source instructions one_dimensional_area target
							    in entry :: accu
								) m []
				    ) :: accu2
				) es [] in
    let segments = List.concat segments in
    let segments = Common.String.string_as_columns
      ~shift
      ~column_separator:(!Glyph.direct_sum ^ " ")
      (*Common.String.longest_length segments*) 180 segments in
    let criterion_in v =
      if verbose
      then Enriched.V.mem v vs
      else (Enriched.V.mem v vs) && (G.succ es v = []) && (G.pred es v = []) in
    let criterion_out v =
      if verbose
      then not (Enriched.V.mem v vs)
      else (not (Enriched.V.mem v vs)) && (G.succ es v = []) && (G.pred es v = []) in
    let vertex_in =
			G.fold_vertex
			  (
			    fun v accu3 ->
			      let mem = criterion_in v
			      in Printf.sprintf "%s%s"
							accu3 (*if accu3<>"" then accu3 else "   +{"*)
							(if mem then " "^(V.string_of v) else "")
			  ) es "" in
    let vertex_in = if vertex_in <> "" then "   +{"^vertex_in^" }\n" else "" in
    let vertex_out =
			G.fold_vertex
			  (
			    fun v accu3 ->
			      let mem = criterion_out v
			      in Printf.sprintf "%s%s" accu3 (*if accu3<>"" then accu3 else "   -{"*)
				(if mem then " "^(V.string_of v) else "")
			  ) es "" in
    let vertex_out = if vertex_out <> "" then "   -{"^vertex_out^" }\n" else "" in
    let output =
      Printf.sprintf "%s%s%s"
			(if segments<>"" then segments else "")
			(if vertex_in<>"   +{ }\n" then vertex_in else "")
			(if vertex_out<>"   -{ }\n" then vertex_out else "")
    in
    if endline then output else String.sub output 0 ((String.length output)-1)


  let string_of_edges
      ?(endline=true)
      ?(shift=0)
      ?(hollow=false)
      ?(pretty=true)
      ?(show=fun s i o t -> true)
      edges =
		    let segments =
		      G.fold_edges_e
						(
						  fun (x,m,y) accu2 ->
						    (
						      Enriched.E.fold
										(
										  fun k ar accu -> if show x k ar y
											  then
												  let source = (V.string_of x) in
												  let target = (V.string_of y) in
												  let instructions =
													  let ks = E.string_of k in
													  if pretty
													  then
													    if ks <> "" then " "^ks else ""
													  else (" \""^ks^"\"") in
												  let one_dimensional_area =
													  if hollow
													  then ""
													  else
													    if pretty
													    then
																let aux =
																  if x<>y
																  then HL.string_of
																    ~empty_set_denotation:(!Glyph.empty_set)
																    ar
																  else Ci.string_of
																    ~empty_set_denotation:(!Glyph.empty_set)
																    ~full_set_denotation:(!Glyph.circle)
																    ar
																in if aux <> "" then " "^aux else ""
													    else " ["^(Sh.string_of ar)^"]" in
											    let entry = Printf.sprintf "%s%s%s %s " source instructions one_dimensional_area target
											    in entry::accu
										    else accu
										) m []
						    )::accu2
						) edges [] in
		    let segments = List.concat segments in
		    let segments = Common.String.string_as_columns
		      ~shift
		      ~column_separator:(!Glyph.direct_sum^" ")
		      (*Common.String.longest_length segments*) 180 segments in
		    let output = if segments<>"" then segments else ""
		    in if endline then output else String.sub output 0 ((String.length output)-1)


  (* Debugging functions *)


  let display_underlying_arrow x y =
    let display = !display_all && true in
    if display
    then Printf.printf "%s --> %s\n"  
    (Message.blue ~bold:true (V.string_of x)) (Message.blue ~bold:true (V.string_of y))

  let before_propagation x flag y_in_vs2 =
    let display = !display_all && true in
    if display
    then
      Printf.printf "  flag = %b\n  y is in vs2 = %b\n%s\n"
	flag y_in_vs2
	(
	  if flag && y_in_vs2
	  then Message.green "propagate"
	  else Message.red "does not propagate\n"
	)

  let display_add_zeroes add_zero1 add_zero2 =
    let display = !display_all && true in
    if display then Printf.printf "  add_zero1 = %b\n  add_zero2 = %b\n" add_zero1 add_zero2

  let display_add_last_connected_component b =
    let display = !display_all && true in
    if display then Printf.printf "  add_last_connected_component = %b\n " b

  let display_map name x y m =
    let display = !display_all && true in
    if display
    then
      (
	print_endline name ;
	Enriched.E.iter
	  (
	    fun k ar -> Printf.printf "  %s : %s\n" (Message.blue (E.string_of k))
	      (
		if x<>y
		then HL.string_of ar
		else Ci.string_of ar
	      )
	  ) m
      )

  let call_counter = ref 1

  let how_many_points name vs =
    if !display_all && true then
      let counter = ref 0 in
      let () = Enriched.V.iter (fun _ -> incr counter) vs in
      Printf.printf "%s has %i points\n" name !counter

  let how_many_vertices name es =
    if !display_all && true then
      let answer = G.fold_vertex (fun _ accu -> accu+1) es 0 in
      Printf.printf "%s has %i vertex\n" name answer

  let how_many_arrows name es =
    if !display_all && true then
      let counter = ref 0 in
      let () = G.iter_edges_e (fun (_,m,_) -> counter := !counter + (Enriched.E.size m)) es  in
      Printf.printf "%s has %i arrows\n" name !counter

  let display_n_incr () = if !display_all && true then Printf.printf "%s %i\n" (Message.yellow ~bold:true "call number") !call_counter;incr call_counter

  (* Boolean operators *)

  let complement {vertices=vs;edges=es} =
    {
      vertices = G.fold_vertex
	(fun x accu -> if Enriched.V.mem x vs then accu else Enriched.V.add x accu)
	es Enriched.V.empty
      ;
      edges=
	let answer=G.create () in
	G.iter_vertex (fun x -> G.add_vertex answer x) es;
	G.iter_edges_e (fun (x,m,y) -> G.add_edge_e answer (x,(Enriched.E.complement m),y)) es;
	answer
    }

  let intersection {vertices=vs1;edges=es1} {vertices=vs2;edges=es2} =
    let es3 = G.create () in
    G.iter_edges_e
      (
	fun (x,m1,y) ->
	  try
	    let _,m2,_ = G.find_edge es2 x y in
	    let m3 = Enriched.E.intersection m1 m2 in
	    if m3 <> Enriched.E.empty
	    then G.add_edge_e es3 (x,m3,y)
	  with
	    | Not_found -> ()
      )
      es1;
    G.iter_vertex (fun x -> G.add_vertex es3 x) es1;
    gather (Enriched.V.inter vs1 vs2) es3

  let iter_intersection {vertices=vs1;edges=es1} ({vertices=vs2;edges=es2} as z) =
    G.iter_edges_e
      (
	fun (x,m1,y) ->
	  try
	    let _,m2,_ = G.find_edge es2 x y in
	    let m3 = Enriched.E.intersection m1 m2 in
	    if m3 <> Enriched.E.empty
	    then replace_edge_e es2 x y m3
	    else G.remove_edge es2 x y
	  with
	    | Not_found -> G.remove_edge es2 x y
      )
      es1
    ;
    z.vertices <- Enriched.V.inter vs1 vs2

  let union {vertices=vs1;edges=es1} {vertices=vs2;edges=es2} =
    let es3 = G.copy es2 in
    G.iter_edges_e
      (fun (x,m1,y) ->
        try
          let _,m2,_ = G.find_edge es2 x y in
          union_edge_e es3 x y (Enriched.E.union m1 m2)
        with Not_found -> G.add_edge_e es3 (x,m1,y))
      es1;
    gather (Enriched.V.union vs1 vs2) es3

  (*iter_union may be buggy ... anyway, what is its specification. It seems that it overwrites its first argument*)

  let iter_union {vertices=vs1;edges=es1} ({vertices=vs2;edges=es2} as z) =
    G.iter_edges_e
      (fun (x,m1,y) ->
        try
          let _,m2,_ = G.find_edge es2 x y in
          G.add_edge_e es2 (x,Enriched.E.union m1 m2,y)
        with Not_found -> G.add_edge_e es2 (x,m1,y))
      es1;
    z.vertices <- Enriched.V.inter vs1 vs2

  let difference {vertices=vs1;edges=es1} {vertices=vs2;edges=es2} =
    let es3 = G.create () in
    G.iter_edges_e
      (
	fun (x,m1,y) ->
	  try
	    let _,m2,_ = G.find_edge es2 x y in
	    let m3 = Enriched.E.difference m1 m2 in
	    if m3 <> Enriched.E.empty
	    then G.add_edge_e es3 (x,m3,y)
	  with
	    | Not_found -> G.add_edge_e es3 (x,m1,y)
      )
      es1;
    gather (Enriched.V.diff vs1 vs2) es3

  let iter_difference {vertices=vs1;edges=es1} ({vertices=vs2;edges=es2} as z) =
    G.iter_edges_e
      (
	fun (x,m2,y) ->
	  try
	    let _,m1,_ = G.find_edge es1 x y in
	    let m3 = Enriched.E.difference m1 m2 in
	    if m3 <> Enriched.E.empty
	    then replace_edge_e es1 x y m3
	    else G.remove_edge es1 x y
	  with
	    | Not_found -> ()
      )
      es2
    ;
    z.vertices <- Enriched.V.inter vs1 vs2

  let rough_compare g1 g2 =
    let dg1 = difference g1 g2 in
    let dg2 = difference g2 g1 in
    let p1 = glb dg1 in
    let p2 = glb dg2 in
    match p1,p2 with
      | Some p1,Some p2 -> compare_singletons p2 p1
      | Some _ ,None    ->  1 (* g2 ⊆ g1 *)
      | None   ,Some _  -> -1 (* g1 ⊆ g2 *)
      | None   ,None    ->  0

  (* Topological operators *)

  (* reminder: the topological closure of a finite union is the
     finite union of the topological closures *)

  let closure_on_site ({vertices=vs;edges=es} as z) =
    G.iter_edges_e
      (
	fun (x,m,y) ->
	  if x<>y
	  then
	    (
	      let unbounded_answer = ref false in
	      replace_edge_e es x y (Enriched.E.HalfLine.closure ~unbounded_answer m) ;
	      if not (Enriched.V.mem x vs) && Enriched.E.HalfLine.closure_contains_source m
	      then z.vertices <- Enriched.V.add x vs ;
	      if !unbounded_answer then z.vertices <- Enriched.V.add y z.vertices
	    )
	  else
	    (
	      replace_edge_e es x y (Enriched.E.Circle.closure m) ;
	      if not (Enriched.V.mem x vs) && (Enriched.E.Circle.closure_contains_origin m)
	      then z.vertices <- Enriched.V.add x vs
	    )
      )
      es

  let closure {vertices=vs;edges=es} =
    let answer = gather vs (G.copy es) in
    let () = closure_on_site answer in
    answer

  (* interior is buggy: the condition interior of the closure might not be enough *)

  let interior_on_site ({vertices=vs;edges=es} as z) =
    G.iter_edges_e
      (
	fun (x,m,y) ->
	  let add_zero = Enriched.V.mem x vs in
	  if x<>y
	  then replace_edge_e es x y (Enriched.E.HalfLine.interior ~add_zero m)
	  else replace_edge_e es x y (Enriched.E.Circle.interior ~add_zero m)
      )
      es
    ;
    z.vertices <- Enriched.V.filter
      (
	fun x -> Enriched.V.mem x vs &&
	  (
	    try
	      G.iter_succ_e
		(
		  fun (_,m,y) ->
		    if x<>y
		    then
		      (
			if Enriched.E.some (fun _ ar -> Sh.does_not_contain_zero ar) m
			then raise Exit
		      )
		    else
		      (
			if Enriched.E.some (fun _ ar -> Sh.does_not_contain_zero ar) m
			then raise Exit
		      )
		)
		es x
	      ;
	      true
	    with
	      | Exit -> false
	  )
	  &&
	  (
	    try
	      G.iter_pred_e
		(
		  fun (y,m,_) ->
		    if x<>y
		    then
		      (
			if Enriched.E.some (fun _ ar -> HL.is_bounded ar) m
			then raise Exit
		      )
		    else
		      (
			if Enriched.E.some (fun _ ar -> Sh.does_not_contain_zero ar) m
			then raise Exit
		      )
		)
		es x
	      ;
	      true
	    with
	      | Exit -> false
	  )
      )
      vs

  let interior {vertices=vs;edges=es} =
    let answer = gather vs (G.copy es) in
    let () = interior_on_site answer in
    answer

  (* Directed operators *)

  (* future_extension and propagate_in_the_future were tested...but problems remain 10 of september 2013 *)

  (*future_closure x

  semantics: add any point p that can be reached by a directed path starting in x, arriving at p, and whose image is entirely
  contained in the union of x and {p} *)

  (* side-effect *)

  (*There is still a bug see future_extension_1.cpr. But it does not come from future_extension. Indeed *)

  (*NB: We suppose that vertices is a subset of edges that is any element of 
  the list in the field "vertices" is a vertex of the graph in the field 
  "edges"*)

  (*was tested...but should be tested further*)

  let future_extension { vertices = vs1 ; edges = es1 } { vertices = vs2 ; edges = es2 } =

    let vs3 = ref Enriched.V.empty in
    let es3 = G.create () in
    let load x = (vs3 := Enriched.V.add x !vs3 ; G.add_vertex es3 x) in

    let already_propagated = ref Enriched.V.empty in

    let rec propagate ~add_zero2 x =
      if not (Enriched.V.mem x !already_propagated) then
        let () = already_propagated := Enriched.V.add x !already_propagated in
        let () = if add_zero2 then load x in
        G.iter_succ_e (fun (_,m2,y) ->
          let flag = ref false in
          let m1 = Enriched.E.atom_zero_map m2 in (*zero is already added*)
          let m3 =
            if x <> y (*do the arrows of the bundle carry halflines or circles?*)
            then Enriched.E.HalfLine.future_extension ~flag ~add_zero1:true ~add_zero2 m1 m2
            else Enriched.E.Circle.future_extension   ~flag ~add_zero1:true ~add_zero2 m1 m2 in
          let () = union_edge_e es3 x y m3 in
          if Enriched.( V.mem y vs2 && !flag )
          then propagate ~add_zero2:true y) es2 x in

    (*initialize*)
    let () = G.iter_vertex (fun x ->
      let add_zero1 = Enriched.V.mem x vs1 in
      let add_zero2 = Enriched.V.mem x vs2 in
      let () = if add_zero1 then propagate ~add_zero2 x in
      G.iter_succ_e (fun (_,m1,y) ->
        let flag = ref false in
        let (_ , m2 , _) = (*get the corresponding bundle of arrows of es2*)
          try G.find_edge es2 x y
          with Not_found -> x , Enriched.E.empty , y in
        let m3 =
          if x <> y (*do the arrows of the bundle carry halflines or circles?*)
          then Enriched.E.HalfLine.future_extension ~flag ~add_zero1 ~add_zero2 m1 m2
          else Enriched.E.Circle.future_extension   ~flag ~add_zero1 ~add_zero2 m1 m2 in
        let () = union_edge_e es3 x y m3 in
        if Enriched.( V.mem y vs2 && ( !flag || E.some (fun _ ar -> HL.is_not_bounded ar) m1 ) )
        then propagate ~add_zero2:true y) es1 x) es1 in

    let () = Enriched.V.iter (fun x -> propagate ~add_zero2:(Enriched.V.mem x vs2) x) vs1 in

    gather !vs3 es3

  (*end of body of function future_extension*)

  (*The rough version*)

(*
  let future_closure g = future_extension g (closure g)
*)

  let future_closure { vertices = vs ; edges = es } =
    let vs' = ref vs in
    let es' = G.create () in
    let () = G.iter_edges_e (fun (x , m , y) ->
      let m' , add_y = if x <> y then Enriched.E.HalfLine.future_closure m else Enriched.E.Circle.future_closure m in
      if add_y then vs' := Enriched.V.add y !vs' ;
      replace_edge_e es' x y m') es in
    gather !vs' es'

  let past_extension { vertices = vs1 ; edges = es1 } { vertices = vs2 ; edges = es2 } =

    let vs3 = ref Enriched.V.empty in
    let es3 = G.create () in
    let load x = (vs3 := Enriched.V.add x !vs3 ; G.add_vertex es3 x) in

    let already_propagated = ref Enriched.V.empty in

    let rec propagate ~add_current_vertex y =
      if not (Enriched.V.mem y !already_propagated) then
        let () = already_propagated := Enriched.V.add y !already_propagated in
        let () = if add_current_vertex then load y in
        G.iter_pred_e (fun (x,m2,_) ->
          let add_zero1 = Enriched.V.mem x vs1 in
          let add_zero2 = Enriched.V.mem x vs2 in
          let m1 = Enriched.E.drain m2 in
          let m3 =
            if x <> y (*do the arrows of the bundle carry halflines or circles?*)
            then Enriched.E.HalfLine.past_extension ~add_last_connected_component:true ~add_zero1 ~add_zero2 m1 m2
            else Enriched.E.Circle.past_extension   ~add_last_connected_component:true ~add_zero1 ~add_zero2 m1 m2 in
          let () = union_edge_e es3 x y m3 in
          if add_zero2 && Enriched.E.some (fun _ ar -> Sh.contains_zero ar) m3
          then propagate ~add_current_vertex:true x) es2 y in

    (*initialize*)
    let () = G.iter_vertex (fun y ->
      let add_last_connected_component = Enriched.V.mem y vs1 in
      let () = if add_last_connected_component then propagate ~add_current_vertex:(Enriched.V.mem y vs2) y in
      G.iter_pred_e (fun (x,m1,_) ->
        let add_zero1 = Enriched.V.mem x vs1 in
        let add_zero2 = Enriched.V.mem x vs2 in
        let (_ , m2 , _) = (*get the corresponding bundle of arrows of es2*)
          try G.find_edge es2 x y
          with Not_found -> x , Enriched.E.empty , y in
        let m3 =
          if x <> y (*do the arrows of the bundle carry halflines or circles?*)
          then Enriched.E.HalfLine.past_extension ~add_zero1 ~add_zero2 ~add_last_connected_component m1 m2
          else Enriched.E.Circle.past_extension   ~add_zero1 ~add_zero2 ~add_last_connected_component m1 m2 in
        let () = union_edge_e es3 x y m3 in
        if add_zero2 && Enriched.E.some (fun _ ar -> HL.glb ar = Sh.zero) m1
        then propagate ~add_current_vertex:true x) es1 y) es1 in

    let () = Enriched.V.iter (fun x -> propagate ~add_current_vertex:(Enriched.V.mem x vs2) x) vs1 in

    gather !vs3 es3


  let past_closure { vertices = vs ; edges = es } =
    let vs' = ref vs in
    let es' = G.create () in
    let () = G.iter_edges_e (fun (x , m , y) ->
      let m' , add_x = if x <> y then Enriched.E.HalfLine.past_closure m else Enriched.E.Circle.past_closure m in
      if add_x then vs' := Enriched.V.add x !vs' ;
      replace_edge_e es' x y m') es in
    gather !vs' es'

  (* highlighting functions: these functions are used to determine the
     buzy area of processes*)

  let forward_highlight rm_entering entering_points add_exiting exiting_points g =
    let answer = ref (future_extension entering_points (difference g exiting_points))
    in
    if rm_entering then answer := difference !answer entering_points ;
    if add_exiting then answer := union      !answer exiting_points  ;
    !answer

  let backward_highlight add_entering entering_points rm_exiting exiting_points g =
    let answer = ref (past_extension exiting_points (difference g entering_points))
    in
    if add_entering then answer := union      !answer entering_points ;
    if rm_exiting   then answer := difference !answer exiting_points  ;
    !answer

  (* Classical examples *)

  let circle v e ar = draw_edges_list [v,e,ar,v,false]

  let half_line v v' e ar = draw_edges_list [v,e,ar,v',false]

  let directed_cube
      ?(v000="O")
      ?(v001="C")
      ?(v010="B")
      ?(v100="A")
      ?(v011="F")
      ?(v101="E")
      ?(v110="D")
      ?(v111="I")
      ?(e000_001="")
      ?(e000_010="")
      ?(e000_100="")
      ?(e100_110="")
      ?(e100_101="")
      ?(e010_110="")
      ?(e010_011="")
      ?(e001_011="")
      ?(e001_101="")
      ?(e101_111="")
      ?(e011_111="")
      ?(e110_111="")
      ar = failwith "directed cube NIY [Dgrog]"

  let fill_directed_cube ?filler ()  = failwith "fill directed cube NIY [Dgrog]"

  let closed_half_line v v' e ar = draw_edges_list [v,e,ar,v',true]

  let star ?(input=[]) ?(output=[]) b v = draw_edges_list
    (
      List.append
	(List.map (fun (x,s,ar) -> x,s,ar,v,b) input)
	(List.map (fun (s,ar,y) -> v,s,ar,y,b) output)
    )

end (*Make*)


(* TODO ?
  Rewrite avoiding the use of maps.
  Also define Rplus without zero to avoid the problem of normalization.*)
