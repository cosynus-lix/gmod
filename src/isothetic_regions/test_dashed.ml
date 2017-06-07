module Shared = 
struct
  module I = Interval.Make(Half_line.Integer)
  module A = Arc.Make(Half_line.Integer)
  module C = Cube.Make(I)
  module T = Tore.Make(A)
  module AC = Area.Make.OverCube(C)
  module AT = Area.Make.OverTore(T)
  module Sh = ODA.Shared(Half_line.Integer)
  module HL = ODA.HalfLine(Half_line.Integer)
  module Ci = ODA.Circle(Half_line.Integer)

(* let () = Printf.printf "%i\n" (Sh.compare [Sh.Cls 0] [Sh.Cls 0;Sh.Pun 1]) *)

  let rec make_tests_list vl = 
    match vl with
      | [v] -> [[Sh.Cls v];[Sh.Opn v];[Sh.Iso v];[Sh.Pun v]]
      | v::vl' -> 
	let sequel = make_tests_list vl' in
	List.concat
	  [
	    (List.fold_left (fun accu tail -> (Sh.Cls v::tail)::accu) [] sequel);
	    (List.fold_left (fun accu tail -> (Sh.Opn v::tail)::accu) [] sequel);
	    (List.fold_left (fun accu tail -> (Sh.Iso v::tail)::accu) [] sequel);
	    (List.fold_left (fun accu tail -> (Sh.Pun v::tail)::accu) [] sequel);
	  ]
      | [] -> []

  let rec make_tests_list_Opn_only vl = 
    match vl with
      | [v] -> [[Sh.Opn v]]
      | v::vl' -> let sequel = make_tests_list_Opn_only vl' in
		  List.concat
		    [
		      (List.fold_left (fun accu tail -> (Sh.Opn v::tail)::accu) [] sequel)
		    ]
      | [] -> []

  let rec make_tests_list_Cls_only vl = 
    match vl with
      | [v] -> [[Sh.Cls v]]
      | v::vl' -> let sequel = make_tests_list_Cls_only vl' in
		  List.concat
		    [
		      (List.fold_left (fun accu tail -> (Sh.Cls v::tail)::accu) [] sequel)
		    ]
      | [] -> []

  let rec make_tests_list_Iso_only vl = 
    match vl with
      | [v] -> [[Sh.Iso v]]
      | v::vl' -> let sequel = make_tests_list_Iso_only vl' in
		  List.concat
		    [
		      (List.fold_left (fun accu tail -> (Sh.Iso v::tail)::accu) [] sequel)
		    ]
      | [] -> []

  let rec make_tests_list_Pun_only vl = 
    match vl with
      | [v] -> [[if v<>HL.zero then Sh.Pun v else Sh.Cls HL.zero]]
      | v::vl' -> let sequel = make_tests_list_Pun_only vl' in
		  List.concat
		    [
		      (List.fold_left (fun accu tail -> ((if v<>HL.zero then Sh.Pun v else Sh.Cls HL.zero)::tail)::accu) [] sequel)
		    ]
      | [] -> []


  let all_tests = 
    [|
      List.filter (Sh.is_valid) (make_tests_list []);
      List.filter (Sh.is_valid) (make_tests_list [0]);
      List.filter (Sh.is_valid) (make_tests_list [1]);
      List.filter (Sh.is_valid) (make_tests_list [2]);
      List.filter (Sh.is_valid) (make_tests_list [3]);
      List.filter (Sh.is_valid) (make_tests_list [4]);
      List.filter (Sh.is_valid) (make_tests_list [0;1]);
      List.filter (Sh.is_valid) (make_tests_list [0;2]);
      List.filter (Sh.is_valid) (make_tests_list [0;3]);
      List.filter (Sh.is_valid) (make_tests_list [0;4]);
      List.filter (Sh.is_valid) (make_tests_list [1;2]);
      List.filter (Sh.is_valid) (make_tests_list [1;3]);
      List.filter (Sh.is_valid) (make_tests_list [1;4]);
      List.filter (Sh.is_valid) (make_tests_list [2;3]);
      List.filter (Sh.is_valid) (make_tests_list [2;4]);
      List.filter (Sh.is_valid) (make_tests_list [3;4]);
      List.filter (Sh.is_valid) (make_tests_list [0;1;2]);
      List.filter (Sh.is_valid) (make_tests_list [0;1;3]);
      List.filter (Sh.is_valid) (make_tests_list [0;1;4]);
      List.filter (Sh.is_valid) (make_tests_list [0;2;3]);
      List.filter (Sh.is_valid) (make_tests_list [0;2;4]);
      List.filter (Sh.is_valid) (make_tests_list [0;3;4]);
      List.filter (Sh.is_valid) (make_tests_list [1;2;3]);
      List.filter (Sh.is_valid) (make_tests_list [1;2;4]);
      List.filter (Sh.is_valid) (make_tests_list [1;3;4]);
      List.filter (Sh.is_valid) (make_tests_list [2;3;4]);
      List.filter (Sh.is_valid) (make_tests_list [0;1;2;3]);
      List.filter (Sh.is_valid) (make_tests_list [0;1;2;4]);
      List.filter (Sh.is_valid) (make_tests_list [0;1;3;4]);
      List.filter (Sh.is_valid) (make_tests_list [0;2;3;4]);
      List.filter (Sh.is_valid) (make_tests_list [1;2;3;4]);
      List.filter (Sh.is_valid) (make_tests_list [0;1;2;3;4])
    |]


  let all_tests_maker maker = 
    [|
      List.filter (Sh.is_valid) (maker []);
      List.filter (Sh.is_valid) (maker [0]);
      List.filter (Sh.is_valid) (maker [1]);
      List.filter (Sh.is_valid) (maker [2]);
      List.filter (Sh.is_valid) (maker [3]);
      List.filter (Sh.is_valid) (maker [4]);
      List.filter (Sh.is_valid) (maker [0;1]);
      List.filter (Sh.is_valid) (maker [0;2]);
      List.filter (Sh.is_valid) (maker [0;3]);
      List.filter (Sh.is_valid) (maker [0;4]);
      List.filter (Sh.is_valid) (maker [1;2]);
      List.filter (Sh.is_valid) (maker [1;3]);
      List.filter (Sh.is_valid) (maker [1;4]);
      List.filter (Sh.is_valid) (maker [2;3]);
      List.filter (Sh.is_valid) (maker [2;4]);
      List.filter (Sh.is_valid) (maker [3;4]);
      List.filter (Sh.is_valid) (maker [0;1;2]);
      List.filter (Sh.is_valid) (maker [0;1;3]);
      List.filter (Sh.is_valid) (maker [0;1;4]);
      List.filter (Sh.is_valid) (maker [0;2;3]);
      List.filter (Sh.is_valid) (maker [0;2;4]);
      List.filter (Sh.is_valid) (maker [0;3;4]);
      List.filter (Sh.is_valid) (maker [1;2;3]);
      List.filter (Sh.is_valid) (maker [1;2;4]);
      List.filter (Sh.is_valid) (maker [1;3;4]);
      List.filter (Sh.is_valid) (maker [2;3;4]);
      List.filter (Sh.is_valid) (maker [0;1;2;3]);
      List.filter (Sh.is_valid) (maker [0;1;2;4]);
      List.filter (Sh.is_valid) (maker [0;1;3;4]);
      List.filter (Sh.is_valid) (maker [0;2;3;4]);
      List.filter (Sh.is_valid) (maker [1;2;3;4]);
      List.filter (Sh.is_valid) (maker [0;1;2;3;4])
    |]

  let all_tests_Opn_only = all_tests_maker make_tests_list_Opn_only

  let all_tests_Cls_only = all_tests_maker make_tests_list_Cls_only

  let all_tests_Iso_only = all_tests_maker make_tests_list_Iso_only

  let all_tests_Pun_only = all_tests_maker make_tests_list_Pun_only

  let test n = 
    if n<0 || 31<n
    then
      failwith "Test.ml: test: n has to be taken between 0 and 31"
    else
      Array.get all_tests n


  let test_Opn_only n = 
    if n<0 || 31<n
    then
      failwith "Test.ml: test_Opn_only: n has to be taken between 0 and 31"
    else
      Array.get all_tests_Opn_only n


  let test_Cls_only n = 
    if n<0 || 31<n
    then
      failwith "Test.ml: test_Cls_only: n has to be taken between 0 and 31"
    else
      Array.get all_tests_Cls_only n

  let test_Iso_only n = 
    if n<0 || 31<n
    then
      failwith "Test.ml: test_Iso_only: n has to be taken between 0 and 31"
    else
      Array.get all_tests_Iso_only n

  let test_Pun_only n = 
    if n<0 || 31<n
    then
      failwith "Test.ml: test_Pun_only: n has to be taken between 0 and 15"
    else
      Array.get all_tests_Pun_only n

end

module HalfLine =
struct
  include Shared

  let hlp i = print_endline (HL.string_of i)

(* convert HL.t to AC.t *)

  let to_area a =
    let rec to_area ?p a = match a with
      | Sh.Opn y::a ->
	(
	  match p with 
	    | Some Sh.Cls x -> (C.bounded true  false x y)::to_area a 
	    | Some Sh.Opn x
	    | Some Sh.Pun x -> (C.bounded false false x y)::to_area a 
	    | None -> to_area ~p:(Sh.Opn y) a
	    | _ -> failwith "test.ml: to_area: invalid form 1"
	)
      | Sh.Cls y::a ->
	( 
	  match p with 
	    | Some Sh.Cls x -> (C.bounded true  true x y)::to_area a 
	    | Some Sh.Opn x
	    | Some Sh.Pun x -> (C.bounded false true x y)::to_area a 
	    | None -> to_area ~p:(Sh.Cls y) a
	    | _ -> failwith "test.ml: to_area: invalid form 2"
	)
      | Sh.Iso y::a -> (C.of_list [I.atom y])::to_area a
      | Sh.Pun y::a -> 
	(
	  match p with
	    | Some Sh.Cls x -> (C.bounded true  false x y)::to_area ~p:(Sh.Pun y) a 
	    | Some Sh.Opn x
	    | Some Sh.Pun x -> (C.bounded false false x y)::to_area ~p:(Sh.Pun y) a
	    | _ -> failwith "test.ml: to_area: invalid form 3"
	)
      | [] ->
	(
	  match p with
	    | Some Sh.Cls x -> [C.terminal true x]
	    | Some Sh.Opn x
 	    | Some Sh.Pun x -> [C.terminal false x]
	    | None -> []
	    | _ -> failwith "test.ml: to_area: invalid form 4"
	)
    in
    try
      if a <> []
      then AC.of_list (to_area a)
      else AC.empty ~d:1 ()
    with
      | Failure _ -> failwith (Sh.string_of a)

  (* The lists of a given length come first *)

  let next_oda ?(check_validity=true) ?upper_bound ?max_length a = (* next on the list *)
    let max_length = match max_length with
      | Some x -> if x<List.length a then raise Exit else x
      | None   -> List.length a
    in
    let upper_bound = match upper_bound with
      | Some x -> if x<max_length-1 then raise Exit else x
      | None   -> max_length-1
    in
    let rec next_oda a =
      let osff = ref false in (*osff stands for "one step forward found"*) 
      match a with
	| b::a' -> (*The first value should not be Pun x so it has to be treated separately*)
	  let b' = match b with (*The first value is stored in order to force osff updating*)
	    | Sh.Opn x -> osff := true ; Sh.Cls x
	    | Sh.Cls x -> osff := true ; Sh.Iso x
	    | Sh.Iso x -> Sh.Opn x
	    | _     -> failwith "Invalid form detected [Test.next_oda]"
	  in
	  let answer =
	    b'::List.map 
	      (
		fun b -> 
		  if !osff
		  then b 
		  else 
		    match b with 
		      | Sh.Pun x -> Sh.Opn x 
		      | Sh.Opn x -> osff := true ; Sh.Cls x
		      | Sh.Cls x -> osff := true ; Sh.Iso x
		      | Sh.Iso x -> osff := true ; Sh.Pun x
	      ) a'
	  in
	  (
	    if !osff
	    then 
	      if Sh.is_valid answer 
	      then answer 
	      else next_oda answer
	    else raise Exit
	  )
	| _ -> raise Exit
    in
    try
      next_oda a (*First exhaust all combination of bound types...(1)*)
    with
      | Exit -> (*(1)...then exhaust all possible integer values...(2)*)
	(
	  try
	    let a = Algebra.Combinator.next (List.map Sh.rvb a) upper_bound in
	    List.map (fun x -> Sh.Opn x) a
	  with
	    | Exit ->
	      (
		let next_level = (List.length a)+1 in
		if next_level > max_length
		then raise Exit (*Can't go any further*)
		else (*(2)...and finally extend the list*)
		  List.map 
		    (fun x -> Sh.Opn x) 
		    (Globals.segment_initial_croissant next_level)
	      )
	)

  let show_them_all_up_to ?upper_bound len =
    try
      let upb = match upper_bound with
	| Some upb -> 
	  if upb < len-1 
	  then (print_endline "Nothing to show" ; raise Exit) 
	  else upb
	| None -> len-1
      in
      let rec show_them_all a = 
	print_endline (Sh.string_of a);
	show_them_all (next_oda ~upper_bound:upb ~max_length:len a)
      in
      try show_them_all (List.map (fun x -> Sh.Opn x) [])
      with | Exit -> ()
    with
      | Exit -> ()

  let counter = ref 0

  let niy_counter = ref 0

  let confront prototype oracle a1 a2 =
    incr counter;
    try
      let prototype_return = prototype a1 a2 in
      let area_prototype_return = to_area (prototype_return) in
      let oracle_return = oracle (to_area a1) (to_area a2) in
      if AC.compare area_prototype_return oracle_return <> 0
      then
	(
	  Printf.printf "Argument 1\n [%s]\nArgument 2\n [%s]\nThe prototype returns\n [%s]\nwhile the oracle returns\n %s\n"
	    (Sh.string_of a1)
	    (Sh.string_of a2)
	    (Sh.string_of prototype_return)
	    (AC.string_of oracle_return)
	  ;
	  raise Exit
	)
    with 
      | Failure "binary_boolean_operator NIY [Oda]" -> incr niy_counter

  let confront_exists prototype oracle a1 a2 =
    incr counter;
    try
      let prototype_return = prototype a1 a2 in
      let oracle_return = Sh.is_not_empty (oracle a1 a2) in
      if prototype_return <> oracle_return 
      then
	(
	  Printf.printf "Argument 1\n [%s]\nArgument 2\n [%s]\nThe prototype returns\n %b\nwhile the oracle returns\n %b\n"
	    (Sh.string_of a1)
	    (Sh.string_of a2)
	    prototype_return
	    oracle_return
	  ;
	  raise Exit
	)
    with 
      | Failure "binary_boolean_operator NIY [Oda]" -> incr niy_counter

  let confront_for_all prototype oracle a1 a2 =
    incr counter;
    try
      let prototype_return = prototype a1 a2 in
      let oracle_return = Sh.is_full (oracle a1 a2) in
      if prototype_return <> oracle_return 
      then
	(
	  Printf.printf "Argument 1\n [%s]\nArgument 2\n [%s]\nThe prototype returns\n %b\nwhile the oracle returns\n %b\n"
	    (Sh.string_of a1)
	    (Sh.string_of a2)
	    prototype_return
	    oracle_return
	  ;
	  raise Exit
	)
    with 
      | Failure "binary_boolean_operator NIY [Oda]" -> incr niy_counter

  let binary_comparator ?(upper_bound=4) ?(max_length=5) prototype oracle =
    counter := 0 ; niy_counter := 0 ;
    let rec go a1 a2 =
      (
	try confront prototype oracle a1 a2
	with 
	  | Failure _ ->
	    failwith (Printf.sprintf "a1 = [%s]\na2 = [%s]\n"
			(Sh.string_of a1)
			(Sh.string_of a2))
      )
      ;
      let a1,a2 = 
	try a1,next_oda ~upper_bound ~max_length a2 
	with 
	  | Exit -> next_oda ~upper_bound ~max_length a1,[]
      in
      go a1 a2
    in
    try go [] []
    with 
      | Exit -> 
	Printf.printf "%i effective tests\n%i dump case crashes\n" !counter !niy_counter

  let binary_comparator_exists ?(upper_bound=4) ?(max_length=5) prototype oracle =
    counter := 0 ; niy_counter := 0 ;
    let rec go a1 a2 =
      (
	try confront_exists prototype oracle a1 a2
	with 
	  | Failure _ ->
	    failwith (Printf.sprintf "a1 = [%s]\na2 = [%s]\n"
			(Sh.string_of a1)
			(Sh.string_of a2))
      )
      ;
      let a1,a2 = 
	try a1,next_oda ~upper_bound ~max_length a2 
	with 
	  | Exit -> next_oda ~upper_bound ~max_length a1,[]
      in
      go a1 a2
    in
    try go [] []
    with 
      | Exit -> 
	Printf.printf "%i effective tests\n%i dump case crashes\n" !counter !niy_counter

  let switch = false

  let switch_exists = false

  let switch_for_all = false

  let switch_compare = false

  module BuiltFromAC =
  struct
  let symmetric_difference = 
    fun a1 a2 -> AC.difference (AC.normalize (AC.join a1 a2)) (AC.meet a1 a2)
  let intersection = AC.meet
  let union = fun a1 a2 -> AC.normalize (AC.join a1 a2)
  let complement_of_union = fun a1 a2 -> AC.complement (AC.join a1 a2)
  let complement_of_intersection = fun a1 a2 -> AC.complement (AC.meet a1 a2)
  let same = fun a1 a2 -> AC.complement (symmetric_difference a1 a2)
  let difference = AC.difference
  let less_or_equal = fun a1 a2 -> AC.complement (AC.difference a1 a2)
  let greater_or_equal = fun a1 a2 -> AC.complement (AC.difference a2 a1)
  let strictly_less_than = fun a1 a2 -> AC.difference a2 a1
  let strictly_greater_than = fun a1 a2 -> AC.difference a1 a2
  let binary_full:(AC.t -> AC.t -> AC.t) = fun a1 a2 -> AC.full ~d:1 ()
  let binary_empty:(AC.t -> AC.t -> AC.t) = fun a1 a2 -> AC.empty ~d:1 ()
  let proj1 = fun a1 a2 -> a1
  let proj2 = fun a1 a2 -> a2
  let not_proj1 = fun a1 a2 -> AC.complement a1
  let not_proj2 = fun a1 a2 -> AC.complement a2
  end

  let proj1 = fun b1 b2 -> b1
  let proj2 = fun b1 b2 -> b2
  let not_proj1 = fun b1 b2 -> not b1
  let not_proj2 = fun b1 b2 -> not b2

  let xor = fun b1 b2 -> match b1,b2 with 
    | true,true | false,false -> false 
    | _ -> true

  let less_or_equal = fun b1 b2 -> b1 <= b2

  let greater_or_equal = fun b1 b2 -> b2 <= b1

  let difference = fun b1 b2 -> b1 > b2

  let strictly_less_than = (<)

  let strictly_greater_than = (>)

  let binary_true = fun b1 b2 -> true

  let binary_false = fun b1 b2 -> false

  let not_or = fun b1 b2 -> not (b1||b2)

  let not_and = fun b1 b2 -> not (b1&&b2)

  let all_operators =
    [
      "⋂",Sh.binary_boolean_operator (&&),BuiltFromAC.intersection;
      "¬⋂",Sh.binary_boolean_operator not_and,BuiltFromAC.complement_of_intersection;
      "⋃",Sh.binary_boolean_operator (||),BuiltFromAC.union;
      "¬⋃",Sh.binary_boolean_operator not_or,BuiltFromAC.complement_of_union;
      "Δ (symmetric difference/parity 1)",Sh.binary_boolean_operator xor,BuiltFromAC.symmetric_difference;
      "∇ a.k.a. ¬Δ (parity 0)",Sh.binary_boolean_operator (=),BuiltFromAC.same;
      "⩽ a.k.a. =>",Sh.binary_boolean_operator less_or_equal,BuiltFromAC.less_or_equal;
      "⩾ a.k.a. <=",Sh.binary_boolean_operator greater_or_equal,BuiltFromAC.greater_or_equal;
      "> a.k.a. ≠> a.k.a. \\" ,Sh.binary_boolean_operator strictly_greater_than,BuiltFromAC.strictly_greater_than;
      "< a.k.a <≠",Sh.binary_boolean_operator strictly_less_than,BuiltFromAC.strictly_less_than;
      "π₁",Sh.binary_boolean_operator proj1,BuiltFromAC.proj1;
      "π₂",Sh.binary_boolean_operator proj2,BuiltFromAC.proj2;
      "¬π₁",Sh.binary_boolean_operator not_proj1,BuiltFromAC.not_proj1;
      "¬π₂",Sh.binary_boolean_operator not_proj2,BuiltFromAC.not_proj2;
      "⊥ a.k.a. Ø",Sh.binary_boolean_operator binary_false,BuiltFromAC.binary_empty;
      "⊤",Sh.binary_boolean_operator binary_true,BuiltFromAC.binary_full;
    ]

  let all_operators_exists =
    [
      "⋂",Sh.exists (&&),Sh.binary_boolean_operator (&&);
      "¬⋂",Sh.exists not_and,Sh.binary_boolean_operator not_and;
      "⋃",Sh.exists (||),Sh.binary_boolean_operator (||);
      "¬⋃",Sh.exists not_or,Sh.binary_boolean_operator not_or;
      "Δ (symmetric difference/parity 1)",Sh.exists xor,Sh.binary_boolean_operator xor;
      "∇ a.k.a. ¬Δ (parity 0)",Sh.exists (=),Sh.binary_boolean_operator (=);
      "⩽ a.k.a. =>",Sh.exists less_or_equal,Sh.binary_boolean_operator less_or_equal;
      "⩾ a.k.a. <=",Sh.exists greater_or_equal,Sh.binary_boolean_operator greater_or_equal;
      "> a.k.a. ≠> a.k.a. \\" ,Sh.exists strictly_greater_than,Sh.binary_boolean_operator strictly_greater_than;
      "< a.k.a <≠",Sh.exists strictly_less_than,Sh.binary_boolean_operator strictly_less_than;
      "π₁",Sh.exists proj1,Sh.binary_boolean_operator proj1;
      "π₂",Sh.exists proj2,Sh.binary_boolean_operator proj2;
      "¬π₁",Sh.exists not_proj1,Sh.binary_boolean_operator not_proj1;
      "¬π₂",Sh.exists not_proj2,Sh.binary_boolean_operator not_proj2;
      "⊥ a.k.a. Ø",Sh.exists binary_false,Sh.binary_boolean_operator binary_false;
      "⊤",Sh.exists binary_true,Sh.binary_boolean_operator binary_true;
    ]

  let others = 
    [
    ]

  let prototype = Sh.binary_boolean_operator less_or_equal

  let oracle = BuiltFromAC.less_or_equal

  let argument1 = [Sh.Opn 0;Sh.Opn 1;Sh.Iso 3]

  let argument2 = [Sh.Opn 0;Sh.Cls 2]

  let () =
    if switch
    then
      List.iter
	(
	  fun (symbol,prototype,oracle) ->
	    (
	      Printf.printf "Testing the %s operator\n%!" symbol; 
	      binary_comparator prototype oracle
	    )
	) all_operators

  let () =
    if switch_exists
    then
      List.iter
	(
	  fun (symbol,prototype,oracle) ->
	    (
	      Printf.printf "Testing the %s existence test\n%!" symbol; 
	      binary_comparator_exists prototype oracle
	    )
	) all_operators_exists


  let () =
    if switch_for_all
    then
      List.iter
	(
	  fun (symbol,prototype,oracle) ->
	    (
	      Printf.printf "Testing the %s universality test\n%!" symbol; 
	      binary_comparator_exists prototype oracle
	    )
	) all_operators_exists

  (*Testing the comparison function from oda.ml*)

  let sgn x = 
    if x<0
    then -1 
    else 
      if x>0
      then 1
      else 0

  let test_compare ?(verbose=false) ar1 ar2 =
    let prototype = Sh.compare ar1 ar2 in
    let oracle1 = Sh.difference ar1 ar2 in
    let oracle2 = Sh.difference ar2 ar1 in
    let oracle = 
      if Sh.is_empty oracle1
      then
	if Sh.is_empty oracle2
	then 0
	else -1
      else
	if Sh.is_empty oracle2
	then 1
	else
	  let x1 = HL.glb oracle1 in
	  let x2 = HL.glb oracle2 in
	  if x1<x2
	  then 1
	  else 
	    if x2<x1
	    then -1
	    else
	      if Sh.belongs_to x1 oracle1 
	      then  1
	      else -1
    in
    if sgn prototype <> sgn oracle || verbose
    then
      (
	if not verbose
	then
	  (
	    Printf.printf 
	      "ar1 = [%s]
ar2 = [%s]
oracle1 = [%s]
oracle2 = [%s]
the Oracle returns %i
while the Prototype returns %i
"
	      (Sh.string_of ar1)
	      (Sh.string_of ar2)
	      (Sh.string_of oracle1) 
	      (Sh.string_of oracle2)
	      oracle
	      prototype
	    ;
	    failwith "Error"
	  )
	else
	  Printf.printf 
	    "compare [%s] [%s] = %i
"
	    (Sh.string_of ar1)
	    (Sh.string_of ar2)
	    prototype

      )


  let () = 
    if false (*not switch_compare*)
    then 
      test_compare [Sh.Opn 0] [Sh.Cls 0]

  let full_testing_compare ?(upper_bound=4) ?(max_length=5) () = 
    print_endline "Testing ODA.compare";
    let rec go a1 a2 =
      (
	try test_compare a1 a2
	with 
	  | Failure _ -> failwith "Error found"
      )
      ;
      let a1,a2 = 
	try a1,next_oda ~upper_bound ~max_length a2 
	with 
	  | Exit -> next_oda ~upper_bound ~max_length a1,[]
      in
      go a1 a2
    in
    try go [] []
    with 
      | Exit -> print_endline "All tests successfully passed."

  let () = 
    if switch_compare
    then
      full_testing_compare ()


  let () =
    if false (*not switch*)
    then
      try confront prototype oracle argument1 argument2
      with | Failure f -> (print_endline ("Failure "^f)) | Exit -> (print_endline "Exit")

  let future_extension a1 a2 = 
    AC.normalize 
      (
	AC.fold 
	  (
	    fun c1 accu1 ->
	      AC.fold 
		(
		  fun c2 accu2 ->
		    AC.add (C.in_the_future_of c1 c2) accu2
		)
		a2
		accu1
	  ) 
	  a1
	  (AC.empty ~d:1 ())
      )

  let past_extension a1 a2 = 
    AC.normalize 
      (
	AC.fold 
	  (
	    fun c1 accu1 ->
	      AC.fold 
		(
		  fun c2 accu2 ->
		    AC.add (C.in_the_past_of c1 c2) accu2
		)
		a2
		accu1
	  ) 
	  a1
	  (AC.empty ~d:1 ())
      )


  let is_valid ar = print_endline (string_of_bool (Sh.is_valid ar))


  let testing_union i j =
    let counter = ref 0 in
    Printf.printf "Start testing %i (%i) %i (%i).\n" i (List.length (test i)) j (List.length (test j)) ;
    List.iter
      (
	fun op1 ->
	  (
	    List.iter
	      (
		fun op2 -> incr counter;
		  let answer1 = HL.union op1 op2 in
		  let answer2 = AC.normalize (AC.join (to_area op1) (to_area op2)) in
		  if not((AC.is_included (to_area answer1) answer2)&&(AC.is_included answer2 (to_area answer1)))
		  then
		    (
		      Printf.printf "%i:\n op1=%s\n%s\n op2=%s\n%s\n union is \n%s which differs from\n%s\n\n"
			!counter
			(HL.string_of op1)
			(AC.string_of (to_area op1))
			(HL.string_of op2)
			(AC.string_of (to_area op2))
			(HL.string_of answer1)
			(AC.string_of answer2)
		    )
		  else
		  (*Printf.printf "%i\n" !counter*)()
	      )
	  ) (test i)
      ) (test j)

  let testing_intersection i j =
    let counter = ref 0 in
    Printf.printf "Start testing %i (%i) %i (%i).\n" i (List.length (test i)) j (List.length (test j)) ;
    List.iter
      (
	fun op1 ->
	  (
	    List.iter
	      (
		fun op2 -> incr counter;
		  let answer1 = HL.intersection op1 op2 in
		  let answer2 = AC.normalize (AC.meet (to_area op1) (to_area op2)) in
		  if not((AC.is_included (to_area answer1) answer2)&&(AC.is_included answer2 (to_area answer1)))
		  then
		    (
		      Printf.printf "%i:\n op1=%s\n%s\n op2=%s\n%s\n intersection is \n%s which differs from\n%s\n\n"
			!counter
			(HL.string_of op1)
			(AC.string_of (to_area op1))
			(HL.string_of op2)
			(AC.string_of (to_area op2))
			(HL.string_of answer1)
			(AC.string_of answer2)
		    )
		  else
		  (*Printf.printf "%i\n" !counter*)()
	      )
	  ) (test i)
      ) (test j)


  let testing_difference i j =
    let counter = ref 0 in
    Printf.printf "Start testing %i (%i) %i (%i).\n" i (List.length (test i)) j (List.length (test j)) ;
    List.iter
      (
	fun op1 ->
	  (
	    List.iter
	      (
		fun op2 -> incr counter;
		  let answer1 = HL.difference op1 op2 in
		  let answer2 = AC.normalize (AC.difference (to_area op1) (to_area op2)) in
		  if not((AC.is_included (to_area answer1) answer2)&&(AC.is_included answer2 (to_area answer1)))
		  then
		    (
		      Printf.printf "%i:\n op1=%s\n%s\n op2=%s\n%s\n difference is \n%s which differs from\n%s\n\n"
			!counter
			(HL.string_of op1)
			(AC.string_of (to_area op1))
			(HL.string_of op2)
			(AC.string_of (to_area op2))
			(HL.string_of answer1)
			(AC.string_of answer2)
		    )
		  else
		  (*Printf.printf "%i\n" !counter*)()
	      )
	  ) (test i)
      ) (test j)


  let testing_future_extension i j =
    try
      let counter = ref 0 in
      Printf.printf "Start testing %i (%i) %i (%i).\n" i (List.length (test i)) j (List.length (test j)) ;
      List.iter
	(
	  fun op1 ->
	    (
	      List.iter
		(
		  fun op2 -> incr counter;
		    let answer1 = HL.future_extension op1 op2 in
		    let answer2 = future_extension (to_area op1) (to_area op2) in
		    if (try not((AC.is_included (to_area answer1) answer2)&&(AC.is_included answer2 (to_area answer1))) with | _ -> true)
		    then
		      (
			Printf.printf "%i:\n op1=%s\n%s\n op2=%s\n%s\n future_extension is \n%s which differs from\n%s\n\n"
			  !counter
			  (HL.string_of op1)
			  (AC.string_of (to_area op1))
			  (HL.string_of op2)
			  (AC.string_of (to_area op2))
			  (HL.string_of answer1)
			  (AC.string_of answer2)
		      )
		    else
		  (*Printf.printf "%i\n" !counter*)()
		)
	    ) (test i) 
	) (test j)
    with
      | Failure s -> print_endline s


  let testing_past_extension i j =
    try
      let counter = ref 0 in
      Printf.printf "Start testing %i (%i) %i (%i).\n" i (List.length (test i)) j (List.length (test j)) ;
      List.iter
	(
	  fun op1 ->
	    (
	      List.iter
		(
		  fun op2 -> incr counter;
		    (
		      try
			let answer1 = HL.past_extension op1 op2 in
			let answer2 = past_extension (to_area op1) (to_area op2) in
			if (try not((AC.is_included (to_area answer1) answer2)&&(AC.is_included answer2 (to_area answer1))) with | Failure "Surprise" -> (print_endline "Aïe";true) | _ -> true)
			then
			  (
			    Printf.printf "%i:\n op1=%s    %s\n op2=%s    %s\npast_extension false (false,[%s]) (false,[%s])\nreturns %s which differs from\n%s\n\n"
			      !counter
			      (HL.string_of op1)
			      (AC.string_of (to_area op1))
			      (HL.string_of op2)
			      (AC.string_of (to_area op2))

(Sh.string_of op1)
(Sh.string_of op2)


			      (HL.string_of answer1)
			      (AC.string_of answer2)
			  )
			else
		  (*Printf.printf "%i\n" !counter*)()
		      with
			| Failure "NIY past_extension" -> ()
		    )
		)
	    ) (test i)
	) (test j)
    with
      | Failure s -> print_endline s

	
  let testing_is_included i j =
    let counter = ref 0 in
    Printf.printf "Start testing %i (%i) %i (%i).\n" i (List.length (test i)) j (List.length (test j)) ;
    List.iter
      (
	fun op1 ->
	  (
	    List.iter
	      (
		fun op2 -> incr counter;
		  let answer1 = HL.is_included op1 op2 in
		  let answer2 = AC.is_included (to_area op1) (to_area op2) in
		  if answer1 <> answer2
		  then
		    (
		      Printf.printf "HL claims that %s\n%s included in\n%s\nbut A says it is not so\n"
			(HL.string_of op1)
			(if answer1 then "is" else "is not")
			(HL.string_of op2)
		    )
		  else
		  (*Printf.printf "%i\n" !counter*)()
	      )
	  ) (test i)
      ) (test j)






(* TODO *)

  let testing_belongs_to i =
    let counter = ref 0 in
    Printf.printf "Start testing %i (%i).\n" i (List.length (test i)) ;
    List.iter
      (
	fun pt1 ->
	  (
	    List.iter
	      (
		fun op2 -> incr counter;
		  let answer1 = HL.belongs_to pt1 op2 in
		  let answer2 = AC.belongs_to [|pt1|] (to_area op2) in
		  if answer1 <> answer2
		  then
		    (
		      Printf.printf "HL claims that %i\nbelongs to %s\nbut A says it is not so\n"
			pt1
			(HL.string_of op2)
		    )
		  else
		  (*Printf.printf "%i\n" !counter*)()
	      )
	  ) (test i)
      ) (Array.to_list (Array.init 6 (fun n -> n)))


  let testing_future_extension_unbounded_flag i j =
    let counter = ref 0 in
    let flag = ref false in
    Printf.printf "Start testing %i (%i) %i (%i).\n" i (List.length (test i)) j (List.length (test j)) ;
    List.iter
      (
	fun op1 ->
	  (
	    List.iter
	      (
		fun op2 -> incr counter;
		  let result = HL.future_extension ~flag op1 op2 in
		  let test = HL.is_not_bounded result || HL.is_not_bounded op1 in
		  if !flag <> test
		  then
		    (
		      Printf.printf "op1 = %s\nop2 = %s\nThe flag raised by future_extension claims that %s or %s\nis %s but the function is_not_bounded asserts otherwise\n"
			(Sh.string_of op1)
			(Sh.string_of op2)
			(Sh.string_of op1)
			(Sh.string_of result)
			(if !flag then "not bounded" else "bounded")
		    )
		  else
		  (*Printf.printf "%i\n" !counter*)()
	      )
	  ) (test i)
      ) (test j)

  let testing_complement i =
    let counter = ref 0 in
    Printf.printf "Start testing %i (%i).\n" i (List.length (test i)) ;
    List.iter
      (
	fun op -> incr counter;
	  let answer1 = HL.complement op in
	  let answer2 = AC.complement (to_area op) in
	  if not((AC.is_included (to_area answer1) answer2)&&(AC.is_included answer2 (to_area answer1)))
	  then
	    (
	      Printf.printf "%i:\n op=%s\n%s\n complement is \n%s which differs from\n%s\n\n"
		!counter
		(HL.string_of op)
		(AC.string_of (to_area op))
		(HL.string_of answer1)
		(AC.string_of answer2)
	    )
	  else
		  (*Printf.printf "%i\n" !counter*)()
      )
      (test i)

  let testing_interior i =
    let counter = ref 0 in
    Printf.printf "Start testing %i (%i).\n" i (List.length (test i)) ;
    List.iter
      (
	fun op -> incr counter;
	  let answer1 = HL.interior op in
	  let answer2 = AC.normalize(AC.interior (to_area op)) in
	  if not((AC.is_included (to_area answer1) answer2)&&(AC.is_included answer2 (to_area answer1)))
	  then
	    (
	      Printf.printf "%i:\n op=%s\n%s\n interior is \n%s which differs from\n%s\n\n"
		!counter
		(HL.string_of op)
		(AC.string_of (to_area op))
		(HL.string_of answer1)
		(AC.string_of answer2)
	    )
	  else
		  (*Printf.printf "%i\n" !counter*)()
      )
      (test i)

  let testing_closure i =
    let counter = ref 0 in
    Printf.printf "Start testing %i (%i).\n" i (List.length (test i)) ;
    List.iter
      (
	fun op -> incr counter;
	  let answer1 = HL.closure op in
	  let answer2 = AC.normalize(AC.closure (to_area op)) in
	  if not((AC.is_included (to_area answer1) answer2)&&(AC.is_included answer2 (to_area answer1)))
	  then
	    (
	      Printf.printf "%i:\n op=%s\n%s\n closure is \n%s which differs from\n%s\n\n"
		!counter
		(HL.string_of op)
		(AC.string_of (to_area op))
		(HL.string_of answer1)
		(AC.string_of answer2)
	    )
	  else
		  (*Printf.printf "%i\n" !counter*)()
      )
      (test i)


  let testing_boundary i =
    let counter = ref 0 in
    Printf.printf "Start testing %i (%i).\n" i (List.length (test i)) ;
    List.iter
      (
	fun op -> incr counter;
	  let answer1 = HL.boundary op in
	  let answer2 = HL.difference (HL.closure op) (HL.interior op) in
	  (*	  let answer2 = AC.normalize(AC.boundary (to_area op)) in*)
	  if answer1 <> answer2
	  then
	    (
	      Printf.printf "%i:\n op=%s\n boundary is \n%s which differs from\n%s\n\n"
		!counter
		(HL.string_of op)
		(HL.string_of answer1)
		(HL.string_of answer2)
	    )
	  else
		  (*Printf.printf "%i\n" !counter*)()
      )
      (test i)


  let testing_to_area i =
    let counter = ref 0 in
    Printf.printf "Start testing %i (%i).\n" i (List.length (test i)) ;
    List.iter
      (
	fun op -> incr counter;
	  let answer1 = HL.string_of op in
	  let answer2 = (AC.string_of (to_area op)) in
	  Printf.printf "%s\n%s\n----------------\n" answer1 answer2
      )
      (test i)



  let full_testing_union () = 
    for i=0 to 15
    do
      for j=0 to 15
      do
	testing_union i j
      done
    done

  let full_testing_intersection () = 
    for i=0 to 15
    do
      for j=0 to 15
      do
	testing_intersection i j
      done
    done

  let full_testing_difference () = 
    for i=0 to 15
    do
      for j=0 to 15
      do
	testing_difference i j
      done
    done

  let full_testing_future_extension () =
    print_endline "Testing future_extension (halfline mode) begin";
    for i=0 to 31
    do
      for j=0 to 31
      do
	testing_future_extension i j
      done
    done;
    print_endline "Testing future_extension (halfline mode) end"

  let full_testing_future_extension_unbounded_flag () = 
    for i=0 to 15
    do
      for j=0 to 31
      do
	testing_future_extension_unbounded_flag i j
      done
    done

  let full_testing_past_extension () = 
    print_endline "Testing past_extension (halfline mode) begin";
    for i=0 to 31
    do
      for j=0 to 31
      do
	testing_past_extension i j
      done
    done;
    print_endline "Testing past_extension (halfline mode) end"

  let full_testing_is_included () = 
    for i=0 to 31
    do
      for j=0 to 31
      do
	testing_is_included i j
      done
    done

  let full_testing_belongs_to () = 
    for i=0 to 15
    do
      testing_belongs_to i
    done

  let full_testing_complement () = 
    for i=0 to 15
    do
      testing_complement i
    done

  let full_testing_interior () = 
    for i=0 to 15
    do
      testing_interior i
    done

  let full_testing_closure () = 
    for i=0 to 15
    do
      testing_closure i
    done

  let full_testing_boundary () = 
    for i=0 to 15
    do
      testing_boundary i
    done


  let full_testing_to_area () = 
    for i=0 to 15
    do
      testing_to_area i
    done

  let test_em_all do_it =
    if do_it
    then
      (
	print_endline "\nTesting complement\n";
	full_testing_complement () ;
	print_endline "\nTesting intersection\n";
	full_testing_intersection () ;
	print_endline "\nTesting difference\n";
	full_testing_difference () ;
	print_endline "\nTesting future_extension\n";
	full_testing_future_extension () ;
	print_endline "\nTesting interior\n";
	full_testing_interior () ;
	print_endline "\nTesting closure\n";
	full_testing_closure ();
	print_endline "\nTesting boundary\n";
	full_testing_boundary ();
	print_endline "\nTesting union\n";
	full_testing_union () ;
	print_endline "\nTesting is_included\n";
	full_testing_is_included () ;
      )

end(*HalfLine*)

module Circle =
struct
  include Shared

(* TODO: convert Ci.t to AT.t *)

(* to be tested *)

  let to_area a =
    let last_bound = ref None in
    let rec to_area ?p a = 
      match a with
	| Sh.Opn y::a -> 
	  (
	    match p with 
	      | Some Sh.Cls x -> (A.bounded true  false x y)::to_area a 
	      | Some Sh.Opn x
	      | Some Sh.Pun x -> (A.bounded false false x y)::to_area a 
	      | None -> to_area ~p:(Sh.Opn y) a
	      | _ -> failwith "test.ml: Ci.to_area: invalid form 1"
	  )
	| Sh.Cls y::a -> 
	  (
	    match p with 
	      | Some Sh.Cls x -> (A.bounded true  true x y)::to_area a 
	      | Some Sh.Opn x
	      | Some Sh.Pun x -> (A.bounded false true x y)::to_area a 
	      | None -> to_area ~p:(Sh.Cls y) a
	      | _ -> failwith "test.ml: Ci.to_area: invalid form 2"
	  )
	| Sh.Iso y::a -> (A.atom y)::to_area a
	| Sh.Pun y::a -> 
	  (
	    match p with
	      | Some Sh.Cls x -> (A.bounded true  false x y)::to_area ~p:(Sh.Pun y) a 
	      | Some Sh.Opn x
	      | Some Sh.Pun x -> (A.bounded false false x y)::to_area ~p:(Sh.Pun y) a
	      | _ -> failwith "test.ml: to_area: invalid form 3"
	  )
	| [] -> last_bound := p ; []
    in
    let answer = match a with
      | Sh.Iso x::a ->
	let answer = to_area a in
	(
	  match !last_bound with
	    | Some Sh.Cls y ->
	      (
		if x <> Ci.zero
		then
		  A.bounded true false y Ci.zero::(A.atom x)::answer
		else
		  A.bounded true true y Ci.zero::answer
	      )
	    | Some Sh.Pun y
	    | Some Sh.Opn y ->
	      (
		if x <> Ci.zero
		then
		  A.bounded false false y Ci.zero::A.atom x::answer
		else
		  A.bounded false true y Ci.zero::answer
	      )
	    | None -> A.atom x::answer
	    | _ -> failwith "The remaining last bound cannot be a singleton"
 	)

      | [Sh.Cls x] -> [A.terminal true x] (*the test x<>Ci.zero is actually performed by the function terminal*)
	(*if x<>Ci.zero
	then [A.bounded true false x Ci.zero]
	else [A.full]*)

      | [Sh.Cls x;Sh.Pun y] ->
	if x<>Ci.zero
	then
	  [A.terminal false y;A.bounded true false x y]
	else
	  [A.coatom y]

      | Sh.Cls x::a -> 
	let answer = to_area ~p:(Sh.Cls x)  a in
	(
	  match !last_bound with
	    | Some Sh.Cls y ->
	      (
		if x <> Ci.zero
		then
		  A.bounded true false y Ci.zero::answer
		else
		  (
		    A.dummy_union 
		      (A.bounded true true y Ci.zero) 
		      (List.hd answer)
		  )::(List.tl answer)
	      )
	    | Some Sh.Pun y
	    | Some Sh.Opn y ->
	      (
		if x <> Ci.zero
		then
		  A.bounded false false y Ci.zero::answer
		else
		  (
		    A.dummy_union 
		      (A.bounded false true y Ci.zero) 
		      (List.hd answer)
		  )::(List.tl answer)
	      )
	    | None -> answer
	    | _ -> failwith "The remaining last bound cannot be a singleton"
	)
      | [Sh.Opn x] -> [A.terminal false x] (*the test x<>Ci.zero is actually performed by the function terminal*)
	(*if x<>Ci.zero 
	then [A.terminal false x] (*[A.bounded false false x Ci.zero]*)
	else [A.coatom Ci.zero]*)
      | Sh.Opn x::a -> 
	let answer = to_area ~p:(Sh.Opn x)  a in
	(
	  match !last_bound with
	    | Some Sh.Cls y -> A.bounded true false y Ci.zero::answer
	    | Some Sh.Pun y
	    | Some Sh.Opn y -> A.bounded false false y Ci.zero::answer
	    | None -> answer
	    | _ -> failwith "The remaining last bound cannot be a singleton"
	)
      | [] -> [A.empty]
      (* dump *)
      | _ -> failwith "test.ml:Circle.to_area NIY"
    in
    List.fold_left 
      (fun accu b -> AT.add b accu) 
      (AT.empty ~d:1 ()) 
      (List.map (fun arc -> T.of_arc arc) answer) 

  let to_area_naive a =
    let rec to_area ?p a = match a with
      | Sh.Opn y::a ->
	(
	  match p with 
	    | Some Sh.Cls x -> (A.bounded true  false x y)::to_area a 
	    | Some Sh.Opn x
	    | Some Sh.Pun x -> (A.bounded false false x y)::to_area a 
	    | None -> to_area ~p:(Sh.Opn y) a
	    | _ -> failwith "test.ml: to_area: invalid form 1"
	)
      | Sh.Cls y::a ->
	( 
	  match p with 
	    | Some Sh.Cls x -> (A.bounded true  true x y)::to_area a 
	    | Some Sh.Opn x
	    | Some Sh.Pun x -> (A.bounded false true x y)::to_area a 
	    | None -> to_area ~p:(Sh.Cls y) a
	    | _ -> failwith "test.ml: to_area: invalid form 2"
	)
      | Sh.Iso y::a -> A.atom y::to_area a
      | Sh.Pun y::a -> 
	(
	  match p with
	    | Some Sh.Cls x -> (A.bounded true  false x y)::to_area ~p:(Sh.Pun y) a 
	    | Some Sh.Opn x
	    | Some Sh.Pun x -> (A.bounded false false x y)::to_area ~p:(Sh.Pun y) a
	    | _ -> failwith "test.ml: to_area: invalid form 3"
	)
      | [] ->
	(
	  match p with
	    | Some Sh.Cls x -> [A.terminal true x]
	    | Some Sh.Opn x
 	    | Some Sh.Pun x -> [A.terminal false x]
	    | None -> []
	    | _ -> failwith "test.ml: to_area: invalid form 4"
	)
    in
    if a <> [] 
    then AT.normalize 
      (
	List.fold_left 
	  (fun accu b -> AT.add b accu) 
	  (AT.empty ~d:1 ()) 
	  (List.map (fun arc -> T.of_arc arc) (to_area a)) 
      )
    else AT.empty ~d:1 ()

  let testing_string_of i =
    let counter = ref 0 in
    Printf.printf "Start testing %i (%i).\n" i (List.length (test i)) ;
    List.iter
      (
	fun op -> incr counter;
	  (
	    Printf.printf "%i:\n HL %s\n Ci %s\n\n"
	      !counter
	      (HL.string_of op)
	      (Ci.string_of op)
	  )
      )
      (test i)

  let full_testing_string_of () = 
    for i=0 to 15
    do
      testing_string_of i
    done

  let testing_to_area i =
    let counter = ref 0 in
    Printf.printf "Start testing %i (%i).\n" i (List.length (test i)) ;
    List.iter
      (
	fun op -> incr counter;
	  let answer1 = to_area op in
	  let answer2 = to_area_naive op in
	  if not((AT.is_included answer1 answer2)&&(AT.is_included answer2 answer1))
	  then
	    Printf.printf "%i:\n op=%s\nto_area\n%s\nto_area_naive\n%s\n\n"
	      !counter
	      (Ci.string_of op)
	      (AT.string_of (AT.remove_useless_empty_brick answer1))
	      (AT.string_of (AT.remove_useless_empty_brick answer2))
      )
      (test i)

  let testing_interior i =
    let counter = ref 0 in
    Printf.printf "Start testing %i (%i).\n" i (List.length (test i)) ;
    List.iter
      (
	fun op -> incr counter;
	  let answer1 = Ci.interior op in
	  let answer2 = AT.normalize(AT.interior (to_area op)) in
	  if not((AT.is_included (to_area answer1) answer2)&&(AT.is_included answer2 (to_area answer1)))
	  then
	    (
	      Printf.printf "%i:\n op=%s\n%s\n interior is \n%s which differs from\n%s\n\n"
		!counter
		(Ci.string_of op)
		(AT.string_of (to_area op))
		(Ci.string_of answer1)
		(AT.string_of answer2)
	    )
      )
      (test i)

  let testing_closure i =
    let counter = ref 0 in
    Printf.printf "Start testing %i (%i).\n" i (List.length (test i)) ;
    List.iter
      (
	fun op -> incr counter;
	  let answer1 = Ci.closure op in
	  let answer2 = AT.normalize(AT.closure (to_area op)) in
	  if not((AT.is_included (to_area answer1) answer2)&&(AT.is_included answer2 (to_area answer1)))
	  then
	    (
	      Printf.printf "%i:\n op=%s\n%s\n closure is \n%s which differs from\n%s\n\n"
		!counter
		(Ci.string_of op)
		(AT.string_of (to_area op))
		(Ci.string_of answer1)
		(AT.string_of answer2)
	    )
	  else
		  (*Printf.printf "%i\n" !counter*)()
      )
      (test i)

  let full_testing_to_area () = 
    for i=0 to 31
    do
      testing_to_area i
    done

  let full_testing_interior () = 
    for i=0 to 15
    do
      testing_interior i
    done

  let full_testing_closure () = 
    for i=0 to 15
    do
      testing_closure i
    done

  let future_extension a1 a2 =
    AT.normalize
      (
	AT.fold 
	  (
	    fun c1 accu1 ->
	      AT.fold 
		(
		  fun c2 accu2 -> 
		    AT.add (T.in_the_future_of c1 c2) accu2
		)
		a2
		accu1
	  )
	  a1
	  (AT.empty ~d:1 ())
      )

  let past_extension a1 a2 = 
    AT.normalize 
      (
	AT.fold 
	  (
	    fun c1 accu1 ->
	      AT.fold 
		(
		  fun c2 accu2 ->
		    AT.add (T.in_the_past_of c1 c2) accu2
		)
		a2
		accu1
	  ) 
	  a1
	  (AT.empty ~d:1 ())
      )

  let testing_future_extension i j =
    try
      let counter = ref 0 in
      Printf.printf "Start testing %i (%i) %i (%i).\n" i (List.length (test i)) j (List.length (test j)) ;
      List.iter
	(
	  fun op1 ->
	    (
	      List.iter
		(
		  fun op2 -> incr counter;
		    let answer1 = Ci.future_extension op1 op2 in
		    let answer2 = future_extension (to_area op1) (to_area op2) in
		    if (try not((AT.is_included (to_area answer1) answer2)&&(AT.is_included answer2 (to_area answer1))) with | _ -> true)
		    then
		      (
			Printf.printf "%i:\n op1=%s\n%s\n op2=%s\n%s\n future_extension is \n%s which differs from\n%s\nfuture_extension (circle mode) false (false,[%s]) (false,[%s])\n\n"
			  !counter
			  (Sh.string_of op1)
			  (AT.string_of (AT.remove_useless_empty_brick (to_area op1)))
			  (Sh.string_of op2)
			  (AT.string_of (AT.remove_useless_empty_brick (to_area op2)))
			  (Sh.string_of answer1)
			  (AT.string_of answer2)
			  (Sh.string_of op1)
			  (Sh.string_of op2)
		      )
		)
	    ) (test i) 
	) (test j)
    with
      | Failure s -> print_endline s


  let testing_past_extension i j =
    try
      let counter = ref 0 in
      Printf.printf "Start testing %i (%i) %i (%i).\n" i (List.length (test i)) j (List.length (test j)) ;
      List.iter
	(
	  fun op1 ->
	    (
	      List.iter
		(
		  fun op2 -> incr counter;
		    (
		      try
			let answer1 = Ci.past_extension op1 op2 in
			let answer2 = past_extension (to_area op1) (to_area op2) in
			if (try not((AT.is_included (to_area answer1) answer2)&&(AT.is_included answer2 (to_area answer1))) with | Failure "Surprise" -> (print_endline "Aïe";true) | _ -> true)
			then
			  (
			    Printf.printf "%i:\n op1=%s    %s\n op2=%s    %s\n past_extension (%s mode) returns %s which differs from %s\npast_extension false (false,[%s]) (false,[%s])\n\n"
			      !counter
			      (Sh.string_of op1)
			      (AT.string_of (AT.remove_useless_empty_brick (to_area op1)))
			      (Sh.string_of op2)
			      (AT.string_of (AT.remove_useless_empty_brick (to_area op2)))
			      (if Sh.unbounded_connected_component_must_be_added op1 op2 
			       then "circle" 
			       else "halfline")
			      (Sh.string_of answer1)
			      (AT.string_of answer2)
			      (Sh.string_of op1)
			      (Sh.string_of op2)
			  )
		      with
			| Failure "NIY past_extension" -> (print_endline "Not treated")
		    )
		)
	    ) (test i)
	) (test j)
    with
      | Failure s -> print_endline s

  let full_testing_future_extension () =
    print_endline "Testing future_extension (circle mode) begin";
    for i=0 to 31
    do
      for j=0 to 31
      do
	testing_future_extension i j
      done
    done;
    print_endline "Testing future_extension (circle mode) end"


  let full_testing_past_extension () = 
    print_endline "Testing past_extension (circle mode) begin";
    for i=0 to 31
    do
      for j=0 to 31
      do
	testing_past_extension i j
      done
    done;
    print_endline "Testing past_extension (circle mode) end"


end(*Circle*)

module OverInteger=
struct

  let test_string_of strings_to_test = 
    List.iter
      (
	fun s ->
	  try
	    let s = BuiltInWithoutParsers.ODA.OverInteger.of_string s in
	    print_endline (BuiltInWithoutParsers.ODA.OverInteger.Sh.string_of s) ;
	    print_endline (BuiltInWithoutParsers.ODA.OverInteger.HL.string_of s) ;
	    print_endline (BuiltInWithoutParsers.ODA.OverInteger.Ci.string_of s) ;
	    print_endline ""
	  with
	    | Failure s' -> print_string (Printf.sprintf "malformed expression: %s\n" s')
      )
      strings_to_test

end(*OverInteger*)

