open BuiltInWithoutParsers
open ODA.OverInteger
module E = Expression

let list_of_variables:string list ref = ref []

type mixed_return =
  | Bool of bool
  | Int  of int
  | Dgrog of DGROG.t

let string_of_mixed_return ?verbose ?pretty mr = match mr with
  | Dgrog g -> "\n"^(DGROG.string_of ~shift:2 ?verbose ?pretty g)^"\n"
  | Bool x  -> Printf.sprintf " %b\n" x
  | Int x   -> Printf.sprintf " %i\n" x

let variable answer id =
  match id with
    | "true"  -> Bool true
    | "false" -> Bool false
    | _ ->
      match E.find id answer with
	| Dgrog t -> Dgrog (DGROG.copy t)
	| x -> x
	(*| _ -> failwith "Type Mismatch [Sheet_solver_dgrog]"*)

let constant t = Dgrog t

let intersection v1 v2 =
  match v1,v2 with
    | Dgrog t1,Dgrog t2 -> Dgrog (DGROG.intersection t1 t2)
    | Bool b1,Bool b2 -> Bool (b1 && b2)
    | _ -> failwith "Type Mismatch [Sheet_solver_dgrog.intersection]"

let union v1 v2 =
  match v1,v2 with
    | Dgrog t1,Dgrog t2 -> Dgrog (DGROG.union t1 t2)
    | Bool b1,Bool b2 -> Bool (b1 || b2)
    | _ -> failwith "Type Mismatch [Sheet_solver_dgrog.union]"

let difference v1 v2 =
  match v1,v2 with
    | Dgrog t1,Dgrog t2 -> Dgrog (DGROG.difference t1 t2)
    | Bool b1,Bool b2   -> Bool (b1 > b2)
    | _ -> failwith "Type Mismatch [Sheet_solver_dgrog.difference]"

let compare v1 v2 =
  match v1,v2 with
    | Dgrog t1,Dgrog t2 -> Int (DGROG.compare t1 t2)
    | Bool b1,Bool b2   -> Int (Pervasives.compare b1 b2)
    | Int n1,Int n2     -> Int (Pervasives.compare n1 n2)
    | _ -> failwith "Type Mismatch [Sheet_solver_dgrog.difference]"

let less_or_equal v1 v2 =
  match v1,v2 with
    | Dgrog t1,Dgrog t2 -> Bool (DGROG.compare t1 t2 <= 0)
    | Bool b1,Bool b2   -> Bool (b1 <= b2)
    | _ -> failwith "Type Mismatch [Sheet_solver_dgrog.difference]"

let greater_or_equal v1 v2 =
  match v1,v2 with
    | Dgrog t1,Dgrog t2 -> Bool (DGROG.compare t1 t2 >= 0)
    | Bool b1,Bool b2   -> Bool (b1 >= b2)
    | _ -> failwith "Type Mismatch [Sheet_solver_dgrog.difference]"

let strictly_less v1 v2 =
  match v1,v2 with
    | Dgrog t1,Dgrog t2 -> Bool (DGROG.compare t1 t2 < 0)
    | Bool b1,Bool b2   -> Bool (b1 < b2)
    | _ -> failwith "Type Mismatch [Sheet_solver_dgrog.difference]"

let strictly_greater v1 v2 =
  match v1,v2 with
    | Dgrog t1,Dgrog t2 -> Bool (DGROG.compare t1 t2 > 0)
    | Bool b1,Bool b2   -> Bool (b1 > b2)
    | _ -> failwith "Type Mismatch [Sheet_solver_dgrog.difference]"

let complement v =
  match v with
    | Dgrog t -> Dgrog (DGROG.complement t)
    | Bool b  -> Bool (not b)
    | _ -> failwith "Type Mismatch [Sheet_solver_dgrog.complement]"

let future_closure v =
  match v with
    | Dgrog t -> Dgrog (DGROG.future_closure t)
    | _ -> failwith "Type Mismatch [Sheet_solver_dgrog.future_closure]"

let past_closure v =
  match v with
    | Dgrog t -> Dgrog (DGROG.past_closure t)
    | _ -> failwith "Type Mismatch [Sheet_solver_dgrog.past_closure]"

let closure v =
  match v with
    | Dgrog t -> Dgrog (DGROG.closure t)
    | _ -> failwith "Type Mismatch [Sheet_solver_dgrog.closure]"

let interior v =
  match v with
    | Dgrog t -> Dgrog (DGROG.interior t)
    | _ -> failwith "Type Mismatch [Sheet_solver_dgrog.interior]"

let future v1 v2 =
  match v1,v2 with
    | Dgrog t1,Dgrog t2 -> Dgrog (DGROG.future_extension t1 t2)
    | _ -> failwith "Type Mismatch [Sheet_solver_dgrog.future]"

let past v1 v2 =
  match v1,v2 with
    | Dgrog t1,Dgrog t2 -> Dgrog (DGROG.past_extension t1 t2)
    | _ -> failwith "Type Mismatch [Sheet_solver_dgrog.past]"

let calculation t answer =
  let rec calculation t = match t with
    | E.DGROG.Variable s              -> variable answer s
    | E.DGROG.Constant t              -> constant t
    | E.DGROG.Intersection(t1,t2)     -> intersection     (calculation t1) (calculation t2)
    | E.DGROG.Union(t1,t2)            -> union            (calculation t1) (calculation t2)
    | E.DGROG.Complement(t)           -> complement       (calculation t)
    | E.DGROG.FutureClosure(t)        -> future_closure   (calculation t)
    | E.DGROG.PastClosure(t)          -> past_closure     (calculation t)
    | E.DGROG.Difference(t1,t2)       -> difference       (calculation t1) (calculation t2)
    | E.DGROG.Closure t               -> closure          (calculation t)
    | E.DGROG.Interior t              -> interior         (calculation t)
    | E.DGROG.Future(t1,t2)           -> future           (calculation t1) (calculation t2)
    | E.DGROG.Past(t1,t2)             -> past             (calculation t1) (calculation t2)
    | E.DGROG.Compare(t1,t2)          -> compare          (calculation t1) (calculation t2)
    | E.DGROG.Less_or_equal(t1,t2)    -> less_or_equal    (calculation t1) (calculation t2)
    | E.DGROG.Greater_or_equal(t1,t2) -> greater_or_equal (calculation t1) (calculation t2)
    | E.DGROG.Strictly_less(t1,t2)    -> strictly_less    (calculation t1) (calculation t2)
    | E.DGROG.Strictly_greater(t1,t2) -> strictly_greater (calculation t1) (calculation t2)
  in
  calculation t

let from_string s =
  let () = list_of_variables := [] in
  let (domain,moct) =
    try
      Parser_dgrog_sheet.output Lexer_dgrog_sheet.entry_point (Lexing.from_string s)
    with
      | Parsing.Parse_error -> failwith (Printf.sprintf "%s to parse the given string [Dgrog]" Common.Terminal.(color Red ~bold:true "Unable"))
  in
  let () = list_of_variables := domain in (*side-effect preserving the order in which variable are defined*)
  List.fold_left
    (
      fun accu name -> E.add name (calculation (E.find name moct) accu) accu
    ) E.empty domain

let from_filename p =
  let () = list_of_variables := [] in
  let (domain,moct) =
    try
      let c = open_in p in
      let aux = Parser_dgrog_sheet.output Lexer_dgrog_sheet.entry_point (Lexing.from_channel c) in
      close_in c;aux
    with
      | Sys_error _ -> failwith (Printf.sprintf "%s unable to open the file %s." Message.error Common.Terminal.(color Blue ~bold:true p))
      | Parsing.Parse_error -> failwith (Printf.sprintf "%s unable to parse the content of file\n%S" Message.error p)
  in
  let () = list_of_variables := domain in (*side-effect preserving the order in which variable are defined*)
  List.fold_left (fun accu name -> E.add name (calculation (E.find name moct) accu) accu) E.empty domain

let agreement = ref true

let confront ?(pretty=true) ?(verbose=false) m1 m2 =
  agreement := true ;
  (
    try
      E.iter
	(
	  fun key ar1 ->
	    try
	      let ar2 = (E.find key m2) in
	      let () = (*side effect normalization*)
		DGROG.coherent_form ar1 ;
		DGROG.coherent_form ar2 ;
	      in
	      (*if ar1 <> ar2 then*)
	      let ar1_subset_of_ar2 = DGROG.is_included ar1 ar2 in
	      let ar2_subset_of_ar1 = DGROG.is_included ar2 ar1 in
	      if not ar1_subset_of_ar2 || not ar2_subset_of_ar1
	      then
		(
		  (*Printf.printf "\nar1%sar2\n"
		    (if ar1_subset_of_ar2 then " is included in " else " is not included in ");
		    Printf.printf "ar2%sar1\n"
		    (if ar2_subset_of_ar1 then " is included in " else " is not included in ");*)
		  Printf.printf "Value of variable %S mismatch:\n%s\n%s%s\n%s\n"
		    key
		    Common.Terminal.(color Red "The library Dgrog returns")
		    (DGROG.string_of ~verbose:false ~pretty:true ar1)
		    Common.Terminal.(color Red "but the Oracle expects")
		    (DGROG.string_of ~verbose:false ~pretty:true ar2)
		  ;
		  agreement := false ;
		  raise Exit
		)
	    with
	      | Not_found ->
		agreement := false ;
		Printf.printf "Variable %s defined in first map not in the second one\n"
		  Common.Terminal.(color Red key);
		raise Exit
	) m1
    with
      | Exit -> ()
  )

let print ?only ?(alphabetical=false) ?(pretty=true) ?(verbose=false) m =
  match only with
    | None ->
      if alphabetical
      then
	E.iter
	  (
	    fun key x -> Printf.printf "%s =%s"
	      Common.Terminal.(color Blue ~bold:true key)
	      (string_of_mixed_return ~verbose ~pretty x)
	  )
	  m
      else
	List.iter
	  (
	    fun key ->
	      try
		Printf.printf "%s =%s" Common.Terminal.(color Blue ~bold:true key)
		  (string_of_mixed_return (E.find key m))
	      with
		| Not_found -> Common.Terminal.(Printf.printf "%s the variable %s is not defined\nShould not happened\n" (color Red ~bold:true "Error:") (color Blue ~bold:true key))
	  )
	  !list_of_variables
    | Some key_list ->
      List.iter
	(
	  fun key ->
	    try
	      Printf.printf "%s =%s" Common.Terminal.(color Blue ~bold:true key)
		(string_of_mixed_return ~verbose ~pretty (E.find key m))
	    with
	      | Not_found -> Common.Terminal.(Printf.printf "%s the variable %s is not defined\n" (color Yellow ~bold:true "Warning:") (color Blue ~bold:true key))
	)
	key_list

let string_of ?(active=false) ?only ?(alphabetical=false) ?(pretty=true) ?(verbose=false) m =
  match only with
    | None ->
      if alphabetical
      then
	E.fold (fun key x accu -> Printf.sprintf "%s%s =\n%s" accu Common.Terminal.(color Blue ~active ~bold:true key) (string_of_mixed_return ~verbose ~pretty x)) m ""
      else
	List.fold_left
	  (
	    fun accu key ->
	      try
		Printf.sprintf "%s%s =\n%s" accu Common.Terminal.(color Blue ~active ~bold:true key) (string_of_mixed_return ~verbose ~pretty (E.find key m))
	      with
		| Not_found -> Common.Terminal.(Printf.sprintf "%s the variable %s is not defined\nShould not happened\n" (color Red ~active ~bold:true "Error:") (color Blue ~active ~bold:true key) (*TO BE IMPROVED*))
	  )
	  ""
	  !list_of_variables
    | Some key_list ->
      List.fold_left
	(
	  fun accu key ->
	    try
	      Printf.sprintf "%s%s =\n%s" accu Common.Terminal.(color Blue ~active ~bold:true key) (string_of_mixed_return ~verbose ~pretty (E.find key m))
	    with
	      | Not_found -> Common.Terminal.(Printf.sprintf "%s the variable %s is not defined\n" (color Yellow ~active ~bold:true "Warning:") (color Blue ~active ~bold:true key))
	)
	""
	key_list


