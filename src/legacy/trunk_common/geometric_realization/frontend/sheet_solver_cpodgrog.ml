open BuiltInWithoutParsers
open ODA.OverInteger
module E = Expression

let list_of_variables = ref []

type prime =
  | Natural_numbers of (int*int) list
  | Coordinates of bool array list

type mixed_return = 
  | Bool of bool
  | Int  of int
  | Cpodgrog of Area.t
  | Decompose of prime

(*let string_of_boolean_array ba = "{"^(String.concat "," (Algebra.BooleanArray.string_list_of ba))^"}"*)

let rec string_of_mixed_return ?(shift="  ") ?verbose ?pretty mr = match mr with
  | Cpodgrog g  -> "\n"^(Area.string_of ?verbose ?pretty g)
  | Bool x      -> Printf.sprintf " %b\n" x
  | Int x       -> Printf.sprintf " %i\n" x
  | Decompose x -> match x with
    | Natural_numbers x -> Algebra.Integer.string_of_factorization ~utf8:true x
    | Coordinates x  -> (String.concat " " (List.rev_map (fun sp -> Algebra.BooleanArray.string_of_support ~left:"[" ~separator:"⋅" ~right:"]" sp) x))^"\n"


let variable answer id = 
  match id with
    | "true"  -> Bool true
    | "false" -> Bool false
    | _ -> E.find id answer

let constant t = Cpodgrog t

let integer n = Int n

let intersection v1 v2 =
  match v1,v2 with
    | Cpodgrog t1,Cpodgrog t2 -> Cpodgrog (Area.intersection t1 t2)
    | Bool b1,Bool b2 -> Bool (b1 && b2)
    | _ -> failwith "Type Mismatch [Sheet_solver_cpodgrog.intersection]"

let union v1 v2 =
  match v1,v2 with
    | Cpodgrog t1,Cpodgrog t2 -> Cpodgrog (Area.union t1 t2)
    | Bool b1,Bool b2 -> Bool (b1 || b2)
    | _ -> failwith "Type Mismatch [Sheet_solver_cpodgrog.union]"

let difference v1 v2 =
  match v1,v2 with
    | Cpodgrog t1,Cpodgrog t2 -> Cpodgrog (Area.difference t1 t2)
    | Bool b1,Bool b2   -> Bool (b1 > b2)
    | _ -> failwith "Type Mismatch [Sheet_solver_cpodgrog.difference]"

let compare v1 v2 =
  match v1,v2 with
    | Cpodgrog t1,Cpodgrog t2 -> Int (Area.compare t1 t2)
    | Bool b1,Bool b2   -> Int (Pervasives.compare b1 b2)
    | Int n1,Int n2     -> Int (Pervasives.compare n1 n2)
    | _ -> failwith "Type Mismatch [Sheet_solver_cpodgrog.difference]"

let less_or_equal v1 v2 =
  match v1,v2 with
    | Cpodgrog t1,Cpodgrog t2 -> Bool (Area.compare t1 t2 <= 0)
    | Bool b1,Bool b2   -> Bool (b1 <= b2)
    | _ -> failwith "Type Mismatch [Sheet_solver_cpodgrog.difference]"

let greater_or_equal v1 v2 =
  match v1,v2 with
    | Cpodgrog t1,Cpodgrog t2 -> Bool (Area.compare t1 t2 >= 0)
    | Bool b1,Bool b2   -> Bool (b1 >= b2)
    | _ -> failwith "Type Mismatch [Sheet_solver_cpodgrog.difference]"

let strictly_less v1 v2 =
  match v1,v2 with
    | Cpodgrog t1,Cpodgrog t2 -> Bool (Area.compare t1 t2 < 0)
    | Bool b1,Bool b2   -> Bool (b1 < b2)
    | _ -> failwith "Type Mismatch [Sheet_solver_cpodgrog.difference]"

let strictly_greater v1 v2 =
  match v1,v2 with
    | Cpodgrog t1,Cpodgrog t2 -> Bool (Area.compare t1 t2 > 0)
    | Bool b1,Bool b2   -> Bool (b1 > b2)
    | _ -> failwith "Type Mismatch [Sheet_solver_cpodgrog.difference]"

let complement v =
  match v with
    | Cpodgrog t -> Cpodgrog (Area.complement t)
    | Bool b  -> Bool (not b)
    | _ -> failwith "Type Mismatch [Sheet_solver_cpodgrog.complement]"

let closure v =
  match v with
    | Cpodgrog t -> Cpodgrog (Area.closure t)
    | _ -> failwith "Type Mismatch [Sheet_solver_cpodgrog.closure]"

let interior v =
  match v with 
    | Cpodgrog t -> Cpodgrog (Area.interior t)
    | _ -> failwith "Type Mismatch [Sheet_solver_cpodgrog.interior]"

let factorize v =
  match v with
    | Cpodgrog t -> Decompose (Coordinates (Area.factorize t))
    | Int t -> Decompose (Natural_numbers (Algebra.Integer.factorization t))
    | _ -> failwith "Type Mismatch [Sheet_solver_cpodgrog.factorize]"

let future v1 v2 =
  match v1,v2 with 
    | Cpodgrog t1,Cpodgrog t2 -> Cpodgrog (Area.future_extension t1 t2)
    | _ -> failwith "Type Mismatch [Sheet_solver_cpodgrog.future]"

let past v1 v2 =
  match v1,v2 with 
    | Cpodgrog t1,Cpodgrog t2 -> Cpodgrog (Area.past_extension t1 t2)
    | _ -> failwith "Type Mismatch [Sheet_solver_cpodgrog.past]"

let product v1 v2 =
  match v1,v2 with
    | Cpodgrog t1,Cpodgrog t2 -> Cpodgrog (Area.product t1 t2)
    | Int t1,Int t2 -> Int (t1 * t2)
    | _ -> failwith "Type Mismatch [Sheet_solver_cpodgrog.past]"

(*————————————————————————————————————————————————————————————————————————————*)

let calculation t answer =
  let rec calculation t = match t with 
    | E.Area.Variable(s)         -> variable answer s
    | E.Area.Int(n)              -> integer n
    | E.Area.Constant(t)         -> constant t
    | E.Area.Intersection(t1,t2) -> intersection (calculation t1) (calculation t2)
    | E.Area.Union(t1,t2)        -> union        (calculation t1) (calculation t2)
    | E.Area.Complement(t)       -> complement   (calculation t)
    | E.Area.Difference(t1,t2)   -> difference   (calculation t1) (calculation t2)
    | E.Area.Closure t           -> closure      (calculation t)
    | E.Area.Interior t          -> interior     (calculation t)
    | E.Area.Future(t1,t2)       -> future       (calculation t1) (calculation t2)
    | E.Area.Past(t1,t2)         -> past         (calculation t1) (calculation t2)
    | E.Area.Compare(t1,t2)      -> compare      (calculation t1) (calculation t2)
    | E.Area.Less_or_equal(t1,t2)    -> less_or_equal    (calculation (t1)) (calculation (t2))
    | E.Area.Greater_or_equal(t1,t2) -> greater_or_equal (calculation (t1)) (calculation (t2))
    | E.Area.Strictly_less(t1,t2)    -> strictly_less    (calculation (t1)) (calculation (t2))
    | E.Area.Strictly_greater(t1,t2) -> strictly_greater (calculation (t1)) (calculation (t2))
    | E.Area.Product(t1,t2) -> product   (calculation t1) (calculation t2)
    | E.Area.Factorize t    -> factorize (calculation t)
 in
  calculation t

let from_string s = 
  let () = list_of_variables := [] in
  let (domain,moct) = 
    try
      Parser_cpodgrog_sheet.output Lexer_cpodgrog_sheet.entry_point (Lexing.from_string s)
    with
      | Parsing.Parse_error -> failwith (Printf.sprintf "%s unable to parse the given string" Message.error)
  in
  let () = list_of_variables := domain in (*side-effect preserving the order in which variable are defined*)
  List.fold_left (fun accu name -> E.add name (calculation (E.find name moct) accu) accu) E.empty domain

let from_filename p =
  let () = list_of_variables := [] in
  let (domain,moct) = 
    try
      let c = open_in p in
      let aux = Parser_cpodgrog_sheet.output Lexer_cpodgrog_sheet.entry_point (Lexing.from_channel c) in
      close_in c;aux
    with
      | Sys_error _ -> failwith (Printf.sprintf "%s unable to open the file %s." Message.error (Message.blue ~bold:true p))
      | Parsing.Parse_error -> failwith (Printf.sprintf "%s unable to parse the given string [Cpodgrog]" Message.error)
  in
  let () = list_of_variables := domain in (*side-effect preserving the order in which variable are defined*)
  List.fold_left 
    (
      fun accu name -> E.add name (calculation (E.find name moct) accu) accu
    ) E.empty domain

let print ?only ?(alphabetical=false) ?(pretty=true) ?(verbose=false) m = 
  (
    match only with
      | None ->
	if alphabetical
	then
	  E.iter 
	    (
	      fun key x -> Printf.printf "%s = %s" 
		(Message.blue ~bold:true key)
		(string_of_mixed_return ~verbose ~pretty x)
	    ) m
	else
	  List.iter
	    (
	      fun key ->
		try
		  Printf.printf "\n%s = %s" (Message.blue ~bold:true key) 
		    (string_of_mixed_return ~verbose ~pretty (E.find key m))
		with
		  | Not_found -> Printf.printf "%s the variable %s is not defined\nShould not happened\n" (Message.red ~bold:true "Error:") (Message.blue ~bold:true key)
	    )
	    !list_of_variables
      | Some key_list ->
	List.iter
	  (
	    fun key ->
	      try
		Printf.printf "%s = %s" (Message.blue ~bold:true key) 
		  (string_of_mixed_return ~verbose ~pretty (E.find key m))
	      with 
		| Not_found -> Printf.printf "%s the variable %s is not defined\n" (Message.yellow ~bold:true "Warning:") (Message.blue ~bold:true key)
	  ) 
	  key_list
  )
  ;
  print_endline "" (*Dirty Patch*)

let string_of ?(active=false) ?only ?(alphabetical=false) ?(pretty=true) ?(verbose=false) m = 
  match only with
    | None -> print_endline "No list of variables";
      if alphabetical
      then
	E.fold 
	  (
	    fun key x accu -> 
	      Printf.sprintf "%s%s =\n%s" 
		accu 
		(Message.blue ~active ~bold:true key) 
		(string_of_mixed_return ~verbose ~pretty x)
	  ) m ""
      else
	List.fold_left
	  (
	    fun accu key -> 
	      try
		Printf.sprintf "%s%s =\n%s"
		  accu 
		  (Message.blue ~active ~bold:true key) 
		  (string_of_mixed_return ~verbose ~pretty (E.find key m))
	      with (*TO BE IMPROVED*)
		| Not_found -> Printf.sprintf "%s the variable %s is not defined\nShould not happened\n" (Message.red ~active ~bold:true "Error:") (Message.blue ~active ~bold:true key)
	  )
	  ""
	  !list_of_variables
    | Some key_list -> 
      List.fold_left
	(
	  fun accu key -> 
	    try
	      Printf.sprintf "%s%s =\n%s" accu (Message.blue ~active ~bold:true key) 
		(string_of_mixed_return ~verbose ~pretty (E.find key m))
	    with
	      | Not_found -> Printf.sprintf "%s the variable %s is not defined\n" (Message.yellow ~active ~bold:true "Warning:") (Message.blue ~active ~bold:true key)
	)
	""
	key_list

