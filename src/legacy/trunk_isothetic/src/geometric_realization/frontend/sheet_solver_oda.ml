open BuiltInWithoutParsers
open ODA.OverInteger
module E = Expression

type mixed_return = 
  | Bool of bool 
  | Int  of int 
  | Area of ((support option)*Sh.t*bool)

(*atoms*)

let variable context s = E.find s context

let infinity support = Area
  (
    support,Sh.empty,match support with
      | Some HalfLine -> 
	(
	  Printf.printf "%s +oo was ignored since it does not belong to the halfline\n%!" 
	    (Message.yellow ~bold:true "Warning") ; false
	)
      | Some Circle -> 
	(
	  Printf.printf "%s 0 and +oo are identified in the circle\n%!" 
	    (Message.yellow ~bold:true "Warning") ; true
	)
      | Some CompactHalfLine -> true
      | None -> failwith "Unspecified support"
  )

let full support = Area
  (
    support,Sh.full,match support with
      | Some HalfLine -> false 
      | Some Circle
      | Some CompactHalfLine -> true
      | None -> failwith "Unspecified support"
  )

let circle support = Area
  (
    support,Sh.full,match support with
      | Some HalfLine -> 
	(
	  Printf.printf 
	    "%s S' is taken as R+ since the support is the halfline\n%!" 
	    (Message.yellow ~bold:true "Warning") ; false
	)
      | Some Circle -> true
      | Some CompactHalfLine -> 
	(
	  Printf.printf (*this convention is not so clear*)
	    "%s S' is taken as R+ (and thus +oo is dropped) since the support is the compact halfline\n%!" 
	    (Message.yellow ~bold:true "Warning") ; true
	)
      | None -> failwith "Unspecified support"
  )

let halfline support = Area
(
support,Sh.full,match support with
  | Some Circle -> 
    (
      Printf.printf 
	"%s R+ is taken as S' since the support is the circle\n%!" 
	(Message.yellow ~bold:true "Warning") ; false
    )
  | Some HalfLine
  | Some CompactHalfLine -> false
  | None -> failwith "Unspecified support"
)

let compacthalfline support = Area
(
support,Sh.full,match support with
  | Some Circle -> 
    (
      Printf.printf 
	"%s KR+ is taken as S' since the support is the circle\n%!" 
	(Message.yellow ~bold:true "Warning") ; false
    )
  | Some HalfLine -> false
  | Some CompactHalfLine -> true
  | None -> failwith "Unspecified support"
)

let atom support (lx,b) = Area
  (
    support,Sh.discrete ~do_sort:true lx,match support with
      | Some HalfLine -> false
      | Some Circle -> 
	let () = 
	  if b then Printf.printf 
	    "%s 0 and +oo are identified in the circle\n%!" 
	    (Message.yellow ~bold:true "Warning")
	in (List.exists (fun x -> x=Sh.zero) lx) || b
      | Some CompactHalfLine -> b (*false*)
      | None -> failwith "Unspecified support"
  )

let coatom support (lx,b) = Area
  (
    support,Sh.codiscrete ~do_sort:true lx,match support with
      | Some HalfLine -> false
      | Some Circle -> List.for_all (fun x -> x<>Sh.zero) lx
      | Some CompactHalfLine -> not b (*true*)
      | None -> failwith "Unspecified support"
  )

let interval support a b x y = 
  Area
    (
      let aux = 
	if x<y
	then Sh.interval a b x y
	else
	  if y<x 
	  then Sh.cointerval a b x y
	  else
	    if a && b
	    then Sh.atom x
	    else Sh.empty
      in 
      (support,aux,(match support with
	| Some HalfLine -> false
	| Some Circle -> Sh.contains_zero aux
	| Some CompactHalfLine -> HL.is_not_bounded aux
	| None -> failwith "Unspecified support"))
    )

let final support b y e = Area
  (
    let aux = Sh.final b y in 
      (
	support,aux,match support with
	  | Some HalfLine -> 
	    (
	      if e 
	      then
		Printf.printf 
		  "%s +oo was dropped since it does not belong to the halfline\n%!" 
		  (Message.yellow ~bold:true "Warning");
	      false
	    )
	  | Some Circle -> 
	    (
	      Printf.printf 
		"%s +oo and 0 are identified in the circle\n%!" 
		(Message.yellow ~bold:true "Warning");
	      (b && y=Sh.zero)||e
	    )
	  | Some CompactHalfLine -> e
	  | None -> failwith "Unspecified support"
      )
  )

(*boolean algebra*)

let union v1 v2 =
  match v1,v2 with 
    | (Area (support1,ar1,b1)),(Area (support2,ar2,b2)) ->
      if support1 = support2
      then Area
	(
	  support1,Sh.union ar1 ar2,match support1 with
	    | Some HalfLine -> false
	    | Some Circle
	    | Some CompactHalfLine -> b1 || b2
	    | None -> failwith "Unspecified support"
	)
      else
	failwith "Support do not match [Sheet_solver.ComputeOda.union]"
    | _ -> failwith "Type Mismatch [Sheet_solver.ComputeOda.union]"

let intersection v1 v2 =
  match v1,v2 with 
    | (Area (support1,ar1,b1)),(Area (support2,ar2,b2)) ->
      if support1 = support2
      then Area
	(
	  support1,Sh.intersection ar1 ar2,match support1 with
	    | Some HalfLine -> false
	    | Some Circle
	    | Some CompactHalfLine -> b1 && b2
	    | None -> failwith "Unspecified support"
	)
      else
	failwith "Support do not match [Sheet_solver.ComputeOda.intersection]"
    | _ -> failwith "Type Mismatch [Sheet_solver.ComputeOda.intersection]"

let difference v1 v2 =
  match v1,v2 with 
    | (Area (support1,ar1,b1)),(Area (support2,ar2,b2)) ->
      if support1 = support2
      then Area
	(
	  support1,Sh.difference ar1 ar2,match support1 with
	    | Some HalfLine -> false
	    | Some Circle
	    | Some CompactHalfLine -> b1 && (not b2)
	    | None -> failwith "Unspecified support"
	)
      else
	failwith "Support do not match [Sheet_solver.ComputeOda.difference]"
    | _ -> failwith "Type Mismatch [Sheet_solver.ComputeOda.difference]"

let complement v =
  match v with 
    | Area (support,ar,b) -> Area
      (
	support,Sh.complement ar,match support with
	  | Some HalfLine -> false
	  | Some Circle
	  | Some CompactHalfLine -> not b
	  | None -> failwith "Unspecified support"
      )
    | _ -> failwith "Type Mismatch [Sheet_solver.ComputeOda.complement]"

(*direction*)

let future v1 v2 =
  match v1,v2 with 
    | (Area (support1,ar1,b1)),(Area (support2,ar2,b2)) ->
      if support1 = support2
      then
	let ar,b = match support1 with
	  | Some HalfLine -> 
	    let aux = HL.future_extension ar1 ar2 in 
	    aux,false
	  | Some Circle -> 
	    let aux = Ci.future_extension ar1 ar2 in 
	    aux,Sh.contains_zero aux
	  | Some CompactHalfLine -> 
	    let aux = HL.future_extension ar1 ar2 in 
	    aux,b2 && (b1 || HL.is_not_bounded aux)
	  | None -> failwith "Unspecified support"
	in
	Area (support1,ar,b)
      else
	failwith "Support do not match [Sheet_solver.ComputeOda.future]"
    | _ -> failwith "Type Mismatch [Sheet_solver.ComputeOda.future]"


let past v1 v2 =
  match v1,v2 with 
    | (Area (support1,ar1,b1)),(Area (support2,ar2,b2)) ->
      if support1 = support2
      then
	let ar,b = match support1 with
	  | Some HalfLine -> HL.past_extension ar1 ar2,false
	  | Some Circle -> 
	    let aux = Ci.past_extension ar1 ar2 in 
	    aux,Sh.contains_zero aux
	  | Some CompactHalfLine -> 
	    let aux = HL.past_extension ar1 ar2 in 
	    if b1
	    then
	      (
		match Sh.last_connected_component ar2 with
		  | [_] as unbounded_component -> Sh.union aux unbounded_component,b2
		  | _ -> aux,b2
	      )
	    else aux,false
	  | None -> failwith "Unspecified support"
	in
	Area (support1,ar,b)
      else
	failwith "Support do not match [Sheet_solver.ComputeOda.past]"
    | _ -> failwith "Type Mismatch [Sheet_solver.ComputeOda.past]"

(*topology*)

let closure v =
  match v with 
    | Area (support,ar,b) -> Area
      (
	let ar,b = match support with
	  | Some HalfLine -> HL.closure ar,false
	  | Some Circle -> 
	    let aux = Ci.closure ar in 
	    aux,Sh.contains_zero aux
	  | Some CompactHalfLine -> 
	    let unbounded_answer = ref false in
	    let aux = HL.closure ~unbounded_answer ar in 
	    aux,b || !unbounded_answer
	  | None -> failwith "Unspecified support"
	in
	support,ar,b
      )
    | _ -> failwith "Type Mismatch [Sheet_solver.ComputeOda.complement]"

let interior v = 
  match v with 
    | Area (support,ar,b) -> Area
      (
	let ar,b = match support with
	  | Some HalfLine -> HL.interior ar,false
	  | Some Circle -> 
	    let aux = Ci.interior ar 
	    in aux,Sh.contains_zero aux
	  | Some CompactHalfLine -> 
	    let aux = HL.interior ar 
	    in aux,b && HL.is_not_bounded aux
	  | None -> failwith "Unspecified support"
	in
	support,ar,b
      )
    | _ -> failwith "Type Mismatch [Sheet_solver.ComputeOda.complement]"

(*comparison*)

let compare v1 v2 =
  match v1,v2 with 
    | (Area (support1,ar1,b1)),(Area (support2,ar2,b2)) -> Int
      (
	if support1 = support2
	then
	  let delta = Sh.compare ar1 ar2 in
	  match support1 with
	    | Some CompactHalfLine -> if delta<>0 then delta else Pervasives.compare b1 b2
	    | _ -> delta
	else
	  failwith "Support do not match [Sheet_solver.ComputeOda.less_or_equal]"
      )
    | Bool b1,Bool b2   -> Int (Pervasives.compare b1 b2)
    | Int n1,Int n2     -> Int (Pervasives.compare n1 n2)
    | _ -> failwith "Type Mismatch [Sheet_solver.ComputeOda.past]"


let less_or_equal v1 v2 =
  match v1,v2 with 
    | (Area (support1,ar1,b1)),(Area (support2,ar2,b2)) -> Bool
      (
	if support1 = support2
	then
	  let delta = Sh.compare ar1 ar2 in
	  match support1 with
	    | Some CompactHalfLine -> if delta<0 then true else delta=0 && b1<=b2
	    | _ -> delta<=0
	else
	  failwith "Support do not match [Sheet_solver.ComputeOda.less_or_equal]"
      )
    | _ -> failwith "Type Mismatch [Sheet_solver.ComputeOda.past]"

let strictly_less v1 v2 =
  match v1,v2 with 
    | (Area (support1,ar1,b1)),(Area (support2,ar2,b2)) -> Bool
      (
	if support1 = support2
	then
	  let delta = Sh.compare ar1 ar2 in
	  match support1 with
	    | Some CompactHalfLine -> if delta<0 then true else delta=0 && b1<b2
	    | _ -> delta<0
	else
	  failwith "Support do not match [Sheet_solver.ComputeOda.less_or_equal]"
      )
    | _ -> failwith "Type Mismatch [Sheet_solver.ComputeOda.past]"


let greater_or_equal v1 v2 =
  match v1,v2 with 
    | (Area (support1,ar1,b1)),(Area (support2,ar2,b2)) -> Bool
      (
	if support1 = support2
	then
	  let delta = Sh.compare ar1 ar2 in
	  match support1 with
	    | Some CompactHalfLine -> if delta>0 then true else delta=0 && b1>=b2
	    | _ -> delta>=0
	else
	  failwith "Support do not match [Sheet_solver.ComputeOda.less_or_equal]"
      )
    | _ -> failwith "Type Mismatch [Sheet_solver.ComputeOda.past]"

let strictly_greater v1 v2 =
  match v1,v2 with 
    | (Area (support1,ar1,b1)),(Area (support2,ar2,b2)) -> Bool
      (
	if support1 = support2
	then
	  let delta = Sh.compare ar1 ar2 in
	  match support1 with
	    | Some CompactHalfLine -> if delta>0 then true else delta=0 && b1>b2
	    | _ -> delta>0
	else
	  failwith "Support do not match [Sheet_solver.ComputeOda.less_or_equal]"
      )
    | _ -> failwith "Type Mismatch [Sheet_solver.ComputeOda.past]"

let calculation t context =
  let rec calculation t = match t with 
    | support,E.ODA.Variable(s)             -> variable context s
    | support,E.ODA.Interval (b1,b2,x1,x2)  -> interval support b1 b2 x1 x2
    | support,E.ODA.Final (b,x,e)           -> final support b x e
    | support,E.ODA.HalfLine                -> halfline support
    | support,E.ODA.Circle                  -> circle support
    | support,E.ODA.CompactHalfLine         -> compacthalfline support
    | support,E.ODA.Atom x                  -> atom support x
    | support,E.ODA.Coatom x                -> coatom support x
    | support,E.ODA.Intersection(t1,t2)     -> intersection (calculation (support,t1)) (calculation (support,t2))
    | support,E.ODA.Union(t1,t2)            -> union (calculation (support,t1)) (calculation (support,t2))
    | support,E.ODA.Complement(t)           -> complement (calculation (support,t))
    | support,E.ODA.Difference(t1,t2)       -> difference (calculation (support,t1)) (calculation (support,t2))
    | support,E.ODA.Closure t               -> closure (calculation (support,t))
    | support,E.ODA.Interior t              -> interior (calculation (support,t))
    | support,E.ODA.Future(t1,t2)           -> future (calculation (support,t1)) (calculation (support,t2))
    | support,E.ODA.Past(t1,t2)             -> past (calculation (support,t1)) (calculation (support,t2))
    | support,E.ODA.Compare(t1,t2)          -> compare (calculation (support,t1)) (calculation (support,t2))
    | support,E.ODA.Less_or_equal(t1,t2)    -> less_or_equal (calculation (support,t1)) (calculation (support,t2))
    | support,E.ODA.Greater_or_equal(t1,t2) -> greater_or_equal (calculation (support,t1)) (calculation (support,t2))
    | support,E.ODA.Strictly_less(t1,t2)    -> strictly_less (calculation (support,t1)) (calculation (support,t2))
    | support,E.ODA.Strictly_greater(t1,t2) -> strictly_greater (calculation (support,t1)) (calculation (support,t2)) in
  calculation t

let from_string s =
  try Parser_oda_sheet.output Lexer_oda_sheet.entry_point (Lexing.from_string s)
  with Parsing.Parse_error -> failwith 
    (Printf.sprintf "%s to parse the given string [Oda]" (Message.red ~bold:true "Unable"))

let solve (domain,context) = 
  List.fold_left (
    fun accu name -> 
      E.add name 
        (calculation (E.find name context) accu) 
        accu) 
    E.empty 
    domain

let print context =
  let print key bind = 
    match bind with
      | Area (support,ar,b) -> (
        match support with
          | Some HalfLine -> Printf.printf "%s = %s\n"
            (Message.blue ~bold:true key) 
            (ODA.OverInteger.HL.string_of ar)
          | Some Circle -> Printf.printf "%s = %s\n" 
            (Message.blue ~bold:true key) 
            (ODA.OverInteger.Ci.string_of ~full_set_denotation:"S'" ar)
          | Some CompactHalfLine -> Printf.printf "%s = %s\n" 
            (Message.blue ~bold:true key) 
            (ODA.OverInteger.HL.string_of ~open_infinity:(not b) ar)
          | None -> failwith "Unspecified support [Sheet_solver.ComputeOda.print]")
      | Bool b -> Printf.printf "%s = %b\n" (Message.blue ~bold:true key) b
      | Int n -> Printf.printf "%s = %i\n" (Message.blue ~bold:true key) n
  in
  E.iter print context
