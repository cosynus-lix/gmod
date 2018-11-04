(* interval.ml *)

module type S =
sig
  exception Undefined
  type regular_value
  val least_regular_value: regular_value
  val compare_regular_values: regular_value -> regular_value -> int
  val max_regular_value: regular_value -> regular_value -> regular_value
  val min_regular_value: regular_value -> regular_value -> regular_value
  type underlying
  type t
  val underlying : t -> underlying
  exception Split of t * t
  val empty: t
  val full: t
  val compare: t -> t -> int
  val atom: ?u:underlying -> regular_value -> t
  val terminal: bool -> regular_value -> t
  val initial: bool -> regular_value -> t
  val bounded: bool -> bool -> regular_value -> regular_value -> t
  val action: (regular_value -> regular_value) -> t -> t
  val belongs_to: regular_value -> t -> bool
  val is_included: t -> t -> bool
  val glb: t -> regular_value
  val lub: t -> regular_value
  val is_singleton: t -> bool
  val is_empty: t -> bool
  val is_full: t -> bool
  val is_not_empty: t -> bool
  val is_not_full: t -> bool
  val is_atomic: t -> bool
  val in_touch: t -> t -> bool
  val meet: t -> t -> t
  val join: t -> t -> t
  val union: t -> t -> t
  val intersection: t -> t -> t
  val not_disjoint: t -> t -> bool
  val above: regular_value -> t -> t
  val below: regular_value -> t -> t
  val strictly_above: regular_value -> t -> t
  val strictly_below: regular_value -> t -> t
  val string_of: ?sd:(string * string) -> t -> string
  val mirror_string_of: ?sd:(string * string) -> t -> string
  val interior: t -> t
  val closure: t -> t
  val normalize: t -> t
  val before: t -> t
  val after: t -> t
  val downward: t -> t 
  val upward: t -> t
  val is_bounded: t -> bool
  val complement: t -> t
  val lft_bool_of_bound: t -> bool
  val rgt_bool_of_bound: t -> bool
  val in_the_future_of: t -> t -> t
  val in_the_past_of: t -> t -> t
  val in_the_future_of_point: regular_value -> t -> t
  val in_the_past_of_point: regular_value -> t -> t
  val next: (regular_value -> regular_value) -> t -> t
  val next_region: (regular_value -> regular_value) -> t list -> t list
end

module Make(B:Sig.Bound)
  =
struct

  let (<) x y = B.compare x y < 0

  let (>) x y = B.compare x y > 0

  let (<=) x y = B.compare x y <= 0

  let (>=) x y = B.compare x y >= 0

  exception Undefined

  type regular_value = B.t

  let least_regular_value = B.least_regular_value

  let compare_regular_values rv1 rv2 = B.compare rv1 rv2

  let max_regular_value rv1 rv2 =
    if rv1 > rv2 then rv1 else rv2

  let min_regular_value rv1 rv2 =
    if rv1 < rv2 then rv1 else rv2

  (* bounds can be either open or closed *)

  type bound =
    | Opn of B.t
    | Cls of B.t

  (* An interval of the positive half-line is either a final segment,
     a bounded set a singleton or empty *)

  type underlying = Line

  type t =
    | Te of bound         (* final     *)
    | Bn of bound * bound (* bounded   *)
    | Si of B.t           (* singleton *)
    | Em                  (* empty     *)

  let underlying it = Line

  let action e it =
    let action_on_bound b = match b with 
    | Opn v -> Opn (e v) 
    | Cls v -> Cls (e v) in
    match it with
    | Te b -> Te (action_on_bound b)
    | Bn (b1,b2) -> Bn (action_on_bound b1,action_on_bound b2)
    | Si v -> Si (e v)
    | Em -> Em

  let is_singleton it = match it with
    | Si _ -> true
    | _    -> false

  exception Split of t * t

  let rvb b = match b with
    | Opn n
    | Cls n -> n

  let reverse_bound b = match b with
    | Opn a -> Cls a
    | Cls a -> Opn a

  let bound_opened b = match b with
    | Opn a -> Opn a
    | Cls a -> Opn a

  let bound_closed b = match b with
    | Opn a -> Cls a
    | Cls a -> Cls a


  (* The type of the arguments is bound. Let ib and id be the
     initial segments whose right bounds are b and d. The output of
     compare_initial b d is strictly negative if ib is strictly
     included in id, strictly positive if id is strictly included in
     ib, null if they are equal *)

  let compare_initial b d =
    let e = B.compare (rvb b) (rvb d)
    in
    match (b,d) with
      | (Opn _,Opn _)
      | (Cls _,Cls _) -> e
      | (Opn _,Cls _) -> if e<>0 then e else -2
      | (Cls _,Opn _) -> if e<>0 then e else  2

  (* The type of the arguments is bound. Let ta and tc be the final
     segments whose left bounds are a and c. The output of
     compare_terminal a c is strictly negative if ta is strictly
     included in tc, strictly positive if tc is strictly included in
     ta, null if they are equal *)

  let compare_terminal a c =
    let e = B.compare (rvb c) (rvb a)
    in
    match (a,c) with
      | (Opn _,Opn _)
      | (Cls _,Cls _) -> e
      | (Opn _,Cls _) -> if e<>0 then e else -2
      | (Cls _,Opn _) -> if e<>0 then e else  2

  (* The function compare describes a total order which extends the
     inclusion relation over the type interval. It also extends the
     partial order defined by it1 <= it2 when it1 = it2 or it1 and it2
     are disjoint and such that any element of it1 is less than some
     element of it2. Formally the functions compare_initial and
     compare_terminal provide two total orders over the type bound,
     the first one is the lexicographic product of the total order
     over the type regular_value and {open<close}, while the second
     one is the lexicographic product of the opposite of the total
     order over the type regular_value and {open<close}. Then, the
     order implemented by the function compare is the lexicographic
     product of the preceding ones *)

  let compare it1 it2 =
    match it1,it2 with
      | Te a,Te c -> compare_terminal a c
      | Te _,_ ->  1
      | _,Te _ -> -1
      | Bn(a,b),Bn(c,d) ->
	let e = compare_initial b d in
	if e<>0 then e else compare_terminal a c
      | Si x,Bn(c,d) ->
	let e = compare_initial (Cls x) d in
	if e<>0 then e else -1
      | Bn(c,d),Si x ->
	let e = compare_initial d (Cls x) in
	if e<>0 then e else  1
      | Si x,Si y -> B.compare x y
      | _,Em -> if it1=Em then 0 else  1
      | Em,_ -> if it2=Em then 0 else -1

  let empty = Em and full = Te(Cls B.least_regular_value)

  let bob b = match b with
    | Cls _ -> true
    | Opn _ -> false

  let lft_bool_of_bound it =
    match it with
      | Bn(Cls _,_) -> true
      | Te(Cls _)   -> true
      | Si _        -> true
      | _ -> false
  and rgt_bool_of_bound it =
    match it with
      | Bn(_,Cls _) -> true
      | Si _        -> true
      | _ -> false

  let normalize it = match it with
    | Bn(Opn va,Opn vb)
    | Bn(Opn va,Cls vb)
    | Bn(Cls va,Opn vb) -> if va < vb then it else Em
    | Bn(Cls va,Cls vb) -> if va < vb then it else (if va=vb then Si va else Em)
    | _                 -> it

  let check it = match it with
    | Bn(l,r) -> (rvb l) < (rvb r)
    | _       -> true

  let atom ?(u=Line) x = Si x

  let terminal b x = Te(if b then Cls x else Opn x)

  let coatom ?(u=Line) x = failwith "Interval : coatom is pointless when dealing with intervals"

  let initial b x =
    if x = B.least_regular_value
    then
      if b
      then Si x
      else Em
    else
      Bn(Cls B.least_regular_value,(if b then Cls x else Opn x))

  let bounded lf rf lb rb =
    let a = if lf then Cls lb else Opn lb
    and b = if rf then Cls rb else Opn rb
    in
    match (a,b) with
      | (Cls _,Cls _) ->
	if lb < rb
	then Bn(a,b)
	else 
	  if lb > rb
	  then Em
	  else Si lb
      | _ ->
	if lb < rb
	then Bn(a,b)
	else Em

  let is_atomic it = match it with
    | Si _ -> true
    | _    -> false
  and
      is_coatomic it = (it = Te(Opn B.least_regular_value))

  let cobounded lf rf lb rb = failwith "Interval : cobounded is pointless when dealing with intervals"

  (* greatest lower bound *)

  let glb it = match it with
    | Bn(l,_) -> rvb l
    | Te(l)   -> rvb l
    | Si x    -> x
    | Em      -> raise Undefined

  (* least upper bound *)

  let lub it = match it with
    | Bn(_,r) -> rvb r
    | Te _    -> raise Undefined
    | Si x    -> x
    | Em      -> B.least_regular_value

  let belongs_to x it =
    match it with
      | Bn(Opn va,Opn vb) -> (va< x) && (x< vb)
      | Bn(Opn va,Cls vb) -> (va< x) && (x<=vb)
      | Bn(Cls va,Opn vb) -> (va<=x) && (x< vb)
      | Bn(Cls va,Cls vb) -> (va<=x) && (x<=vb)
      | Te(Opn va)       ->   va< x
      | Te(Cls va)       ->   va<=x
      | Si va            ->   va =x
      | Em              ->    false

  let is_included it1 it2 =
    match (it1,it2) with
      | (Bn(Opn va,Opn vb),Bn(Opn vc,Opn vd)) -> (vc<=va)&&(vb<=vd)
      | (Bn(Opn va,Opn vb),Bn(Opn vc,Cls vd)) -> (vc<=va)&&(vb<=vd)
      | (Bn(Opn va,Opn vb),Bn(Cls vc,Opn vd)) -> (vc<=va)&&(vb<=vd)
      | (Bn(Opn va,Opn vb),Bn(Cls vc,Cls vd)) -> (vc<=va)&&(vb<=vd)
      | (Bn(Opn va,Cls vb),Bn(Opn vc,Opn vd)) -> (vc<=va)&&(vb< vd)
      | (Bn(Opn va,Cls vb),Bn(Opn vc,Cls vd)) -> (vc<=va)&&(vb<=vd)
      | (Bn(Opn va,Cls vb),Bn(Cls vc,Opn vd)) -> (vc<=va)&&(vb< vd)
      | (Bn(Opn va,Cls vb),Bn(Cls vc,Cls vd)) -> (vc<=va)&&(vb<=vd)
      | (Bn(Cls va,Opn vb),Bn(Opn vc,Opn vd)) -> (vc< va)&&(vb<=vd)
      | (Bn(Cls va,Opn vb),Bn(Opn vc,Cls vd)) -> (vc< va)&&(vb<=vd)
      | (Bn(Cls va,Opn vb),Bn(Cls vc,Opn vd)) -> (vc<=va)&&(vb<=vd)
      | (Bn(Cls va,Opn vb),Bn(Cls vc,Cls vd)) -> (vc<=va)&&(vb<=vd)
      | (Bn(Cls va,Cls vb),Bn(Opn vc,Opn vd)) -> (vc< va)&&(vb< vd)
      | (Bn(Cls va,Cls vb),Bn(Opn vc,Cls vd)) -> (vc< va)&&(vb<=vd)
      | (Bn(Cls va,Cls vb),Bn(Cls vc,Opn vd)) -> (vc<=va)&&(vb< vd)
      | (Bn(Cls va,Cls vb),Bn(Cls vc,Cls vd)) -> (vc<=va)&&(vb<=vd)
      | (Bn(Opn va,_),Te(Opn vc)) -> (vc<=va)
      | (Bn(Cls va,_),Te(Opn vc)) -> (vc< va)
      | (Bn(Opn va,_),Te(Cls vc)) -> (vc<=va)
      | (Bn(Cls va,_),Te(Cls vc)) -> (vc<=va)
      | (Si va,Bn(Opn vc,Opn vd)) -> (vc< va)&&(va< vd)
      | (Si va,Bn(Opn vc,Cls vd)) -> (vc< va)&&(va<=vd)
      | (Si va,Bn(Cls vc,Opn vd)) -> (vc<=va)&&(va< vd)
      | (Si va,Bn(Cls vc,Cls vd)) -> (vc<=va)&&(va<=vd)
      | (Si va,Te(Opn vc)) -> (vc< va)
      | (Si va,Te(Cls vc)) -> (vc<=va)
      | (Te(Opn va),Te(Opn vc)) -> vc<=va
      | (Te(Opn va),Te(Cls vc)) -> vc<=va
      | (Te(Cls va),Te(Opn vc)) -> vc< va
      | (Te(Cls va),Te(Cls vc)) -> vc<=va
      | (Si va,Si vc) -> va=vc
      | (Em,_) -> true
      | _ -> false

  let is_empty it = (it = Em)

  let is_not_empty it = (it <> Em)

  let is_full it = (it = Te(Cls B.least_regular_value))

  let is_not_full it = (it <> Te(Cls B.least_regular_value))

  let intersection it1 it2 = match (it1,it2) with
    | (Bn(a,b),Bn(c,d)) ->
	let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d and
	    fa = bob a and fb = bob b and fc = bob c and fd = bob d
	in
	let cmpbc = B.compare vb vc and cmpda = B.compare vd va
	in
	  if Pervasives.(>) cmpbc 0
	  then
	    if Pervasives.(>)  cmpda 0
	    then
	      Bn((if (vc<va)||((va=vc)&&(not fa)) then a else c),(if (vb<vd)||((vb=vd)&&(not fb)) then b else d))
	    else
	      if (Pervasives.(<) cmpda 0)||(not fa)||(not fd)
	      then
		Em
	      else
		Si va
	  else
	    if (Pervasives.(<) cmpbc 0)||(not fb)||(not fc)
	    then
	      Em
	    else
	      Si vb
    | (Te a,Te c) ->
	let va = rvb a and vc = rvb c
	in
	Te(if (va<vc)||((va=vc)&&(not(bob c))) then c else a)
    | (Te c,Bn(a,b))
    | (Bn(a,b),Te c) ->
	let va = rvb a and vb = rvb b and vc = rvb c and
	    fa = bob a and fb = bob b and fc = bob c
	in
	let cmpbc = B.compare vb vc
	in
	  if Pervasives.(>) cmpbc 0
	  then
	    Bn((if (vc<va)||((va=vc)&&(not fa)) then a else c),b)
	  else
	    if (Pervasives.(<) cmpbc 0)||(not fb)||(not fc)
	    then
	      Em
	    else
	      Si vb
    | (Si x,Bn(a,b))
    | (Bn(a,b),Si x) ->
	let va = rvb a
	and vb = rvb b
	in
	  if ((va=x)&&(bob a))||((vb=x)&&(bob b))||((va < x)&&(x < vb))
	  then Si x
	  else Em
    | (Te c,Si x)
    | (Si x,Te c) ->
	let vc = rvb c
	in
	  if (vc<x)||((vc=x)&&(bob c))
	  then
	    Si x
	  else
	    Em
    | (Si x,Si y) -> if x=y then it1 else Em
    | _ -> Em


  let meet = intersection

  let join it1 it2 =
    match (it1,it2) with
      | (Em,it)
      | (it,Em) -> it
      | (Si x,Si y) -> if x = y then Si x else Bn(Cls(B.min x y),Cls(B.max x y))
      | (Si x,Te c)
      | (Te c,Si x) -> if x <= (rvb c) then Te(Cls(x)) else Te c
      | (Si x,Bn(c,d))
      | (Bn(c,d),Si x) ->
	  if  x <= (rvb c)
	  then
	    Bn(Cls(x),d)
	  else
	    (
	      if (rvb d) <= x
	      then
		Bn(c,Cls(x))
	      else
		Bn(c,d)
	    )
      | (Bn(a,b),Bn(c,d)) -> Bn
	  (
	    (
	      match (a,c) with
		| (Opn(x),Opn(y)) -> Opn(B.min x y)
		| (Cls(x),Cls(y)) -> Cls(B.min x y)
		| (Cls(x),Opn(y))
		| (Opn(y),Cls(x)) -> if y < x then Opn(y) else Cls(x)
	    )
	      ,
	    (
	      match (b,d) with
		| (Opn(x),Opn(y)) -> Opn(B.max x y)
		| (Cls(x),Cls(y)) -> Cls(B.max x y)
		| (Cls(x),Opn(y))
		| (Opn(y),Cls(x)) -> if x < y then Opn(y) else Cls(x)
	    )
 	  )
      | (Te a,Te c)
      | (Bn(c,_),Te a)
      | (Te a,Bn(c,_)) -> Te
	  (
	    match (a,c) with
	      | (Opn(x),Opn(y)) -> Opn(B.min x y)
	      | (Cls(x),Cls(y)) -> Cls(B.min x y)
	      | (Cls(x),Opn(y))
	      | (Opn(y),Cls(x)) -> if y < x then Opn(y) else Cls(x)
 	  )

  (* The function fast_join makes the following hypothesis upon its
     arguments: it1 and it2 are the closure of two disjoint intervals
     such that any sup(it1) <= inf(it2) *)

  let fast_join it1 it2 =
    match (it1,it2) with
      (*| (Em,it)
	| (it,Em) -> it*)
      | Si x,Si y -> Bn(Cls x,Cls y)
      | Si x,Te c
      (*| (Te c,Si x)*) -> Te(Cls(x))
      | Si x,Bn(_,d) -> Bn(Cls x,d)
      | Bn(c,_),Si x -> Bn(c,Cls x)
      | Bn(a,b),Bn(c,d) -> Bn(a,d)
      | Bn(c,_),Te a -> Te c
      | Te a,Te c -> Te a
      | _ -> failwith "fast_join: unexpected case"

  let not_disjoint it1 it2 = is_not_empty (meet it1 it2)

  let above x it = meet it (Te(Cls(x)))

  let below x it = meet it (Bn(Cls(B.least_regular_value),Cls(x)))

  let strictly_above x it = meet it (Te(Opn(x)))

  let strictly_below x it = if x<>B.least_regular_value then meet it (Bn(Cls(B.least_regular_value),Opn(x))) else Em

  module DisplayMode =
  struct

    let debug ?(sd=("[","]")) it = match it with
      | Bn(a,b) -> Printf.sprintf "Bn(%s,%s)" 
	(Printf.sprintf "%s %s" (if bob a then "Cls" else "Opn") (B.string_of (rvb a)))
	(Printf.sprintf "%s %s" (if bob b then "Cls" else "Opn") (B.string_of (rvb b)))
      | Te a -> Printf.sprintf "Te %s" 
	(Printf.sprintf "%s %s" (if bob a then "Cls" else "Opn") (B.string_of (rvb a)))
      | Si x -> Printf.sprintf "Si %s" (B.string_of x)
      | Em -> "Em"


    let pretty ?(sd=("[","]")) it = (* sd stands for "singleton delimiters" *)
      match it with
	| Bn(a,b) ->
	  (if bob a then "[" else "]")
	  ^(B.string_of (rvb a))
	  ^","
	  ^(B.string_of (rvb b))
	  ^(if bob b then "]" else "[")
	| Te a ->
	  (if bob a then "[" else "]")
	  ^(B.string_of (rvb a))
	  ^",+oo["
	| Si x -> (fst sd)^(B.string_of x)^(snd sd)
	| Em -> "@"

    let mirror_pretty ?(sd=("[","]")) it =
      match it with
	| Bn(a,b) ->
	  (if bob b then "]" else "[")
	  ^(B.string_of (rvb b))
	  ^","
	  ^(B.string_of (rvb a))
	  ^(if bob a then "[" else "]")
	| Te(_) -> failwith "Interval.Rawbis : mirror_string"
	| Si(n) -> (snd sd)^(B.string_of n)^(fst sd)
	| Em -> "@"

  end

  let mirror_string_of = DisplayMode.mirror_pretty

  let string_of = DisplayMode.pretty

  (* bugfix 2011-01-20: In the topological space [0,+oo[ the interiors
     of [0,x], [0,x[ and [0,+oo[ are respectively [0,x], [0,x[ and
     [0,+oo[ instead of ]0,x], ]0,x[ and ]0,+oo[. See whether this
     bugfix alter the behaviour of ALCOOL.*)

  let interior it =
    match it with
      | Bn(a,b) -> Bn((if rvb a <> least_regular_value then bound_opened a else a),bound_opened b)
      | Te a -> Te(if rvb a <> least_regular_value then bound_opened a else a)
      | _ -> empty

  let closure it =
    match it with
      | Bn(a,b) -> Bn(bound_closed a,bound_closed b)
      | Te a -> Te(bound_closed a)
      | _ -> it

  let in_touch it1 it2 = match (it1,it2) with
    | (Bn(a,b),Bn(c,d)) ->
      let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d
      in
      (
	(vc < vb)
	|| ((vb=vc)&&((bob b)||(bob c)))
      )
      &&
	(
	  (va < vd)
	  || ((va=vd)&&((bob a)||(bob d)))
	)
    | (Te c,Bn(a,b))
    | (Bn(a,b),Te c) ->
      let vb = rvb b and vc = rvb c
      in
      (
	(vc < vb)
	|| ((vb=vc)&&((bob b)||(bob c)))
      )
    | (Bn(a,b),Si x)
    | (Si x,Bn(a,b)) -> ((rvb a) <= x)&&(x <= (rvb b))
    | (Si x,Si y)    -> x=y
    | (Em,_)
    | (_,Em)         -> false
    | _              -> true

  let union_old it1 it2 =
    match (it1,it2) with
      | (Bn(a,b),Bn(c,d)) ->
	let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d and
	    fa = bob a and fb = bob b and fc = bob c and fd = bob d
	in
	if (vb < vc) || (vd < va) || ((vb=vc)&&(not fb)&&(not fc)) || ((va=vd)&&(not fa)&&(not fd))
	then
	  raise (Split(it1,it2))
	else
	  Bn((if (va < vc)||((va=vc)&&fa) then a else c),(if (vb > vd)||((vb=vd)&&fb) then b else d))
      | (Te c,Bn(a,b))
      | (Bn(a,b),Te c) ->
	let va = rvb a and vb = rvb b and vc = rvb c and
	    fa = bob a and fb = bob b and fc = bob c
	in
	if (vb < vc) || ((vb=vc)&&(not fb)&&(not fc))
	then
	  raise (Split(it1,it2))
	else
	  Te (if (va < vc)||((va=vc)&&fa) then a else c)
      | (Bn(a,b),Si x)
      | (Si x,Bn(a,b)) ->
	let va = rvb a and vb = rvb b
	in
	if (x < va)||(vb < x)
	then raise (Split(it1,it2))
	else
	  if x=va then Bn(Cls(x),b)
	  else
	    if x=vb then Bn(a,Cls(x))
	    else
	      Bn(a,b)
      | (Si x,Si y) -> if x<>y then raise (Split(it1,it2)) else it1
      | (Em,it)
      | (it,Em) -> it
      | _ -> Te (Cls B.least_regular_value)

  (* The new function returns a sorted argument when the exception
     Split is raised *)

  let union it1 it2 =
    match it1,it2 with
      | Bn(a,b),Bn(c,d) ->
	let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d and
	    fa = bob a and fb = bob b and fc = bob c and fd = bob d
	in
	if (vb < vc) || ((vb=vc)&&(not fb)&&(not fc)) 
	then
	  raise (Split(it1,it2))
	else
	  if (vd < va) ||  ((va=vd)&&(not fa)&&(not fd))
	  then
	    raise (Split(it2,it1))
	  else
	    Bn((if (va < vc)||((va=vc)&&fa) then a else c),(if (vb > vd)||((vb=vd)&&fb) then b else d))
      | Te c,Bn(a,b)
      | Bn(a,b),Te c ->
	let va = rvb a and vb = rvb b and vc = rvb c and
	    fa = bob a and fb = bob b and fc = bob c
	in
	if (vb < vc) || ((vb=vc)&&(not fb)&&(not fc))
	then
	  raise (Split (Bn(a,b),Te c))
	else
	  Te (if (va < vc)||((va=vc)&&fa) then a else c)
      | Bn(a,b),Si x
      | Si x,Bn(a,b) ->
	let va = rvb a and vb = rvb b
	in
	if (x < va)
	then raise (Split (Si x,Bn(a,b)))
	else
	  if (vb < x)
	  then raise (Split (Bn(a,b),Si x))
	  else
	    if x=va then Bn(Cls(x),b)
	    else
	      if x=vb then Bn(a,Cls(x))
	      else
		Bn(a,b)
      | Si x,Si y -> 
        if x<y then raise (Split(it1,it2)) 
        else 
          if x>y then raise (Split(it2,it1)) 
          else it1
      | Em,it
      | it,Em -> it
      | _ -> Te (Cls B.least_regular_value)

  let before it =
    match it with
      | Bn(Opn va,_)
      | Te(Opn va) ->
	if va<>B.least_regular_value
	then Bn(Cls(B.least_regular_value),Cls va)
	else Si va
      | Bn(Cls va,_)
      | Te(Cls va) ->
	if va<>B.least_regular_value
	then Bn(Cls(B.least_regular_value),Opn va)
	else Em
      | Si x -> Bn(Cls(B.least_regular_value),Opn(x))
      | Em   -> Te(Cls(B.least_regular_value))

  let after it =
    match it with
      | Bn(_,b) -> Te(reverse_bound b)
      | Te _ -> Em
      | Si x -> Te(Opn(x))
      | Em   -> Te(Cls(B.least_regular_value))

  let downward it = match it with
    | Bn(_,b) -> Bn(Cls(least_regular_value),b)
    | Te b -> full (* Bn(Cls(least_regular_value),b) bug found by Alex Lang 12.06.2010 *)
    | Si x -> Bn(Cls(least_regular_value),Cls(x))
    | Em    -> Em

  let upward it = match it with
    | Bn(a,_)
    | Te a -> Te a
    | Si x -> Te(Cls(x))
    | Em    -> Em

  let is_bounded it = match it with
    | Te(_) -> false
    | _     -> true

  let complement it = match it with
    | Te _
    | Em -> before it
    | Si x ->
	if x<>B.least_regular_value
	then
	  raise (Split (Bn(Cls B.least_regular_value,Opn x),Te(Opn x)))
	else
	  Te(Opn x)
    | Bn(Cls va,b) ->
	if va<>B.least_regular_value
	then
	  raise (Split (Bn(Cls B.least_regular_value,Opn va),Te(reverse_bound b)))
	else
	  Te(reverse_bound b)
    | Bn(Opn va,b) ->
	if va<>B.least_regular_value
	then
	  raise (Split (Bn(Cls B.least_regular_value,Cls va),Te(reverse_bound b)))
	else
	  raise (Split (Si B.least_regular_value,Te(reverse_bound b)))

  let in_the_future_of it1 it2 = match (it1,it2) with
    | (Bn(a,b),Bn(c,d)) ->
	let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d and
	    fa = bob a and fb = bob b and fc = bob c and fd = bob d
	in
	  if (vb<vc)||(vd<va)||((vb=vc)&&(not(fb||fc)))||((va=vd)&&(not(fa&&fd)))
	  then
	    Em
	  else
	    if va<>vd
	    then
	      Bn((if (va<vc)||(va=vc&&(not fc)) then c else a),d)
	    else
	      Si va
    | (Te a,Te c) ->
	let va = rvb a and vc = rvb c
	in
	  Te (if (va<vc)||(va=vc&&(not (bob c))) then c else a)
    | (Bn(a,b),Te c) ->
	let va = rvb a and vb = rvb b and vc = rvb c and
	    fb = bob b and fc = bob c
	in
	  if (vb<vc)||((vb=vc)&&(not(fb||fc)))
	  then
	    Em
	  else
	    Te (if (va<vc)||(va=vc&&(not fc)) then c else a)
    | (Te a,Bn(c,d)) ->
	let va = rvb a and vc = rvb c and vd = rvb d and
	    fa = bob a and fc = bob c and fd = bob d
	in
	  if (vd<va)||((va=vd)&&(not(fa&&fd)))
	  then
	    Em
	  else
	    if va<>vd
	    then
	      Bn((if (va<vc)||(va=vc&&(not fc)) then c else a),d)
	    else
	      Si va
    | (Si x,Bn(c,d)) ->
	let vc = rvb c and vd = rvb d and fd = bob d
	in
	  if (x<vd)&&(vc<x)
	  then Bn(Cls x,d)
	  else
	    if vc=x
 	    then it2
	    else
	      if (x=vd)&&fd
	      then Si x
	      else Em
    | (Si x,Te c) ->
	let vc = rvb c
	in
	  if vc<x
	  then Te(Cls x)
	  else
	    if vc=x
 	    then it2
	    else Em
    | (Bn(a,b),Si x) ->
	let va = rvb a and vb = rvb b and fa = bob a
	in
	  if ((va<x)&&(x<vb))||((x=va)&&fa)||(vb=x)
	  then it2
	  else Em
    | (Te a,Si x) ->
	let va = rvb a
	in
	  if (va<x)||((x=va)&&(bob a))
	  then it2
	  else Em
    | (Si x,Si y) -> if x=y then it1 else Em
    | (_,Em)
    | (Em,_) -> Em

    let in_the_future_of_point p arc = in_the_future_of (atom p) arc

    (* The function in_the_past_of is deduced from the function
       in_the_future_of by considering the opposite order over the
       type regular_value. Actually it suffices to change the body of
       the function in_the_future_of as follows, on the left-hand side
       of the "->" of the "match" d'effectuer les échanges de rôles
       suivants :

       a <-> b et c <-> d

       then on the right hand side of the "->" :

       take the opposite of the comparison order on the regular values i.e. exchange < et >

       exchange the bounds in each occurence of the constructor Bn

    *)

    let in_the_past_of it1 it2 = match (it1,it2) with
      | (Bn(b,a),Bn(d,c)) ->
	  let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d and
	      fa = bob a and fb = bob b and fc = bob c and fd = bob d
	  in
	    if (vb>vc)||(vd>va)||((vb=vc)&&(not(fb||fc)))||((va=vd)&&(not(fa&&fd)))
	    then
	      Em
	    else
	      if va<>vd
	      then
		Bn(d,(if (va>vc)||(va=vc&&(not fc)) then c else a))
	      else
		Si va
      | (Bn(b,a),Te d) ->
	  let va = rvb a and vd = rvb d and
	      fa = bob a and fd = bob d
	  in
	    if (vd>va)||((va=vd)&&(not(fa&&fd)))
	    then
	      Em
	    else
	      if va<>vd
	      then
		Bn(d,a)
	      else
		Si va
      | (Te b,Bn(d,c)) ->
	  let vb = rvb b and vc = rvb c and
	      fb = bob b and fc = bob c
	  in
	    if (vb>vc)||((vb=vc)&&(not(fb||fc)))
	    then
	      Em
	    else
	      it2
      | (Si x,Bn(d,c)) ->
	  let vc = rvb c and vd = rvb d and fd = bob d
	  in
	    if (x>vd)&&(vc>x)
	    then Bn(d,Cls x)
	    else
	      if vc=x
 	      then it2
	      else
		if (x=vd)&&fd
		then Si x
		else Em
      | (Si x,Te d) ->
	  let vd = rvb d
	  in
	    if x>vd
	    then Bn(d,Cls x)
	    else
		if (x=vd)&&(bob d)
		then Si x
		else Em
      | (Bn(b,a),Si x) ->
	  let va = rvb a and vb = rvb b and fa = bob a
	  in
	    if ((va>x)&&(x>vb))||((x=va)&&fa)||(vb=x)
	    then it2
	    else Em
      | (Te b,Si x) ->
	  let vb = rvb b
	  in
	    if (x>vb)||(vb=x)
	    then it2
	    else Em
      | (Si x,Si y) -> if x=y then it1 else Em
      | (Te _,Te _) -> it2
      | (_,Em)
      | (Em,_) -> Em

  let in_the_past_of_point p arc = in_the_past_of (atom p) arc

  let step_forward = in_the_future_of
  and step_backward = in_the_past_of

  (* Énumère les intervalles *)
  (* The function next is supposed to raise Exit if one is beyond the last 
  element of the enumeration *)
  let next next it = 
    let next_bound b = match b with
      | Opn y -> Cls y
      | Cls y -> Opn (next y) in
    match it with
      | Si x -> (try Bn ((Cls x), (Opn (next x))) with Exit -> Te (Cls x))
      | Bn (a, b) -> (try Bn (a, (next_bound b)) with Exit -> Te a) 
      | Te (Cls x) -> (try Bn ((Opn x), (Opn (next x))) with Exit -> Te (Opn x))
      | Te (Opn x) -> (try Si (next x) with Exit -> Em)
      | Em -> raise Exit
      
  let nonempty_disconnected_next next it =
    match it with
      | Te _ -> raise Exit
      | Bn (_,Opn x) -> (try Bn ((Opn x) , (Opn (next x))) with Exit -> Te (Opn x))
      | Si x | Bn (_,Cls x) -> Si (next x)
      | Em -> raise Exit
  
  (* Une région est ici représentée par une liste décroissante d'intervalles, 
  l'ordre I < J signifiant que tout point de I est strictement inférieur à 
  tout point de J et que I et J sont déconnectés. Pour des raisons pratiques, 
  la fonction next region prend les listes renversées. La région vide est 
  représentée par la liste vide. Aucun intervalle de la liste n'est vide. *)
  
(*
  let print_region re = 
    List.iter (fun it -> print_string ((string_of it)^" ")) re; 
    print_endline ""
*)
  
  let rec glb_region re = 
    match re with 
      | [it] -> glb it 
      | _ :: re -> glb_region re
      | _ -> raise Undefined

  let next_region next_value re = 
    let next it = 
      let next = next next_value it in
      if next <> Em then next else raise Exit in
    let next_with_lesser_lub it =
      let x = ref it in
      let y = ref (next it) in
      while (
        (is_bounded !x) && (not (is_bounded !y)) || 
        (is_bounded !x && is_bounded !y && lub !x <= lub !y))
      do x := !y ; y := next !y
      done;
      !y in
    let rec init k re =
      if (Pervasives.(>)) k 0 
      then 
        init (pred k) (
          match re with 
            | it :: _ -> nonempty_disconnected_next next_value it :: re
            | []      -> [atom B.least_regular_value])
      else re in
    let init k re = List.rev (init k re) in
    let rec next_region len re =
        match re with
          (*| [it] -> assert (len = 1); [next it]*) (* petite optimisation *)
          | it :: re' -> (
              try it :: next_region (pred len) re'
              with Exit -> (
               try init (pred len) [next it]
               with Exit -> init (pred len) [next_with_lesser_lub it] ))
          | [] -> raise Exit (* assert false *) in
    try
      match re with 
        | [] -> [atom B.least_regular_value]
        | _ -> next_region (List.length re) re 
    with Exit -> init (succ (List.length re)) []
end
