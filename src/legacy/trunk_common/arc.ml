(* arc.ml *)

module type S =
sig
  exception Undefined
  type regular_value
  val least_regular_value: regular_value
  val compare_regular_values: regular_value -> regular_value -> int
  type underlying
  type t
  val underlying: t -> underlying
  exception Split of t * t
  val empty: t
  val full: t
  val compare: t -> t -> int
  val atom: ?u:underlying -> regular_value -> t
  val coatom: ?u:underlying -> regular_value -> t
  val bounded: bool -> bool -> regular_value -> regular_value -> t
  val action: (regular_value -> regular_value) -> t -> t
  val belongs_to: regular_value -> t -> bool
  val is_included: t -> t -> bool
  val glb: t -> regular_value 
  val lub: t -> regular_value 
  val is_empty: t -> bool
  val is_full: t -> bool
  val is_not_empty: t -> bool
  val is_not_full: t -> bool
  val is_atomic: t -> bool
  val is_coatomic: t -> bool
  val in_touch: t -> t -> bool
  val union: t -> t -> t
  val dummy_union: t -> t -> t
  val intersection: t -> t -> t
  val not_disjoint: t -> t -> bool 
  val string_of: ?sd:(string * string) -> t -> string
  val interior: t -> t
  val closure: t -> t
  val normalize: t -> t
  val complement: t -> t
  val in_the_future_of: t -> t -> t 
  val in_the_past_of: t -> t -> t 
  val in_the_future_of_point: regular_value -> t -> t 
  val in_the_past_of_point: regular_value -> t -> t 
  val initial: bool -> regular_value -> t
  val terminal: bool -> regular_value -> t
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

  let compare_regular_values rv1 rv2 = B.compare rv1 rv2

  type bound =
    | Opn of B.t
    | Cls of B.t

    (* One may have only 4 constructors but the resulting code would
       be more intricate. *)

  type underlying = Circle

  type t =
    | Em                (* empty arc              *)
    | Co of B.t         (* complement of a point  *)
    | Ex of bound*bound (* extended interval      *)
    | St of bound*bound (* standard interval      *)
    | Pn of B.t         (* point (i.e. singleton) *)
    | Fu                (* full circle            *)

    (* In an arc St(a,b) (resp. Ex(a,b)) we must have a<b (resp. b<a) *)

    let underlying arc = Circle

    exception Split of t * t

    (* rvb: the bounds of an interval may be finite, in this case rvb
       returns the regular value associated with the given bound. *)

    let rvb b = match b with
      | Opn x -> x
      | Cls x -> x

    (* bound_reverse: in some sense, this function returns the complement
       of the given terminal segment; it is mainly used in the test of
       inclusion. *)

    (** Chaque borne peut-être "ouverte" ou "fermée", bound_reverse échange "ouvert" et "fermé". *)
    let bound_reverse b = match b with
      | Opn a -> Cls a
      | Cls a -> Opn a

    (** Chaque borne peut-être "ouverte" ou "fermée", bound_opened met la borne dans l'état "ouvert".*)
    let bound_opened b = match b with
      | Opn a -> Opn a
      | Cls a -> Opn a

    (** Chaque borne peut-être "ouverte" ou "fermée", bound_opened met la borne dans l'état "fermé".*)
    let bound_closed b = match b with
      | Opn a -> Cls a
      | Cls a -> Cls a

let action e arc =
    let action_on_bound b = match b with 
    | Opn v -> Opn (e v) 
    | Cls v -> Cls (e v) in
    match arc with
    | Em -> Em
    | Co v -> Co (e v)
    | Ex (a,b) -> Ex (action_on_bound a,action_on_bound b)
    | St (a,b) -> Ex (action_on_bound a,action_on_bound b)
    | Pn v -> Pn (e v)
    | Fu -> Fu

    (** Soit ia (respectivement ib) le segment initial dont la borne
	droite est a (respectivement b). La fonction compare_initial
	retourne une valeur entière strictement négative si ia est
	strictement inclus dans ib, une valeur entière strictement
	positive si ib est strictement inclus dans ia, la valeur nulle
	en cas d'égalité. **)

    let compare_initial a b =
      let e = B.compare (rvb a) (rvb b)
      in
	match (a,b) with
	  | (Opn _,Opn _)
	  | (Cls _,Cls _) -> e
	  | (Opn _,Cls _) -> if e=0 then -2 else e
	  | (Cls _,Opn _) -> if e=0 then  2 else e

    (** Soit ta (respectivement tb) le segment terminal dont la borne
	gauche est a (respectivement b). La fonction compare_terminal
	retourne une valeur entière strictement négative si ta est
	strictement inclus dans tb, une valeur entière strictement
	positive si tb est strictement inclus dans ta, la valeur nulle
	en cas d'égalité. **)

    let compare_terminal a b =
      let e = B.compare (rvb b) (rvb a)
      in
	match (a,b) with
	  | (Opn _,Opn _)
	  | (Cls _,Cls _) -> e
	  | (Opn _,Cls _) -> if e=0 then -2 else e
	  | (Cls _,Opn _) -> if e=0 then  2 else e

    (** La fonction compare décrit un ordre total qui prolonge la
	relation d'inclusion. Elle retourne une valeur entière
	strictement négative si arc1 est strictement inférieur à arc2,
	une valeur entière strictement positive si arc1 est strictement
	supérieur à arc2, la valeur nulle en cas d'égalité. **)

    let compare arc1 arc2 =
      match (arc1,arc2) with
	| (St(a,b),Ex(c,d))
	| (Ex(a,b),St(c,d))
	| (Ex(a,b),Ex(c,d))
	| (St(a,b),St(c,d)) ->
	    let e = compare_initial b d
	    in
	      if e<>0 then e else compare_terminal a c
	| (Pn x,St(c,d))
	| (Pn x,Ex(c,d)) ->
	    let e = compare_initial (Cls x) d
	    in
	      if e<>0 then e else compare_terminal (Cls x) c
	| (St(c,d),Pn x)
	| (Ex(c,d),Pn x) ->
	    let e = compare_initial d (Cls x)
	    in
	      if e<>0 then e else compare_terminal c (Cls x)
	| (Co x,St(c,d))
	| (Co x,Ex(c,d)) ->
	    let e = compare_initial (Opn x) d
	    in
	      if e<>0 then e else compare_terminal (Opn x) c
	| (St(c,d),Co x)
	| (Ex(c,d),Co x) ->
	    let e = compare_initial d (Opn x)
	    in
	      if e<>0 then e else compare_terminal c (Opn x)
	| (Pn x,Pn y)
	| (Co x,Co y) -> B.compare x y
	| (Pn x,Co y) -> let e = B.compare x y in if e <> 0 then e else  1
	| (Co x,Pn y) -> let e = B.compare x y in if e <> 0 then e else -1
	| (Fu,_)
	| (_,Em) -> if (arc1=Em) || (arc2=Fu) then 0 else  1
	| (_,Fu)
	| (Em,_) -> if (arc1=Fu) || (arc2=Em) then 0 else -1

    (** least_regular_value : On donne un nom au plus petit élément de
	l'ensemble ordonné que l'on utilise **)

    let least_regular_value = B.least_regular_value

    let empty = Em 

    let full = Fu

    let bounded lf rf lb rb =
      let e = B.compare lb rb
      in
	if Pervasives.(<) e 0
	then St((if lf then Cls lb else Opn lb),(if rf then Cls rb else Opn rb))
	else 
	  if Pervasives.(>) e 0
	  then Ex((if lf then Cls lb else Opn lb),(if rf then Cls rb else Opn rb))
	  else raise Undefined

    (** Envoie une borne "fermée" sur true et une borne "ouverte" sur false. *)
    let bob b = match b with
      | Cls _ -> true
      | Opn _ -> false

    let complement arc = match arc with
      | St(Opn va,Opn vb) -> Ex(Cls vb,Cls va)
      | St(Opn va,Cls vb) -> Ex(Opn vb,Cls va)
      | St(Cls va,Opn vb) -> Ex(Cls vb,Opn va)
      | St(Cls va,Cls vb) -> Ex(Opn vb,Opn va)
      | Ex(Opn va,Opn vb) -> St(Cls vb,Cls va)
      | Ex(Opn va,Cls vb) -> St(Opn vb,Cls va)
      | Ex(Cls va,Opn vb) -> St(Cls vb,Opn va)
      | Ex(Cls va,Cls vb) -> St(Opn vb,Opn va)
      | Pn x -> Co x
      | Co x -> Pn x
      | Fu -> Em
      | Em -> Fu

    let cobounded lf rf lb rb = complement (bounded lf rf lb rb)

    let before = complement and after = complement

    let atom ?(u=Circle) x = Pn x and coatom ?(u=Circle) x = Co x

    let is_empty arc = (arc = Em)
    let is_full arc = (arc = Fu)
    let is_not_empty arc = (arc <> Em)
    let is_not_full arc = (arc <> Fu)
    let is_atomic arc = match arc with
      | Pn _ -> true
      | _    -> false
    let is_coatomic arc = match arc with
      | Co x -> true
      | _    -> false
    let glb arc = match arc with
      | St(a,b) | Ex(a,b) -> rvb a
      | Pn x | Co x -> x
      | _ -> raise Undefined
    let lub arc = match arc with
      | St(a,b) | Ex(a,b) -> rvb b
      | Pn x | Co x -> x
      | _ -> raise Undefined

    let belongs_to p arc = match arc with
      | St(a,b) ->
	  let va = rvb a and vb = rvb b and fa = bob a and fb = bob b
	  in
	    ((va < p)&&(p < vb))||(fa&&(p=va))||(fb&&(p=vb))
      | Ex(a,b) ->
	  let va = rvb a and vb = rvb b and fa = bob a and fb = bob b
	  in
	    ((va < p)||(p < vb))||(fa&&(p=va))||(fb&&(p=vb))
      | Pn x -> p=x
      | Co x -> p<>x
      | Fu -> true
      | Em -> false

    (** La fonction is_included renvoie true si arc1 est inclus dans
	arc2, false sinon. **)

    let is_included arc1 arc2 = match (arc1,arc2) with
      | (St(a,b),St(c,d)) ->
	  let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d
	  in
	    (((va=vc)&&((not(bob a))||(bob c)))||(vc < va))
	    &&(((vb=vd)&&((not(bob b))||(bob d)))||(vb < vd))
      | (Ex(a,b),Ex(c,d)) ->
	  let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d
	  in
	    (((va=vc)&&((not(bob a))||(bob c)))||(vc < va))
	    &&(((vb=vd)&&((not(bob b))||(bob d)))||(vb < vd))
      | (St(a,b),Ex(c,d)) ->
	  let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d
	  in
	    (vc < va)
	    ||(vb < vd)
	    ||((va=vc)&&((not(bob a))||(bob c)))
	    ||((vb=vd)&&((not(bob b))||(bob d)))
      | (St(a,b),Co x) ->
	  let va = rvb a and vb = rvb b
	  in
	    (x<va)
	    ||(vb<x)
	    ||((va=x)&&(not(bob a)))
	    ||((vb=x)&&(not(bob b)))
      | (Ex(a,b),Co x) ->
	  let va = rvb a and vb = rvb b
	  in
	    (((va=x)&&(not(bob a)))||(x < va))
	    &&(((vb=x)&&(not(bob b)))||(vb < x))
      | (Pn x,St(c,d)) ->
	  let vc = rvb c and vd = rvb d
	  in
	    ((x=vc)&&(bob c))
	    ||((x=vd)&&(bob d))
	    ||((vc < x)&&(x < vd))
      | (Pn x,Ex(c,d)) ->
	  let vc = rvb c and vd = rvb d
	  in
	    ((x=vc)&&(bob c))
	    ||((x=vd)&&(bob d))
	    ||(vc < x)
	    ||(x < vd)
      | (Pn x,Co y) -> x<>y
      | (_,Fu) | (Em,_) -> true
      | (Co _,Co _) | (_,Pn _) -> arc1=arc2
      | _ -> false

    let closure arc = match arc with
      | St(a,b) -> St(bound_closed a,bound_closed b)
      | Ex(a,b) -> Ex(bound_closed a,bound_closed b)
      | Co x    -> Fu
      | _       -> arc

    let interior arc = match arc with
      | St(a,b) -> St(bound_opened a,bound_opened b)
      | Ex(a,b) -> Ex(bound_opened a,bound_opened b)
      | Pn x    -> Em
      | _       -> arc

    (** La fonction in_touch renvoie true si la clôture (topologique)
	de l'un des deux arguments rencontre l'autre argument. **)

    let in_touch arc1 arc2 = match (arc1,arc2) with
      | (St(a,b),St(c,d)) ->
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
      | (St(a,b),Ex(c,d)) | (Ex(c,d),St(a,b)) ->
	  let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d
	  in
	    (
	      (vc < vb)
	      ||
		((vb=vc)&&((bob b)||(bob c)))
	    )
	    ||
	      (
		(va < vd)
		|| ((va=vd)&&((bob a)||(bob d)))
	      )
      | (Ex(a,b),Pn x) | (Pn x,Ex(a,b)) -> ((rvb a) <= x)||(x <= (rvb b))
      | (St(a,b),Pn x) | (Pn x,St(a,b)) -> ((rvb a) <= x)&&(x <= (rvb b))
      | (Pn x,Pn y)    -> x=y
      | (Em,_) | (_,Em) -> false
      | _              -> true

    (** La fonction union renvoie la réunion ensembliste des deux
	arguments et lève une exception qui transmet les deux
	arguments dans le cas où le résultat n'est pas connexe : c'est
	le cas si et seulement si in_touch arc1 arc2 = false. On peut
	donc redéfinir la fonction in_touch à partir de la fonction
	union.**)

    let union arc1 arc2 =
      match (arc1,arc2) with
	| (St(a,b),St(c,d)) ->
	  let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d and
	      fa = bob a and fb = bob b and fc = bob c and fd = bob d
	  in
	  (
	    if (vb < vc) || (vd < va) || ((vb=vc)&&(not fb)&&(not fc)) || ((va=vd)&&(not fa)&&(not fd))
	    then
	      raise (Split(arc1,arc2))
	    else
	      St((if (va < vc)||((va=vc)&&fa) then a else c),(if (vb > vd)||((vb=vd)&&fb) then b else d))
	  )
	| (Ex(a,b),Ex(c,d)) ->
	  let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d and
	      fa = bob a and fb = bob b and fc = bob c and fd = bob d
	  in
	  if (vb > vc) || (va < vd) || ((vb=vc)&&(fb||fc)) || ((va=vd)&&(fa||fd))
	  then Fu
	  else
	    if va=vd 
	    then Co va
	    else
	      if vb=vc then Co vb
	      else Ex((if (va<vc)||((va=vc)&&fa) then a else c),(if (vb>vd)||((vb=vd)&&fb) then b else d))
	| (Ex(c,d),St(a,b)) | (St(a,b),Ex(c,d)) ->
	  let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d and
	      fa = bob a and fb = bob b and fc = bob c and fd = bob d in
	  if (vb > vc) || ((vb=vc)&&(fb||fc))
	  then
	    if (va < vd) || ((va=vd)&&(fa||fd))
	    then Fu
	    else (if va=vd then Co va else Ex((if (va<vc)||((va=vc)&&fa) then a else c),d))
	  else
	    if (va < vd) || ((va=vd)&&(fa||fd))
	    then (if vb=vc then Co vb else Ex(c,if (vd<vb)||((vd=vb)&&fb) then b else d))
	    else raise (Split(arc1,arc2))
	| (St(a,b),Co x)
	| (Co x ,St(a,b)) ->
	  let va = rvb a and vb = rvb b and fa = bob a and fb = bob b in
	  if ((va < x)&&(x < vb))||(fa&&(x=va))||(fb&&(x=vb))
	  then Fu
	  else Co x
	| (Co x,Ex(a,b))
	| (Ex(a,b),Co x) ->
	  let va = rvb a and vb = rvb b and fa = bob a and fb = bob b in
	  if (va < x)||(x < vb)||(fa&&(x=va))||(fb&&(x=vb))
	  then Fu
	  else Co x
	| (St(a,b),Pn x)
	| (Pn x,St(a,b)) ->
	  let va = rvb a and vb = rvb b in
	  if (x < va)||(vb < x)
	  then raise (Split(arc1,arc2))
	  else
	    if x=va then St(Cls(x),b)
	    else (if x=vb then St(a,Cls(x)) else St(a,b))
	| (Pn x,Ex(a,b)) | (Ex(a,b),Pn x) ->
	  let va = rvb a and vb = rvb b in
	  if (x < va)&&(vb < x)
	  then raise (Split(arc1,arc2))
	  else
	    if x=va then Ex(Cls(x),b)
	    else
	      if x=vb 
	      then Ex(a,Cls(x))
	      else Ex(a,b)
	| (Co x,Co y) -> if x<>y then Fu else arc1
	| (Co y,Pn x) | (Pn x,Co y) -> if x<>y then Co y else Fu
	| (Pn x,Pn y) -> if x<>y then raise (Split(arc1,arc2)) else arc1
	| (Em,arc) | (arc,Em) -> arc
	| _ -> Fu

    let dummy_union arc1 arc2 = match arc1,arc2 with
      | Ex(a,_),St(_,b) -> Ex(a,b)
      | _ -> failwith "Arc.dummy_union cannot be used in this case" 

    (** La fonction intersection renvoie l'intersection ensembliste
	des deux arguments, dans la cas où le résultat n'est pas
	connexe, la fonction lève une exception qui transmet les deux
	composantes connexes.**)

    let intersection arc1 arc2 = match (arc1,arc2) with
      | (St(a,b),St(c,d)) ->
	let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d and
	    fa = bob a and fb = bob b and fc = bob c and fd = bob d
	in
	let cmpbc = B.compare vb vc and cmpda = B.compare vd va
	in
	if Pervasives.(>) cmpbc 0
	then
	  if Pervasives.(>) cmpda 0
	  then St((if (vc<va)||((va=vc)&&(not fa)) then a else c),(if (vb<vd)||((vb=vd)&&(not fb)) then b else d))
	  else
	    if (Pervasives.(<) cmpda 0)||(not fa)||(not fd)
	    then Em
	    else Pn va
	else
	  if (Pervasives.(<) cmpbc 0)||(not fb)||(not fc)
	  then Em
	  else Pn vb
      | (Ex(a,b),Ex(c,d)) ->
	let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d
	in
	if va < vd then raise (Split(St(a,d),Ex(c,b)))
	else
	  if vc < vb then raise (Split(St(c,b),Ex(a,d)))
	  else
	    let fa = bob a in
	    if fa && (a=d) then raise (Split(Pn va,Ex(c,b)))
	    else
	      let fb = bob b in
	      if fb && (b=c) then raise (Split(Pn vb,Ex(a,d)))
	      else
		let left = B.compare va vc and right = B.compare vb vd in
		Ex
		  (
		    (if Pervasives.(<) left 0 then c else (if Pervasives.(>) left 0 then a else (if not fa then a else c))),
		    (if Pervasives.(<) right 0 then b else (if Pervasives.(>) right 0 then d else (if not fb then b else d)))
		  )
      | (Co x,Ex(a,b)) | (Ex(a,b),Co x) ->
	let va = rvb a
	and vb = rvb b
	in
	if va < x 
	then raise (Split((St(a,Opn(x)),Ex(Opn(x),b))))
	else
	  if x < vb 
	  then raise (Split((St(Opn(x),b),Ex(a,Opn(x)))))
       	  else
	    if va = x 
	    then Ex(Opn(x),b)
	    else
	      if vb = x 
	      then Ex(a,Opn(x))
	      else Ex(a,b)
      | (Ex(c,d),St(a,b))
      | (St(a,b),Ex(c,d)) ->
	(
	  let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d
	  in
	  match
	    (
	      ((vc < vb)||((vb=vc)&&(bob b)&&(bob c)))
		,
	      ((va < vd)||((va=vd)&&(bob a)&&(bob d)))
	    )
	  with
	    | (true,false) -> (if vb=vc then Pn vb else St((if (vc<va)||((va=vc)&&(not(bob a))) then a else c),b))
	    | (false,true) -> (if va=vd then Pn va else St(a,((if (vb<vd)||((vb=vd)&&(not(bob b))) then b else d))))
	    | (true,true) -> raise 
      (Split((if va=vd then Pn va else St(a,d)),(if vb=vc then Pn vb else St(c,b))))
	    | _ -> Em
	)
      | (Pn x,St(a,b))
      | (St(a,b),Pn x) ->
	let va = rvb a and vb = rvb b in
	if ((va=x)&&(bob a))||((vb=x)&&(bob b))||((va < x)&&(x < vb))
	then Pn x
	else Em
      | (Pn x,Ex(a,b))
      | (Ex(a,b),Pn x) ->
	let va = rvb a and vb = rvb b in
	if ((va=x)&&(bob a))||((vb=x)&&(bob b))||((va < x)||(x < vb))
	then Pn x
	else Em
      | (Pn x,Pn y) -> if x=y then arc1 else Em
      | (Co x,Co y) -> if x=y then arc1 else
	  if x < y
	  then raise (Split((St(Opn(x),Opn(y)),Ex(Opn(y),Opn(x)))))
	  else raise (Split((St(Opn(y),Opn(x)),Ex(Opn(x),Opn(y)))))
      | (Co x,St(a,b))
      | (St(a,b),Co x) ->
	let va = rvb a and vb = rvb b in
	if (x < va)||(vb < x)
	then St(a,b)
	else
	  if va=x 
	  then St(Opn(x),b)
	  else
	    if vb=x 
	    then St(a,Opn(x))
	    else raise (Split((St(a,Opn(x)),St(Opn(x),b))))
      | (Co y,Pn x) | (Pn x,Co y) -> if x=y then Em else Pn x
      | (Fu,arc) | (arc,Fu) -> arc
      | _ -> Em



    let is_included_standard arc1 arc2 =
      try intersection arc1 arc2 = arc1
      with Split _ -> false

    (** Renvoie l'ensemble des points du second argument que l'on peut
	atteindre via un chemin dirigé partant d'un point du premier
	argument et dont la trace est incluse dans la réunion des deux
	arguments. **)

    (* Attention, il pourrait y avoir des problèmes avec la borne
       B.least_regular_value *)

    let in_the_future_of arc1 arc2 = match (arc1,arc2) with
      | (Ex(a,b),Ex(c,d)) ->
        let va = rvb a and vb = rvb b and vc = rvb c in
        Ex((
          if (va<vc) || (vc<vb) || ((va=vc)&&(bob a)) || ((vc=vb)&&((bob b)||(bob c))) 
          then c else a),d)
      | (St(a,b),St(c,d)) ->
	let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d and
	    fa = bob a and fb = bob b and fc = bob c and fd = bob d in
	if (vb<vc)||(vd<va)||((vb=vc)&&(not(fb||fc)))||((va=vd)&&(not(fa&&fd)))
	then Em
	else
	  if va<>vd
	  then St((if (va<vc)||(va=vc&&(not fc)) then c else a),d)
	  else Pn va
      | (St(a,b),Ex(c,d)) ->
	let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d and
	    fa = bob a and fb = bob b and fc = bob c and fd = bob d in
	if (vc<vb) || ((vc=vb)&&(fb||fc))
	then Ex((if (va<vc)||((va=vc)&&(not fc)) then c else a),d)
	else
	  if (va<vd)
 	  then St(a,d)
	  else
	    if (va=vd)&&fa&&fd
	    then Pn va
	    else Em
      | (Ex(a,b),St(c,d)) ->
	let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d and
	    fa = bob a and fb = bob b and fc = bob c and fd = bob d in
	if (vc<vb)||(va<vc)||((va=vc)&&fa)||((vc=vb)&&(fc||fb))
	then arc2
	else
	  if va<vd
	  then St(a,d)
	  else
	    if (va=vd)&&fa&&fd
	    then Pn va
	    else Em
      | (Ex(a,b),Pn x) ->
	let va = rvb a and vb = rvb b and fa = bob a in
	if ((va<x)||(x<vb))||((x=va)&&fa)||(vb=x)
	then arc2
	else Em
      | (Ex(a,b),Co x) ->
	let va = rvb a and vb = rvb b
	in
	if (va <= x) || (x < vb) || (x=vb && bob b) (*bugfix 2011/04/19 18:29*)
	then arc2
	else Ex(a,Opn x)
      | (Pn x,St(c,d)) ->
	let vc = rvb c and vd = rvb d and fd = bob d
	in
	if (x<vd)&&(vc<x)
	then St(Cls x,d)
	else
	  if vc=x
 	  then arc2
	  else
	    if (x=vd)&&fd
	    then Pn x
	    else Em
      | (Pn x,Ex(c,d)) ->
	let vc = rvb c and vd = rvb d and fd = bob d
	in
	if vc <= x
	then Ex((if vc=x then c else Cls(x)),d)
	else
	  if (x<vd)
	  then St(Cls x,d)
	  else
	    if (x=vd)&&fd
 	    then Pn x
	    else Em
      | (St(a,b),Co x) -> (* bug when x=zero *)
	let va = rvb a and vb = rvb b and fb = bob b
	in
	if ((va<x)&&(x<vb))||(x=va)||((vb=x)&&fb)
	then arc2
	else (*St(a,Opn x)*) (*bug here*)
	  if x<va
	  then Ex(a,Opn x)
	  else St(a,Opn x)
      | (St(a,b),Pn x) ->
	let va = rvb a and vb = rvb b and fa = bob a
	in
	if ((va<x)&&(x<vb))||((x=va)&&fa)||(vb=x)
	then arc2
	else Em
      | (Pn x,Pn y) -> if x=y then arc1 else Em
      | (Pn x,Co y) ->
	let e = B.compare x y
	in
	if Pervasives.(<) e 0
	then St(Cls x,Opn y)
	else
	  if Pervasives.(<) 0 e
	  then Ex(Cls x,Opn y)
	  else arc2
      | (Co _,_) | (Fu,_) -> arc2
      | (_,Em) | (Em,_) -> Em
      | (_,Fu) -> Fu


    let in_the_future_of_point p arc = in_the_future_of (atom p) arc

    (** Renvoie l'ensemble des points du second argument que l'on peut
	atteindre via un chemin anti-dirigé partant d'un point du
	premier argument et dont la trace est incluse dans la réunion
	des deux arguments. **)

    (* La fonction in_the_past_of se déduit de la fonction
       in_the_future_of en considérant l'ordre opposé à celui donné
       sur les valeurs réguières. Concrètement il suffit dans le corps
       de la fonction in_the_future_of, à gauche des "->" du "match"
       d'effectuer les échanges de rôles suivants :

       a <-> b et c <-> d

       puis à droite des "->" :

       d'échanger l'ordre de comparaison sur les valeurs régulières i.e. d'échanger < et >

       d'échanger l'ordre des bornes dans les constructeurs St et Ex

    *)

    let in_the_past_of arc1 arc2 = match (arc1,arc2) with
      | (Ex(b,a),Ex(d,c)) ->
	let va = rvb a and vb = rvb b and vc = rvb c in
	Ex(d,(if (va>vc) || (vc>vb) || ((va=vc)&&(bob a)) || ((vc=vb)&&((bob b)||(bob c))) then c else a))
      | (St(b,a),St(d,c)) ->
	let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d and
	    fa = bob a and fb = bob b and fc = bob c and fd = bob d in
	if (vb>vc)||(vd>va)||((vb=vc)&&(not(fb||fc)))||((va=vd)&&(not(fa&&fd)))
	then Em
	else
	  if va<>vd
	  then St(d,(if (va>vc)||(va=vc&&(not fc)) then c else a))
	  else Pn va
      | (St(b,a),Ex(d,c)) ->
	let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d and
	    fa = bob a and fb = bob b and fc = bob c and fd = bob d in
	if (vc>vb) || ((vc=vb)&&(fb||fc))
	then Ex(d,(if (va>vc)||((va=vc)&&(not fc)) then c else a))
	else
	  if (va>vd)
 	  then St(d,a)
	  else
	    if (va=vd)&&fa&&fd
	    then Pn va
	    else Em
      | (Ex(b,a),St(d,c)) ->
	let va = rvb a and vb = rvb b and vc = rvb c and vd = rvb d and
	    fa = bob a and fb = bob b and fc = bob c and fd = bob d in
	if (vc>vb)||(va>vc)||((va=vc)&&fa)||((vc=vb)&&(fc||fb))
	then arc2
	else
	  if va>vd
	  then St(d,a)
	  else
	    if (va=vd)&&fa&&fd
	    then Pn va
	    else Em
      | (Ex(b,a),Pn x) ->
	let va = rvb a and vb = rvb b and fa = bob a in
	if ((va>x)||(x>vb))||((x=va)&&fa)||(vb=x)
	then arc2
	else Em
      | (Ex(b,a),Co x) -> (* 02/03/2011 bug found and fixed *)
	let va = rvb a and vb = rvb b in
	if x<=va || vb<x || (x=vb&&(bob b))
	then arc2
	else Ex(Opn x,a)
      (*if x<=rvb a || (x=rvb b&&(bob b)) || rvb b<x
	then arc2
	else Ex(Opn x,a)*)
      | (Pn x,St(d,c)) ->
	let vc = rvb c and vd = rvb d and fd = bob d in
	if (x>vd)&&(vc>x)
	then St(d,Cls x)
	else
	  if vc=x
 	  then arc2
	  else
	    if (x=vd)&&fd
	    then Pn x
	    else Em
      | (Pn x,Ex(d,c)) ->
	let vc = rvb c and vd = rvb d and fd = bob d in
	if vc >= x
	then Ex(d,(if vc=x then c else Cls x))
	else
	  if (x>vd)
	  then St(d,Cls x)
	  else
	    if (x=vd)&&fd
 	    then Pn x
	    else Em
      | (St(b,a),Co x) -> (* 03/02/2011 bug found and fixed *)
	let va = rvb a and vb = rvb b and fb = bob b in
	if ((va>x)&&(x>vb))||(x=va)||((vb=x)&&fb)
	then arc2
	else (* fix up *)
	  if x<=vb
	  then St(Opn x,a)
	  else Ex(Opn x,a)
      | (St(b,a),Pn x) ->
	let va = rvb a and vb = rvb b and fa = bob a in
	if ((va>x)&&(x>vb))||((x=va)&&fa)||(vb=x)
	then arc2
	else Em
      | (Pn x,Pn y) -> if x=y then arc1 else Em
      | (Pn x,Co y) ->
	let e = B.compare x y in
	if Pervasives.(>) e 0
	then St(Opn y,Cls x)
	else
	  if Pervasives.(>) 0 e
	  then Ex(Opn y,Cls x)
	  else arc2
      | (Co _,_) | (Fu,_) -> arc2
      | (_,Em) | (Em,_) -> Em
      | (_,Fu) -> Fu

    let in_the_past_of_point p arc = in_the_past_of (atom p) arc

    (*debuggin display*)

    module DisplayMode = struct

      let debug ?(sd=("(",")")) arc = match arc with
	| St(a,b) -> Printf.sprintf "St(%s,%s)"
	  (Printf.sprintf "%s %s" (if bob a then "Cls" else "Opn") (B.string_of (rvb a)))
	  (Printf.sprintf "%s %s" (if bob b then "Cls" else "Opn") (B.string_of (rvb b)))
	| Ex(a,b) -> Printf.sprintf "Ex(%s,%s)"
	  (Printf.sprintf "%s %s" (if bob a then "Cls" else "Opn") (B.string_of (rvb a)))
	  (Printf.sprintf "%s %s" (if bob b then "Cls" else "Opn") (B.string_of (rvb b)))
	| Pn x -> "Pn "^(B.string_of x)
	| Co x -> "Co "^(B.string_of x)
	| Fu -> "Fu"
	| Em -> "Em"

      let pretty ?(sd=("(",")")) arc = match arc with
	| St(a,b)
	| Ex(a,b) ->
	  let x = rvb a and y = rvb b and left  = if bob a then "(" else ")" and right = if bob b then ")" else "(" in
	  left^(B.string_of x)^","^(B.string_of y)^right
	| Pn x -> (fst sd)^(B.string_of x)^(snd sd)
	| Co x -> (snd sd)^(B.string_of x)^(fst sd)
	| Fu -> "S"
	| Em -> "@"

    end

    let string_of = DisplayMode.debug

    let initial b x =
      if x <> B.least_regular_value
      then let bnd = if b then Cls x else Opn x in
	   St((Cls B.least_regular_value),bnd)
      else
        if b
        then Pn B.least_regular_value
        else Em

    let terminal b x =
      if x<>least_regular_value
      then Ex((if b then Cls x else Opn x),(Opn B.least_regular_value))
      else
        if b
        then Fu
        else Co least_regular_value

    (* La fonction qui suit n'est utilisée que pour vérifier la
       cohérence des fonction intersection et union *)

    let union_standard arc1 arc2 =
      if in_touch arc1 arc2
      then complement (intersection (complement arc1) (complement arc2))
      else raise (Split (arc1,arc2))

    let meet = intersection
    let join = union

    (* Dans cette réalisation, la fonction normalize est inutile
       puisque toutes les fonctions sont écrites de façon à ce que si les
       entrées sont en forme normale, alors les sorties le sont aussi. *)

    let not_disjoint arc1 arc2 = (intersection arc1 arc2) <> Em

    let normalize arc = arc

  end
