exception Unbounded
exception Undefined

(*--------------------------------------------------------------------------------------*)
(* regular_value : "regular values", désigne l'ensemble des valeurs dans lequel on      *)
(* prend les bornes finies. Actuellement, ce sont des valeurs entières mais on pourrait *)
(* imaginer utiliser des flottants ou même des rationnels.                              *)
(*--------------------------------------------------------------------------------------*)

type regular_value = int

let least_regular_value = 0

let compare_regular_values rv1 rv2 = (rv1 - rv2)

let max_regular_value rv1 rv2 = Pervasives.max rv1 rv2

let min_regular_value rv1 rv2 = Pervasives.min rv1 rv2

(*--------------------------------------------------------------------------------------*)
(* bound : les bornes d'un intervalle peuvent être ouvert fermé ou bien "illimité" en   *)
(* sens que l'intervalle que l'on décrit est alors un segment terminal de l'ensemble    *)
(* ordonné avec lequel on travaille.                                                    *)
(*--------------------------------------------------------------------------------------*)

type bound = 
  | Opn of regular_value
  | Cls of regular_value
  | Unrchbl_max (* la borne "plus infini ouverte" *)


(*--------------------------------------------------------------------------------------*)
(* t : c'est le type "intervalle", c'est-à-dire qu'un intervalle peut être              *)
(* "non-dégénéré" auquel cas on utilise le constructeur Itv, "dégénéré non vide" auquel *)
(* cas on utilise le constructeur Di ou bien "vide" auquel cas on utilise le            *)
(* constructeur Ei.                                                                     *)
(*--------------------------------------------------------------------------------------*)

type 
  non_degenerate_interval = { lft_bnd : bound ; rgt_bnd : bound }
and 
  t =
  | Itv of non_degenerate_interval (* Intervalle non dégénéré      *) 
  | Di of regular_value            (* Intervalle dégénéré non vide *)
  | Ei                             (* Intervalle vide              *)

let is_singleton it = match it with
  | Di(_) -> true
  | _     -> false

exception Split of (t list)

(*--------------------------------------------------------------------------------------*)
(* regular_value_of_bounds : les bornes d'un intervalle peuvent être "finies" ou non, dans le *)
(* premier cas on renvoie la "valeur régulière" associée à cette borne, celle-ci fait   *)
(* partie de l'ensemble ordonné avec lequel on travaille.                               *)
(*--------------------------------------------------------------------------------------*)

let regular_value_of_bounds b = match b with 
  | Opn(n) -> n 
  | Cls(n) -> n 
  | Unrchbl_max -> raise Unbounded

(*--------------------------------------------------------------------------------------*)
(* bound_reverse : d'un certaine manière, cette fonction renvoie le complémentaire d'un *)
(* segment terminal de l'ensemble ordonné avec lequel on travaille. Cette fonction est  *)
(* principalement utilisée pour le test d'inclusion.                                    *)
(*--------------------------------------------------------------------------------------*)

(** Chaque borne peut-être "ouverte" ou "fermée", bound_reverse échange "ouvert" et "fermé".*)
let bound_reverse b = match b with 
  | Opn(a)      -> Cls(a)
  | Cls(a)      -> Opn(a)
  | Unrchbl_max -> Unrchbl_max

(** Chaque borne peut-être "ouverte" ou "fermée", bound_opened met la borne dans l'état "ouvert".*)
let bound_opened b = match b with 
  | Opn(a)      -> Opn(a)
  | Cls(a)      -> Opn(a)
  | Unrchbl_max -> Unrchbl_max

(** Chaque borne peut-être "ouverte" ou "fermée", bound_opened met la borne dans l'état "fermé".*)
let bound_closed b = match b with 
  | Opn(a)      -> Cls(a)
  | Cls(a)      -> Cls(a)
  | Unrchbl_max -> Unrchbl_max

(*--------------------------------------------------------------------------------------*)
(* compare_bounds : une fonction de comparaison qui suit la spécification du langage    *)
(* Ocaml pour l'utilisation des foncteurs Set et Map (par exemple).                     *)
(*--------------------------------------------------------------------------------------*)

let compare_bounds b1 b2 = 
  let aux a b = 
    if a < b 
    then 
      -((b - a)+2) 
    else 
      ((a - b)+2)
  in 
    if (b1 = b2)
    then 
      0 
    else
      ( 
	match (b1,b2) with
	  | (Opn(a),Opn(b)) -> aux a b 
	  | (Cls(a),Cls(b)) -> aux a b 
	  | (Opn(a),Cls(b)) -> if a = b then -2 else (aux a b)
	  | (Cls(a),Opn(b)) -> if a = b then 2 else (aux a b)
	  | (b,Unrchbl_max) -> -1
	  | (Unrchbl_max,b) -> 1
      )

(*--------------------------------------------------------------------------------------*)
(* least_regular_value : On donne un nom au plus petit élément de l'ensemble ordonné que  *)
(* l'on utilise.                                                                        *)
(*--------------------------------------------------------------------------------------*)

let least_regular_value = 0 

let empty = Ei and full = Itv({lft_bnd = Cls(least_regular_value) ; rgt_bnd = Unrchbl_max})

(*--------------------------------------------------------------------------------------*)
(* bool_of_bounds :                                                                     *)
(*--------------------------------------------------------------------------------------*)

(** Envoie une borne "fermée" sur true et une borne "ouverte" sur false. *)

let bool_of_bounds b = match b with 
  | Cls(n) -> true
  | _      -> false (* un intervalle ne peut pas être fermé à l'infini *)

let lft_bool_of_bound it =
  match it with
    | Itv({lft_bnd = Cls(_) ; rgt_bnd = _ }) -> true 
    | _ -> false
and rgt_bool_of_bound it =
  match it with
    | Itv({lft_bnd = _ ; rgt_bnd = Cls(_) }) -> true 
    | _ -> false


(*--------------------------------------------------------------------------------------*)
(* normalize : cette fonction "normalise" une valeur de type t ("intervalle") au sens   *)
(* où si cette valeur est, par exemple, décrite avec le constructeur Itv avec une borne *)
(* gauche inférieure à la droite, cette donnée est "transformée" en Ei puisqu'elle      *)
(* représente alors l'ensemble vide.                                                    *)
(*--------------------------------------------------------------------------------------*)

(* Forme normale d'un intervalle, autrement renvoie un contructeur
    différent selon que : 

    - La borne inférieure de l'intervalle est strictement plus petite que
    la borne supérieure

    - Les bornes
    inférieures et supérieures sont égales 

    - l'intervalle est vide. *)

let normalize it = match it with 
  | Itv({lft_bnd = a ; rgt_bnd = b}) -> 
      (
	try
          let x = regular_value_of_bounds a
	  in
	    (
	      try
		(
		  let y = regular_value_of_bounds b
		  in 
		    if x < y 
		    then 
		      it
		    else
		      (
			if x > y 
			then 
			  empty
			else
			  (
			    if ( (bool_of_bounds a) && (bool_of_bounds b) )
			    then 
                              Di(x) 
			    else 
                              empty
			  )
		      )
		)
	      with
		| Unbounded -> it
	    )
	with 
	  | Unbounded -> empty
      )
  | _ -> it


(*--------------------------------------------------------------------------------------*)
(* compare : une fonction de comparaison qui suit la spécification du langage Ocaml     *)
(* pour l'utilisation des foncteurs Set et Map (par exemple).                           *)
(*--------------------------------------------------------------------------------------*)

let compare it1 it2 =
  let aux it1 it2 = match it1 with 
    | Itv({ lft_bnd = a  ; rgt_bnd = b }) -> 
	(
	  match it2 with 
	    | Itv({ lft_bnd = c  ; rgt_bnd = d }) -> 
		(
		  let e = compare_bounds a c 
		  in 
		    (
		      if e <> 0
		      then 
			e
		      else
			compare_bounds b d
		    )
		)
	    | Di(n) ->
		(
		  let e = compare_bounds a (Cls(n))
		  in 
		    (
		      if e <> 0
		      then 
			e
		      else
			compare_bounds b (Cls(n))
		    )
		) 
	    | Ei -> 1
	)
    | Di(n) ->
	(
	  match it2 with 
	    | Itv({ lft_bnd = c ; rgt_bnd = d }) -> 
		(
		  let e = compare_bounds (Cls(n)) c 
		  in 
		    (
		      if e <> 0
		      then 
			e
		      else
			compare_bounds (Cls(n)) d
		    )
		)
	    | Di(m) -> n - m 
	    | Ei -> 1
	)
    | Ei -> 
	(
	  match it2 with 
	    | Ei -> 0
	    | _ -> -1
	)
  in 
    aux (normalize it1) (normalize it2)

(*--------------------------------------------------------------------------------------*)
(* atom :                                                                               *)
(*--------------------------------------------------------------------------------------*)

let atom n = Di(n)

(*--------------------------------------------------------------------------------------*)
(* terminal : est mis pour "terminal segment".                                          *)
(*--------------------------------------------------------------------------------------*)

let terminal b n = 
  let a = (if b then Cls(n) else Opn(n))
  in
    (Itv({lft_bnd = a ; rgt_bnd = Unrchbl_max }))

(*--------------------------------------------------------------------------------------*)
(* initial : est mis pour "initial segment".                                            *)
(*--------------------------------------------------------------------------------------*)

let initial b n = 
  if n = least_regular_value
  then
    (
      if b
      then
	Di(n)
      else
	empty
    )
  else
    (    let r = (if b then Cls(n) else Opn(n))
	 in
	   (Itv({lft_bnd = Cls(least_regular_value) ; rgt_bnd = r}))
    )

(*--------------------------------------------------------------------------------------*)
(* bounded : est mis pour "bounded segment".                                            *)
(*--------------------------------------------------------------------------------------*)

let bounded lf rf lb rb = 
  let 
      a = (if lf then Cls(lb) else Opn(lb))
  and 
      b = (if rf then Cls(rb) else Opn(rb))
  in
    match (a,b) with 
      | (Cls(_),Cls(_)) -> 
	  (
	    if lb < rb 
	    then 
	      (Itv({lft_bnd = a ; rgt_bnd = b})) 
	    else
	      (
		if lb > rb
		then 
		  empty
		else
		  Di(lb)
	      )
	  )
      | _ ->
	  (
	    if lb < rb 
	    then 
	      (Itv({lft_bnd = a ; rgt_bnd = b})) 
	    else
	      empty
	  )


let is_atomic it = match it with
  | Di(_) -> true
  | _     -> false 

(*--------------------------------------------------------------------------------------*)

let belongs_to x it =
  match it with 
    | Itv({ lft_bnd = c ; rgt_bnd = d }) -> 
	let c' = regular_value_of_bounds c 
	in
	  (
	    try 
	      (
		let d' = regular_value_of_bounds d 
		in
		  ((x=c')&&(bool_of_bounds c))||((x=d')&&(bool_of_bounds d))||((c' < x)&&(x < d'))
	      )
	    with
	      | Unbounded -> ((x=c')&&(bool_of_bounds c)) || (c' < x)
          )
    | Di(n) -> x = n
    | Ei -> false 

(*--------------------------------------------------------------------------------------*)

let is_included it1 it2 =
  let left_aux s1 s2 =   
    (
      try
	let v1 = regular_value_of_bounds s1
	and v2 = regular_value_of_bounds s2
	in 
	  ( v1 > v2) 
	  || ( (v1 = v2) && ( (not(bool_of_bounds s1)) || (bool_of_bounds s2) ) )
      with
	| Unbounded -> ( (not(s2 = Unrchbl_max)) || (s1 = Unrchbl_max) ) 
    )
  and right_aux s1 s2 =   
    (
      try
	let v1 = regular_value_of_bounds s1
	and v2 = regular_value_of_bounds s2 
	in
	  ( v1 < v2) 
	  || ( (v1 = v2) && ( (not(bool_of_bounds s1)) || (bool_of_bounds s2) ) )
      with
	| Unbounded -> (s2 = Unrchbl_max)
    ) 
  in
  let aux it1 it2 = 
    match (it1,it2) with 
      | ( Itv{ lft_bnd = a ; rgt_bnd = b } , Itv{ lft_bnd = c ; rgt_bnd = d }) -> 
	  ( (left_aux a c) && (right_aux b d) )
      | (Di(n),Itv({ lft_bnd = c ; rgt_bnd = d }))                          -> 
	  let c' = regular_value_of_bounds c 
	  in
	    (
	      try 
		(
		  let d' = regular_value_of_bounds d 
		  in
		    ((n=c')&&(bool_of_bounds c))||((n=d')&&(bool_of_bounds d))||((c' < n)&&(n < d'))
		)
	      with
		| Unbounded -> ((n=c')&&(bool_of_bounds c)) || (c' < n)
            )
      | (x,Di(n)) -> x = Di(n)
      | (Ei,_)    -> true
      | _         -> false 
  in
    aux (normalize it1) (normalize it2)



(*--------------------------------------------------------------------------------------*)
(* glb : greatest lower bound i.e. "borne inférieure".                                  *)
(*--------------------------------------------------------------------------------------*)

let glb it = 
  let aux it = 
    match it with 
      | Itv({ lft_bnd = x ; rgt_bnd = _ }) -> regular_value_of_bounds x
      | Di(n) -> n
      | Ei -> raise Unbounded
  in 
    aux (normalize it)


(*--------------------------------------------------------------------------------------*)
(* lub : least upper bound i.e. "borne supérieure".                                     *)
(*--------------------------------------------------------------------------------------*)

let lub it = 
  let aux it = 
    match it with 
      | Itv({ lft_bnd = _ ; rgt_bnd = x }) -> regular_value_of_bounds x
      | Di(n) -> n
      | Ei -> least_regular_value
  in
    aux (normalize it)


(*--------------------------------------------------------------------------------------*)
(* is_not_empty : renvoie "true" si l'intervalle n'est pas vide, "false" dans le cas    *)
(* contraire.                                                                           *)
(*--------------------------------------------------------------------------------------*)

let is_not_empty it = ((normalize it) <> empty)


let is_not_full it = ((normalize it) <> full)


(*--------------------------------------------------------------------------------------*)
(* meet : renvoie l'intersection des intervalles it1 et it2 (au format normalisé).      *)
(*--------------------------------------------------------------------------------------*)

let meet it1 it2 = 
  let aux it1 it2 = 
    match (it1,it2) with 
      | (Ei,_)
      | (_,Ei) -> empty
      | (Di(n),Di(m)) -> if n = m then Di(n) else empty
      | (Di(n),Itv{ lft_bnd = c ; rgt_bnd = d }) 
      | (Itv{ lft_bnd = c ; rgt_bnd = d },Di(n)) -> 
	  let c' = regular_value_of_bounds c
	  in
	    (
	      try
		let d' = regular_value_of_bounds d
		in
		  (
		    if (((n=c')&&(bool_of_bounds c))||((n=d')&&(bool_of_bounds d))||((c' < n)&&(n < d')))
		    then 
		      Di(n)
		    else 
		      empty
		  )
	      with
		| Unbounded -> 
		    (
		      if (((n=c')&&(bool_of_bounds c))||(c' < n))
		      then 
			Di(n)
		      else 
			empty
		    )
	    )
      | (Itv({lft_bnd = a ; rgt_bnd = b }),Itv({lft_bnd = c ; rgt_bnd = d })) -> 
	  let 
	      meet_inf b1 b2 = 
	    match (b1,b2) with
	      | (Opn(x),Opn(y)) -> Opn(max x y)
	      | (Cls(x),Cls(y)) -> Cls(max x y)
	      | (Opn(x),Cls(y))
	      | (Cls(y),Opn(x)) -> if x < y then Cls(y) else Opn(x)
	      | _               -> Unrchbl_max
	  and
	      meet_sup b1 b2 = 
	    match (b1,b2) with 
	      | (Opn(x),Opn(y)) -> Opn(min x y)
	      | (Cls(x),Cls(y)) -> Cls(min x y)
	      | (Opn(x),Cls(y))
	      | (Cls(y),Opn(x)) -> if y < x then Cls(y) else Opn(x)
	      | (Unrchbl_max,z) 
	      | (z,Unrchbl_max) -> z
	  in
	    (
	      let 
                  left = (meet_inf a c)
	      and 
		  right = (meet_sup b d)
	      in
                normalize (Itv({ lft_bnd = left ; rgt_bnd = right }))
	    )
  in
    aux (normalize it1) (normalize it2) 


(*--------------------------------------------------------------------------------------*)
(* join : renvoie l'enveloppe convexe des intervalles it1 et it2 (au format normalisé). *)
(*--------------------------------------------------------------------------------------*)

let join it1 it2 = 
  let aux it1 it2 = 
    match (it1,it2) with 
      | (Ei,z)
      | (z,Ei) -> z 
      | (Di(n),Di(m)) -> if n = m then Di(n) else Itv({lft_bnd = Cls(min n m) ; rgt_bnd = Cls(max n m)})
      | (Di(n),Itv{ lft_bnd = c ; rgt_bnd = d }) 
      | (Itv{ lft_bnd = c ; rgt_bnd = d },Di(n)) -> 
	  let c' = regular_value_of_bounds c
	  in
	    (
	      try
		let d' = regular_value_of_bounds d
		in
		  (
		    if (n <= c')
		    then 
		      Itv({lft_bnd = Cls(n) ; rgt_bnd = d})
		    else 
		      (
			if (d' <= n)
			then
			  Itv({lft_bnd = c ; rgt_bnd = Cls(n)})
			else
			  Itv({ lft_bnd = c ; rgt_bnd = d })
		      )
		  )
	      with
		| Unbounded -> 
		    (
		      if (n <= c')
		      then 
			Itv({lft_bnd = Cls(n) ; rgt_bnd = d})
		      else 
			Itv({lft_bnd = c ; rgt_bnd = d})
		    )
	    )
      | (Itv({lft_bnd = a ; rgt_bnd = b }),Itv({lft_bnd = c ; rgt_bnd = d })) -> 
	  let 
	      join_inf b1 b2 = 
	    match (b1,b2) with
	      | (Opn(x),Opn(y)) -> Opn(min x y)
	      | (Cls(x),Cls(y)) -> Cls(min x y)
	      | (Cls(x),Opn(y))
	      | (Opn(y),Cls(x)) -> if y < x then Opn(y) else Cls(x)
	      | (Unrchbl_max,z) 
	      | (z,Unrchbl_max) -> z
	  and
	      join_sup b1 b2 = 
	    match (b1,b2) with 
	      | (Opn(x),Opn(y)) -> Opn(max x y)
	      | (Cls(x),Cls(y)) -> Cls(max x y)
	      | (Cls(x),Opn(y))
	      | (Opn(y),Cls(x)) -> if x < y then Opn(y) else Cls(x)
	      | _               -> Unrchbl_max
	  in
	    (
	      let 
                  left = (join_inf a c)
	      and 
		  right = (join_sup b d)
	      in
                normalize (Itv({ lft_bnd = left ; rgt_bnd = right }))
	    )
  in
    aux (normalize it1) (normalize it2) 


(*--------------------------------------------------------------------------------------*)
(* not_disjoint : renvoie true si les deux intervalles ne sont pas disjoints, false     *)
(* dans le cas contraire. Les intervalles it1 et it2 doivent être normalisés.           *)
(*--------------------------------------------------------------------------------------*)

let not_disjoint it1 it2 = is_not_empty (meet it1 it2)


(*--------------------------------------------------------------------------------------*)
(* in_touch : renvoie true lorsque la réunion des deux intervalles donnés en argument   *)
(* est connexe. Les arguments doivent être passé sous forme normalisée.                 *)
(*--------------------------------------------------------------------------------------*)

(*

Cette version de la fonction est fausse.

let in_touch it1 it2 = 
  ((meet it1 it2) <> empty) || 
    (
      match (it1,it2) with
        | (Itv({lft_bnd = _       ; rgt_bnd = Cls(b1)}),Itv({lft_bnd = Opn(b2) ; rgt_bnd = _       })) 
        | (Itv({lft_bnd = _       ; rgt_bnd = Opn(b1)}),Itv({lft_bnd = Cls(b2) ; rgt_bnd = _       })) 
        | (Itv({lft_bnd = Cls(b1) ; rgt_bnd = _      }),Itv({lft_bnd = _       ; rgt_bnd = Opn(b2) })) 
        | (Itv({lft_bnd = Opn(b1) ; rgt_bnd = _      }),Itv({lft_bnd = _       ; rgt_bnd = Cls(b2) }))
        | (Di(b1)                                      ,Itv({lft_bnd = Opn(b2) ; rgt_bnd = _       })) 
        | (Di(b1)                                      ,Itv({lft_bnd = Cls(b2) ; rgt_bnd = _       }))
        | (Itv({lft_bnd = Opn(b1) ; rgt_bnd = _      }),Di(b2)                                       ) 
        | (Itv({lft_bnd = Cls(b1) ; rgt_bnd = _      }),Di(b2)                                       )
	| (Di(b1)                                      ,Di(b2)                                       ) -> (b1=b2)
        | _ -> false
    )


*)

let above x it =
  meet it (Itv({lft_bnd = Cls(x) ; rgt_bnd = Unrchbl_max}))


let below x it =
  meet it (Itv({lft_bnd = Unrchbl_max ; rgt_bnd = Cls(x)}))


let strictly_above x it =
  meet it (Itv({lft_bnd = Opn(x) ; rgt_bnd = Unrchbl_max}))


let strictly_below x it =
  meet it (Itv({lft_bnd = Unrchbl_max ; rgt_bnd = Opn(x)}))


let string_of it =
  match it with 
    | Itv({lft_bnd = a ; rgt_bnd = b}) ->
	(
	  let 
	      lb = if bool_of_bounds a then "[" else "]"
	  and 
	      rb = if bool_of_bounds b then "]" else "["
	  and 
	      a' = regular_value_of_bounds a
	  in
	    try
	      lb^(string_of_int a')^","^(string_of_int (regular_value_of_bounds b))^rb
	    with
	      | Unbounded -> lb^(string_of_int a')^","^"-"^rb
	)
    | Di(n) -> "{"^(string_of_int n)^"}"
    | Ei -> "Empty"


 let mirror_string_of it =
  match it with 
    | Itv({lft_bnd = a ; rgt_bnd = b}) ->
	(
	  let 
	      lb = if bool_of_bounds a then "[" else "]"
	  and 
	      rb = if bool_of_bounds b then "]" else "["
	  and 
	      a' = regular_value_of_bounds a
	  in
	    try
	      rb^(string_of_int (regular_value_of_bounds b))^","^(string_of_int a')^lb
	    with
	      | Unbounded -> "]"^"-"^(string_of_int a')^","^lb
	)
    | Di(n) -> "}"^(string_of_int n)^"{"
    | Ei -> "Empty"

let interior it =
  match it with 
    | Itv({lft_bnd = a ; rgt_bnd = b}) -> 
	if a = Unrchbl_max 
	then 
	  Ei 
	else 
	  Itv({lft_bnd = bound_opened a ; rgt_bnd = bound_opened b})
    | _ -> empty

let closure it =
  match it with 
    | Itv({lft_bnd = a ; rgt_bnd = b}) -> 
	if a = Unrchbl_max 
	then 
	  Ei 
	else 
	  Itv({lft_bnd = bound_closed a ; rgt_bnd = bound_closed b})
    | _ -> it



let in_touch_version_1 it1 it2 = (((meet (closure it1) it2) <> empty) || ((meet it1 (closure it2)) <> empty))



let in_touch_version_2 it1 it2 =
  match (it1,it2) with
    | ( Itv({lft_bnd = a ; rgt_bnd = b}) , Itv({lft_bnd = c ; rgt_bnd = d}) ) ->  
	(
	  try 
	    let x  = regular_value_of_bounds a
	    and x' = regular_value_of_bounds c
	    in
	      (
		try
		  let y  = regular_value_of_bounds b
		  in
		    (
		      try
			let y' = regular_value_of_bounds d
			in
			  (not(y < x'))&&((y <> x')||(bool_of_bounds b)||(bool_of_bounds c))
			  && (not(y' < x))&&((y' <> x)||(bool_of_bounds a)||(bool_of_bounds d))
		      with
			| _ ->
			    ((not(y < x'))&&((y <> x')||(bool_of_bounds b)||(bool_of_bounds c)))
		    )
		with 
		  | _ -> 
		      ( 
			try
			  let y' = regular_value_of_bounds d
			  in 
			    ((not(y' < x )) && ((y' <> x)||(bool_of_bounds a)||(bool_of_bounds d))) 
			with
			  | _ -> true
		      )
	      )
	  with
	    | _ -> false 
	)
    | ( Di(n) , Itv({lft_bnd = c ; rgt_bnd = d}) )  
    | ( Itv({lft_bnd = c ; rgt_bnd = d}) , Di(n) ) -> 
	let x' = regular_value_of_bounds c
	in	  
	  (
	    try
	      let y' = regular_value_of_bounds d
	      in
		((x' <= n) && (n <= y'))
	    with
	      | _ -> (x' <= n)
	  )
    | ( Di(n) , Di(m) ) -> ( n = m )
    | _ -> false


let in_touch = in_touch_version_1


let union it1 it2 = 
  if (in_touch it1 it2) || (it1 = empty) || (it2 = empty)
  then
    join it1 it2
  else
    (
      raise 
	(
	  Split [ it1 ; it2 ]
	)
    )


let intersection = meet


(*

let open_close it =
  match it with 
    | Itv({lft_bnd = a ; rgt_bnd = b}) -> 
	if a = Unrchbl_max 
	then 
	  Ei 
	else 
	  Itv({lft_bnd = bound_opened a ; rgt_bnd = bound_closed b})
    | _ -> it


let close_open it =
  match it with 
    | Itv({lft_bnd = a ; rgt_bnd = b}) -> 
	if a = Unrchbl_max 
	then 
	  Ei 
	else 
	  Itv({lft_bnd = bound_closed a ; rgt_bnd = bound_opened b})
    | _ -> it


*)

(*--------------------------------------------------------------------------------------*)
(* Pour comprendre la convention choisie pour l'intervalle vide, il faut se référer aux *)
(* bornes supérieurs et inférieures                                                     *)
(*--------------------------------------------------------------------------------------*)

let before it =
  match it with
    | Itv({lft_bnd = a ; rgt_bnd = _ }) -> normalize (Itv({lft_bnd = Cls(least_regular_value) ; rgt_bnd = (bound_reverse a)}))
    | Di(x)                             -> normalize (initial false x)
    | Ei                                -> full (* c'est-à-dire l'intervalle qui va du minimum (inclus) à "l'inifini" *)


let after it =
  match it with
    | Itv({lft_bnd = _ ; rgt_bnd = b }) -> normalize (Itv({lft_bnd = (bound_reverse b) ; rgt_bnd = Unrchbl_max}))
    | Di(x)                             -> normalize (terminal false x)
    | Ei                                -> full (* c'est-à-dire l'intervalle qui va du minimum (inclus) à "l'inifini" *)

let downward it = match it with 
  | Itv({lft_bnd = _ ; rgt_bnd = b }) -> normalize (Itv({lft_bnd = Cls(least_regular_value) ; rgt_bnd = b}))
  | Di(n)                             -> normalize (Itv({lft_bnd = Cls(least_regular_value) ; rgt_bnd = Cls(n)}))
  | Ei                                -> Ei

let upward it = match it with 
  | Itv({lft_bnd = a ; rgt_bnd = _ }) -> normalize (Itv({lft_bnd = a ; rgt_bnd = Unrchbl_max}))
  | Di(n)                             -> normalize (Itv({lft_bnd = Cls(n) ; rgt_bnd = Unrchbl_max}))
  | Ei                                -> Ei

let is_bounded it = match it with
  | Itv({lft_bnd = _ ; rgt_bnd = Unrchbl_max}) -> false
  | _                                          -> true

let complement_version_1 it = 
  match it with
    | Itv({lft_bnd = a ; rgt_bnd = b}) ->
	if a = Cls(least_regular_value)
	then
	  (
	    if b = Unrchbl_max
	    then
	      Itv({lft_bnd = Cls(least_regular_value) ; rgt_bnd = (bound_reverse a)})  
	    else
	      raise Undefined
	  )
	else
	  raise Undefined
    | Di(x) -> 
	if x = least_regular_value 
	then 
	  Itv({lft_bnd = Opn(least_regular_value) ; rgt_bnd = Unrchbl_max})  
	else
	  raise Undefined
    | Ei -> full

let complement_version_2 it = 
  match it with
    | Itv({lft_bnd = Cls(least_regular_value) ; rgt_bnd = b}) -> 
	if b = Unrchbl_max
	then
	  Ei
	else
	  (
	    Itv({lft_bnd = (bound_reverse b) ; rgt_bnd = Unrchbl_max})
	  )
    | Itv({lft_bnd = Opn(least_regular_value) ; rgt_bnd = b}) -> 
	if b = Unrchbl_max
	then
	  Di(least_regular_value)
	else
	  (
	    raise
	      (
		Split
		  [ 
		    Di(least_regular_value) ; 
		    Itv({lft_bnd = (bound_reverse b) ; rgt_bnd = Unrchbl_max}) 
		  ]
	      )
	  )
    | Itv({lft_bnd = a ; rgt_bnd = b}) -> 
	if b = Unrchbl_max
	then
	  Itv({lft_bnd = Cls(least_regular_value) ; rgt_bnd = (bound_reverse a)})
	else
	  (
	    raise
	      (
		Split
		  [ 
		    Itv({lft_bnd = Cls(least_regular_value) ; rgt_bnd = (bound_reverse a)}) ; 
		    Itv({lft_bnd = (bound_reverse b) ; rgt_bnd = Unrchbl_max}) 
		  ]
	      )
	  )
    | Di(x) ->
	if x = least_regular_value
	then
	  Itv({lft_bnd = Opn(least_regular_value) ; rgt_bnd = Unrchbl_max}) 
	else
	  (
	    raise
	      (
		Split
		  [ 
		    Itv({lft_bnd = Cls(least_regular_value) ; rgt_bnd = Opn(x)}) ; 
		    Itv({lft_bnd = Opn(x) ; rgt_bnd = Unrchbl_max}) 
		  ]
	      )
	  )
    | Ei -> full

let complement = complement_version_2

(*--------------------------------------------------------------------------------------*)
(* Fin du module Interval                                                               *)
(*--------------------------------------------------------------------------------------*)
