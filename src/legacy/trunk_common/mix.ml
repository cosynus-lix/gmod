(*------------------------------------------------------------------------------------------------*)
(*                                                                                                *)
(*                                                                                                *)
(*                                             MIX                                                *)
(*                                                                                                *)
(*                                                                                                *)
(*------------------------------------------------------------------------------------------------*)

(*------------------------------------------------------------------------------------------------*)
(* Ce module implémente à la fois les arcs d'un cercle dirigé et les segments d'une demi-droite   *)
(* Autrement dit on "somme" les deux types de valeurs à ceci près que l'on veut des fonctions qui *)
(* s'appliquent aux deux.                                                                         *)
(*------------------------------------------------------------------------------------------------*)

module type S =
sig
  exception Unbounded
  exception Undefined
  type regular_value
  val compare_regular_values: regular_value -> regular_value -> int 
  type t
  exception Split of t * t
  type underlying
  val line: underlying
  val circle: underlying
  val underlying: t -> underlying
  val least_regular_value: regular_value 
  val empty: underlying -> t
  val full: underlying -> t
  val compare: t -> t -> int
  val atom: underlying -> regular_value -> t
  val terminal: bool -> regular_value -> t
  val initial: bool -> regular_value -> t
  val bounded: bool -> bool -> regular_value -> regular_value -> underlying -> t 
  val belongs_to: regular_value -> t -> bool
  val is_included: t -> t -> bool
  val glb: t -> regular_value 
  val lub: t -> regular_value 
  val is_empty: t -> bool
  val is_full: t -> bool
  val is_not_empty: t -> bool
  val is_not_full: t -> bool
  val is_atomic: t -> bool
  val in_touch: t -> t -> bool
  val meet: t -> t -> t 
  val intersection: t -> t -> t 
  val join: t -> t -> t 
  val not_disjoint: t -> t -> bool
  val string_of: t -> string
  val interior: t -> t
  val closure: t -> t
  val normalize: t -> t
  val complement: t -> t 
  val before: t -> t
  val after: t -> t
  val downward: t -> t 
  val upward: t -> t
  val in_the_future_of: t -> t -> t 
  val in_the_past_of: t -> t -> t 
  val embed: t -> t
end


module Make =
struct

  module FromHalfLine(HL:Sig.Bound)
    =
  struct
    exception Unbounded
    exception Undefined
    type regular_value = HL.t
    let compare_regular_values rv1 rv2 = HL.compare rv1 rv2
    let least_regular_value = HL.least_regular_value
    module I = Interval.Make(HL)
    module A = Arc.Make(*From_HalfLine*)(HL)

    type t =
      | Itv of I.t
      | Arc of A.t

    type underlying =
      | Line
      | Circle

    let line = Line

    let circle = Circle

    let underlying se = match se with
      | Itv(_) -> Line
      | Arc(_) -> Circle

    exception Split of t * t

    let unary interval_operation arc_operation se =
      match se with
	| Itv(it) ->
	    (
	      try
		interval_operation it
	      with
		| I.Split(i1,i2) -> raise (Split(Itv i1,Itv i2))
		| I.Undefined -> raise Undefined
	    )
	| Arc(ar) ->
	    (
	      try
		arc_operation ar
	      with
		| A.Split(a1,a2) -> raise (Split((Arc a1),(Arc a2)))
		| A.Undefined -> raise Undefined
	    )

    let binary interval_operation arc_operation ia_default ai_default se1 se2 =
      match se1 with
	| Itv(it1) ->
	    (
	      match se2 with
		| Itv(it2) ->
		    (
		      try
			(interval_operation it1 it2)
		      with
			| I.Split(i1,i2) -> raise (Split(Itv i1,Itv i2))
			| I.Undefined -> raise Undefined
		    )
		| Arc(ar2) -> (ia_default it1 ar2)
	    )
	| Arc(ar1) ->
	    (
	      match se2 with
		| Itv(it2) -> (ai_default ar1 it2)
		| Arc(ar2) ->
		    (
		      try
			(arc_operation ar1 ar2)
		      with
			| A.Split(a1,a2) -> raise (Split((Arc a1),(Arc a2)))
			| A.Undefined -> raise Undefined
		    )
	    )

    let is_interval se = unary (fun it -> true) (fun ar -> false) se

    let is_arc se = unary (fun it -> false) (fun ar -> true) se

    let compare se1 se2 =
      binary
	(I.compare) (A.compare)
	(fun it ar -> -1) (fun ar it -> 1)
	se1 se2

    let normalize se = unary (fun it -> Itv(I.normalize it)) (fun ar -> Arc(A.normalize ar)) se

    let full u =
      match u with
	| Line -> Itv(I.full)
	| Circle -> Arc(A.full)

    let empty u =
      match u with
	| Line -> Itv(I.empty)
	| Circle -> Arc(A.empty)

    let bounded lf rf lb rb u =
      match u with
	| Line -> Itv(I.bounded lf rf lb rb)
	| Circle -> Arc(A.bounded lf rf lb rb)
    and atom u x = match u with
      | Line -> Itv(I.atom x)
      | Circle -> Arc(A.atom x)
    and initial b x = Itv(I.initial b x)
    and terminal b x = Itv(I.terminal b x)
    and coatom x = Arc(A.coatom x)
    and cobounded lf rf lb rb = Arc(A.bounded rf lf rb lb) (* est-ce utile ? *)

    let embed se = match se with
      | Itv(it) ->
	  (
	    try
	      let a = I.glb it
	      in
		(
		  try
		    let b = I.lub it
		    in
		      (
			if a = b
			then
			  Arc(A.atom a)
			else
			  Arc(A.bounded (I.belongs_to a it) (I.belongs_to b it) a b)
		      )
		  with
		    | _ -> Arc(A.bounded false (I.belongs_to a it) least_regular_value a)
		)
	    with
	      | _ -> Arc(A.empty)
	  )
      | _       -> se

    let lift se = match se with
      | Arc(ar) ->
	  (
	    try
	      (
		let a = A.glb ar
		and b = A.lub ar
		in
		  if (HL.compare a b) < 0
		  then
		    Itv(I.bounded (A.belongs_to a ar) (A.belongs_to b ar) a b)
		  else
		    (
		      if (HL.compare a b) > 0
		      then
			raise Undefined
		      else
			(
			  if ar = A.atom a
			  then Itv(I.atom a)
			  else raise Undefined
			)
		    )
	      )
	    with
	      | _ -> if ar = A.empty then Itv(I.empty) else Itv(I.full)
	  )
      | _ -> se

    let glb se          = unary I.glb A.glb se
    and lub se          = unary I.lub A.lub se
    and complement se   = unary (fun it -> Itv(I.complement it)) (fun ar -> Arc(A.complement ar)) se
    and interior se     = unary (fun it -> Itv(I.interior it)) (fun ar -> Arc(A.interior ar)) se
    and closure se      = unary (fun it -> Itv(I.closure it)) (fun ar -> Arc(A.closure ar)) se
    and after se        = unary (fun it -> Itv(I.after it)) (fun ar -> raise A.Undefined) se
    and before se       = unary (fun it -> Itv(I.before it)) (fun ar -> raise A.Undefined) se
    and belongs_to p se = unary (fun it -> I.belongs_to p it) (fun ar -> A.belongs_to p ar) se

    let in_touch se1 se2 =
      binary
	I.in_touch A.in_touch
	(fun it ar -> raise Undefined) (fun ar it -> raise Undefined)
	se1 se2

    let rec not_disjoint se1 se2 =
      binary
	I.not_disjoint A.not_disjoint
	(fun it ar -> raise Undefined) (fun ar it -> raise Undefined)
	se1 se2

    let meet se1 se2 =
      binary
	(fun it1 it2 -> Itv(I.meet it1 it2)) (fun ar1 ar2 -> Arc(A.intersection ar1 ar2))
	(fun it ar -> raise Undefined) (fun it ar -> raise Undefined)
	se1 se2
    let intersection = meet
    and join se1 se2 =
      binary
	(fun it1 it2 -> Itv(I.join it1 it2)) (fun ar1 ar2 -> Arc(A.union ar1 ar2))
	(fun it ar -> raise Undefined) 	(fun it ar -> raise Undefined)
	se1 se2

    let upward se = unary (fun it -> Itv(I.upward it)) (fun ar -> raise Undefined) se

    let downward se = unary (fun it -> Itv(I.downward it)) (fun ar -> raise Undefined) se

    let is_atomic se = unary I.is_atomic A.is_atomic se
    and is_coatomic se = unary (fun it -> (it=(I.terminal false I.least_regular_value))) A.is_coatomic se

    let is_empty se = unary I.is_empty A.is_empty se

    let is_full se = unary I.is_full A.is_full se

    let is_not_empty se = unary I.is_not_empty A.is_not_empty se

    (* let is_not_full se = unary (fun it -> (it <> I.full)) (fun ar -> (ar <> A.full)) se *)

    let is_not_full se = unary I.is_not_full A.is_not_full se

    let is_included se1 se2 =
      binary
	I.is_included A.is_included
	(fun it ar -> raise Undefined) (fun ar it -> raise Undefined)
	se1 se2

    let in_the_future_of se1 se2 =
      binary
	(fun it1 it2 -> Itv(I.in_the_future_of it1 it2)) (fun ar1 ar2 -> Arc(A.in_the_future_of ar1 ar2))
	(fun it ar -> raise Undefined) (fun ar it -> raise Undefined)
	se1 se2

    let in_the_past_of se1 se2 =
      binary
	(fun it1 it2 -> Itv(I.in_the_past_of it1 it2)) (fun ar1 ar2 -> Arc(A.in_the_past_of ar1 ar2))
	(fun it ar -> raise Undefined) (fun ar it -> raise Undefined)
	se1 se2

    let string_of se = unary (fun it -> I.string_of ~sd:("[","]") it) (fun ar -> A.string_of ~sd:("(",")") ar) se

  end

end

(*

  module Make =
  struct

  module From_HalfLine(HL:(Sig.Bound))=(Raw.From_HalfLine(HL):(S with type regular_value = HL.t))

  end

*)
