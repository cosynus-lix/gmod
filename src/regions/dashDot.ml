(** Implements the Boolean algebra of finite disconnected unions of connected 
subsets of the positive half-line/circle together with topological and directed 
operators. The later behaviour depends on whether one considers subsets of the 
half-line or subsets of the circle.*)

module type S = sig

  (** {2 Types}*)

  type value
  type t

(** {2 Exceptions}*)

  exception Undefined
  
  (** {2 Display} *)
  
  val string_of: t -> string
  val hl_string_of: t -> string
  
  (** {2 Tests} *)  
  
  val mem: value -> t -> bool
  val is_included: t -> t -> bool
  val is_not_included: t -> t -> bool
  val is_empty: t -> bool
  val is_not_empty: t -> bool
  val is_full: t -> bool
  val is_not_full: t -> bool
  val contains_zero: t -> bool
  val does_not_contain_zero: t -> bool
  val contains_more_than_zero: t -> bool
  val contains_at_most_zero: t -> bool
  val lacks_at_most_zero: t -> bool
  val lacks_more_than_zero: t -> bool
  val mem: value -> t -> bool
  
  (** {2 Constants} *)

  val zero: value
  val empty: t
  val full: t
  
  (** {2 Constructors} *)
  
  (** [atom v] is the singleton \{v\} *)
  val atom: value -> t

  (** [discrete \[v1;...;vn\]] is the finite set \{v{_ 1},...,v{_ n}\}. The 
  sequence must be strictly increasing, i.e. v{_1}<...<v{_n}.*)
  val discrete: value list -> t
  
  (** [interval b1 b2 v1 v2] is the interval of points between v{_1} and v{_2}. 
  
  One should have v{_1}<v{_2}, the bounds being included or excluded depending 
  of the values of b{_1} and b{_2} being [true] or [false].*)
  val interval: bool -> bool -> value -> value -> t
  
  (** [coatom v] is a shorthand for [complement (atom v)] *)
  val coatom: value -> t
  
  (** [codiscrete \[v1;...;vn\]] is a shorthand for [complement (discrete \[v1;...;vn\])] *)
  val codiscrete: ?do_sort:bool -> value list -> t
  
  (** [cointerval b1 b2 v1 v2] is a shorthand for [complement (interval (not b1) (not b2) v1 v2)] *)
  val cointerval: bool -> bool -> value -> value -> t

  (** [initial b v] is the set of points below v.  
  The right bound being included or excluded depending 
  of the value of b being [true] or [false].
  
  It is also a shorthand for [interval true b zero v].*)
  val initial: bool -> value -> t
  
  (** [final b v] is the set of points beyond v.  
  The left bound being included or excluded depending 
  of the value of b being [true] or [false]. 
  
  It is equal to [complement (initial (not b) v)]
  *)
  val final: bool -> value -> t
  
  val intervals_of: t -> t list
  
  val of_intervals: t list -> t

  (** {2 Iterator and Enumerator}*)
  
  val next_interval: (value -> value) -> t -> t

  val next: (value -> value) -> t -> t 
    
  (** {2 Boolean operators} *)
  
  val complement: t -> t
  val meet: t -> t -> t
  val join: t -> t -> t
  val difference: t -> t -> t
  val symmetric_difference: t -> t -> t
  
  
  (** {2 Miscellaneous} *)
  
  (** Given a function [f: bool -> bool -> bool], the semantics of the function
[binary_boolean_operator f], let's call it F, is as follows:
 
given A₁:t and A₂:t we have

    {C F(A₁,A₂) = \{ x∊ℝ₊ | f( i(x,A₁) , i(x,A₂) ) \} }

where i(x,A₁) = true iff x∊A₁ ; i(x,A₂) = true iff x∊A₂.

The usual binary operators ⋂ ({i intersection}), ⋃
({i union}), \ ({i difference}) and Δ ({i symmetric difference}) are obtained
that way.*)
  val binary_boolean_operator :
        (bool -> bool -> bool) -> t -> t -> t
        
  (** Given a function [f: bool -> bool -> bool], the semantics of the function 
  [exists f], let's call it F, is as follows: 
  for A₁:t and A₂:t, the value F(A₁,A₂) is true iff there {i exists} some 
  x∊ℝ₊ such that f(i(x,A₁),i(x,A₂)) is true, where i(x,A₁) = true iff x∊A₁ ; i(x,A₂) = true iff x∊A₂.*)
  val exists: (bool -> bool -> bool) -> t -> t -> bool
  
  (** Given a function [f: bool -> bool -> bool], the semantics of the function 
  [exists f], let's call it F, is as follows: 
  for A₁:t and A₂:t, the value F(A₁,A₂) is true iff {i for all}  
  x∊ℝ₊ f(i(x,A₁),i(x,A₂)) is true, where i(x,A₁) = true iff x∊A₁ ; i(x,A₂) = true iff x∊A₂.*)
  val for_all: (bool -> bool -> bool) -> t -> t -> bool
  
  (** Should be a linearization of [is_included]*)
  val compare: t -> t -> int
  val add_zero: t -> t
  val remove_zero: t -> t
  val first_connected_component: ?flag:bool ref -> t -> t
  val last_connected_component: ?parity:bool -> t -> t
  
  (** {2 Directed topology}*)
  
  module type DirectedTopology = sig


    val string_of: t -> string
    (** Shape dependent string converter. *)

    val interior: t -> t
    (** [interior x] is the {i interior} of the set [x] with respect to the 
    topology of the half-line/circle depending on the module from which it is 
    called.*)
    
    val closure: t -> t
    (** [closure x] is the {i closure} of the set [x] with respect to the 
    topology of the half-line/circle depending on the module from which it is 
    called.*)

    val future_extension: t -> t -> t
    (** [future_extension x y] is the set of points {i q} of [y] such that 
    there exists a point {i p} of [x] such the interval/anticlockwise arc from 
    {i p} to {i q} is contained in the union of [x] and [y].*)

    (** In this version, we consider the points {i q} of [y] ... *)
    
    val past_extension: t -> t -> t
    (** [past_extension x y] is the set of points {i q} of [y] such that 
    there exists a point {i p} of [x] such the interval/anticlockwise arc from 
    {i q} to {i p} is contained in the union of [x] and [y].*)

  end
  
  module HalfLine:DirectedTopology
  
  module Circle:DirectedTopology
   
end

(* One-dimensional isothetic regions over the halfline and the circle *)

module Raw(B:Bound.S) =
struct

  type value = B.t

  let bound_compare = ref B.compare
  
  let original_order = ref true
  
  let reverse_bound_order () = 
    let () = 
      if !original_order
      then bound_compare := fun v1 v2 -> -B.compare v1 v2
      else bound_compare := B.compare in
    original_order := not !original_order

  let (<) x y = !bound_compare x y < 0

  let (>) x y = !bound_compare x y > 0

  let (<=) x y = !bound_compare x y <= 0

  let (>=) x y = !bound_compare x y >= 0

  exception Undefined

  let zero = B.least_regular_value

  (* Warning: the order in which the constructors are listed matters,
     see the function compare defined later. Does it actually ? I
     don't think so.*)

  type bound =
    | Opn of B.t (* open *)
    | Iso of B.t (* isolated *)
    | Cls of B.t (* close *)
    | Pun of B.t (* puncture *)

  let rvb b = match b with
    | Opn x | Cls x | Iso x | Pun x -> x

  let reverse_bound b = match b with
    | Opn x -> Cls x
    | Cls x -> Opn x
    | Iso x -> Pun x
    | Pun x -> Iso x

  let close_bound b = match b with
    | Opn x | Cls x -> Cls x
    | Iso x | Pun x -> Iso x

  let open_bound b = match b with
    | Opn x | Cls x -> Opn x
    | Iso x | Pun x -> Pun x

  let bob b = match b with
    | Opn x | Pun x -> false
    | Cls x | Iso x -> true

  let alter_parity b = match b with
    | Opn x | Cls x -> true
    | Pun x | Iso x -> false

  type t = bound list

  let monotonic_map f a =
	  let f b = match b with
			| Cls x -> Cls (f x)
			| Opn x -> Opn (f x)
			| Iso x -> Iso (f x)
			| Pun x -> Pun (f x) in
	  List.map f a

  (* String conversion *)

  let string_of a =
	  let f b = match b with
      | Cls x -> "Cls "^(B.string_of x)
      | Opn x -> "Opn "^(B.string_of x)
      | Iso x -> "Iso "^(B.string_of x)
      | Pun x -> "Pun "^(B.string_of x) in
    String.concat ";" (List.map f a)

  (* Internal *)

  let unbounded_connected_component_must_be_added at1 at2 =
    match at1 with 
      | b1::_ -> (
        b1 = Iso zero || b1 = Cls zero ||
			  match at2 with
			    | Cls x::b2::_ ->
			      let rvbb1 = rvb b1 and rvbb2 = rvb b2 in
			      x=zero && (rvbb1<rvbb2 || (rvbb1=rvbb2 && (bob b1 || bob b2)))
			    | [Cls x] -> x = zero
			    | Iso x::_ -> x = zero && b1 = Opn zero
			    | _ -> false)
      | [] -> false
  
  (* Returns the value of the last bound *)

  let rec value_of_last_bound a = match a with
    | [b] -> rvb b
    | _::a -> value_of_last_bound a
    | [] -> raise Undefined

  let value_of_the_first_bound a = match a with
    | b :: _ -> rvb b
    | _ -> raise Undefined

  (* Tests *)

  (* Criterion for a list of bounds to be valid: the number of Opn _
     and Cls _ values before any Iso _ value must be even and the
     number of Opn _ and Cls _ values before any Pun _ value must be
     odd. In other words a puncture must occur in the interior of a
     connected component while an isolated point must be a connected
     component. *)

  let is_valid a =
    let parity = ref false in
    let switch () = parity := not !parity in
    let rec is_valid prec_x a = match a with
      | Opn x::a | Cls x::a ->
					if prec_x < x
					then (switch ();is_valid x a)
					else false
      | Iso x::a ->
					if !parity || prec_x >= x
					then false
					else is_valid x a
      | Pun x::a ->
					if !parity && prec_x < x
					then is_valid x a
					else false
      | [] -> true in
    match a with
      | Opn x::a | Cls x::a -> switch () ; is_valid x a
      | Iso x::a -> is_valid x a
      | Pun x::a -> false
      | [] -> true

  let is_empty a = (a = [])

  let is_not_empty a = (a <> [])

  let is_full a = (a = [Cls zero])

  let is_not_full a = (a <> [Cls zero])

  let contains_zero ar = match ar with
    | Iso x::_ | Cls x::_ -> x = zero
    | _ -> false

  let does_not_contain_zero ar = match ar with
    | Iso x::_ | Cls x::_ -> x <> zero
    | _ -> true

  let contains_more_than_zero a = (a <> []) && (a <> [Iso zero])

  let contains_at_most_zero a = (a = []) || (a = [Iso zero])

  let lacks_at_most_zero a = (a = [Cls zero]) || (a = [Opn zero])

  let lacks_more_than_zero a = (a <> [Cls zero]) && (a <> [Opn zero])

  let rec parity answer a = match a with
		| Opn x :: a | Cls x :: a -> parity (not answer) a
		| Pun x :: a | Iso x :: a -> parity answer a
		| [] -> answer

  let is_unbounded a = parity false a

  let mem pt ar =
    let rec mem (p,ar) = match (p,ar) with
      | (false,Opn x::ar) -> if x<pt then mem (true,ar) else false
      | (false,Cls x::ar) -> if x<pt then mem (true,ar) else x=pt
      | (true,Opn x::ar) -> if x<pt then mem (false,ar) else x<>pt
      | (true,Cls x::ar) -> if x<pt then mem (false,ar) else true
      | (_,Iso x::ar) -> if x<pt then mem (false,ar) else x=pt
      | (_,Pun x::ar) -> if x<pt then mem (true,ar) else x<>pt
      | (p,[]) -> p
    in
    mem (false,ar)

  (* Constructors *)

  let empty = []

  let full = [Cls zero]

  let iso x = Iso x

  let pun x = Pun x

  let cls x = Cls x

  let opn x = Opn x

  let make bound_list =
    if is_valid bound_list
    then bound_list
    else failwith "DashDot.BooleanAlgebra.make"

  let atom x = [Iso x]

  (* Turns a sorted list of values into a discrete space *)
  let discrete lx = List.map (fun x -> Iso x) lx

  let coatom x = if x=zero then [Opn zero] else [Cls zero; Pun x]

  let codiscrete ?(do_sort=false) lx =
    let sorted_lx = if do_sort then List.sort B.compare lx else lx in
    match sorted_lx with
      | x::sorted_lx_queue ->
					if x<>zero
					then Cls zero::List.map (fun x -> Pun x) sorted_lx
					else Opn zero::List.map (fun x -> Pun x) sorted_lx_queue
      | _ -> full

  let interval a b x y =
    if x < y
    then [if a then Cls x else Opn x; if b then Cls y else Opn y]
    else
      if x = y && a && b
      then [Iso x]
      else failwith "oda.ml: interval: the left must be less than the right bound"

  let initial a x =
    if x <> zero
    then [Cls zero; if a then Cls x else Opn x]
    else
      if a
      then atom zero
      else empty

  let final b y = [if b then Cls y else Opn y]

  let cointerval a b x y =
    if x > y
    then
      if y <> zero
      then [Cls zero;if b then Cls y else Opn y; if a then Cls x else Opn x]
      else
				if b
				then [Iso zero; if a then Cls x else Opn x]
				else [if a then Cls x else Opn x]
    else
      if x = y && (not a) && (not b)
      then [Cls zero; Pun x]
      else failwith "oda.ml: cointerval: the left bound must be greater than the right bound"

  let of_intervals re = 
    let re = List.concat re in
    let rec polish re = match re with
      | a :: ((b :: re'') as re') -> 
          if rvb a <> rvb b 
          then a :: polish re'
          else (match a with
            | Cls x -> (Iso x) :: polish re''  
            | Opn x -> (Pun x) :: polish re''
            | _ -> assert false)
      | _ -> re in
    polish re

  let rec intervals_of at = match at with
    | [] -> []
    | ([Cls x] as a) | ([Opn x] as a) -> [a]
    | Iso x :: at -> [Iso x] :: intervals_of at
    | a :: Pun x :: at -> [a;Opn x] :: intervals_of (Opn x :: at)
    | a :: b :: at -> [a;b] :: intervals_of at
    | _ -> assert false


(*
  head_and_tail returns an ordered pair whose first term is the first 
  connected component and whose second term is the remaining of the region.
*)

  let head_and_tail at = match at with
    | ([Cls x] as a) | ([Opn x] as a) -> (a , [])
    | Iso x :: at -> ([Iso x] , at)
    | a :: Pun x :: at -> ([a;Opn x] , (Opn x :: at))
    | a :: b :: at -> ([a;b] , at)
    | [] -> raise Undefined
    | _ -> failwith "head_and_tail"

  let pop it at =
    let hnt = (try head_and_tail !at with Undefined -> raise Exit) in
    it := fst hnt;
    at := snd hnt

  let cut_off x = 
    let hnt = head_and_tail !x in
    x := snd hnt 

  let clear x = x := empty

  let push it answer = answer := it :: !answer
    
(* Duplicata de la fonction du module HalfLine, provisoirement pour dubbugé le module FutureExtension *)

let hl_string_of a =
  let empty_set_denotation = "Ø" in
  let infinity_denotation = "+oo" in
  let open_infinity = true in
  let rec string_of p a = match a with
    |	Cls x::a ->
        if p
        then (B.string_of x)^(if a<>[] then "] " else "]")^(string_of (not p) a)
        else "["^(B.string_of x)^","^(string_of (not p) a)
    |	Opn x::a ->
        if p
        then (B.string_of x)^(if a<>[] then "[ " else "[")^(string_of (not p) a)
        else "]"^(B.string_of x)^","^(string_of (not p) a)
    |	Iso x::a -> "{"^(B.string_of x)^(if a <>[] then "} " else "}")^(string_of p a)
    |	Pun x::a -> let b = B.string_of x in b^"[ ]"^b^","^(string_of p a)
    | _ ->
        if p
        then if open_infinity then infinity_denotation^"[" else infinity_denotation^"]"
        else if open_infinity then "" else " {"^infinity_denotation^"}" in
  let answer = string_of false a in
  if answer <> "" then answer else empty_set_denotation

  (* The normal form is a sorted list of bounds *)

  (* TODO: improve it *)

  let normalize a =
    let rec normalize parity a = match a with
      | Opn x::(Opn y::a as b) ->
					if parity
					then
					  if x < y
					  then Opn x::normalize false b
					  else
					    if x = y
					    then Pun x::normalize true a
					    else failwith "normalize [DashDot]"
					else
					  if x < y
					  then Opn x::normalize true b
					  else failwith "normalize [DashDot]"
      | (Opn x as c)::(Cls y::a as b)
      | (Cls x as c)::(Opn y::a as b)
      | (Cls x as c)::(Cls y::a as b) ->
					if parity
					then
					  if x<y
					  then c::normalize false b
					  else
					    if x=y
					    then normalize true a
					    else failwith "normalize [DashDot]"
					else
					  if x<y
					  then c::normalize true b
					  else failwith "normalize [DashDot]"
      | (Iso x as c)::(Opn y as b)::a
      | (Iso x as c)::(Cls y as b)::a ->
					if parity
					then failwith "normalize [DashDot]"
					else
					  if x<y
					  then c::b::normalize true a
					  else
					    if x=y
					    then Cls x::normalize true a
					    else failwith "normalize [DashDot]"
      | (Opn x as c)::Iso y::a
      | (Cls x as c)::Iso y::a ->
					if parity
					then
					  if x<y
					  then c::Iso y::normalize false a
					  else
					    if x=y
					    then normalize true (Cls x::a)
					    else failwith "normalize [DashDot]"
					else failwith "normalize [DashDot]"
      | x -> x in
    normalize false a

  (* The normal forms of both arguments have to be provided so the
     function union acts as a kind of merge function *)

  (* boolean algebra *)

  let complement a =
    match a with
      | Cls b::a ->
					if b <> zero
					then Cls zero::Opn b::(List.map reverse_bound a)
					else (List.map reverse_bound a)
      | Opn b::a ->
					if b <> zero
					then Cls zero::Cls b::(List.map reverse_bound a)
					else Iso zero::(List.map reverse_bound a)
      | Iso b::a ->
					if b <> zero
					then Cls zero::Pun b::(List.map reverse_bound a)
					else Opn zero::(List.map reverse_bound a)
      | [] -> full
      | Pun _::_ -> invalid_arg "complement [DashDot]"

  let binary_boolean_operator test =
    let rec binary_boolean_operator ar1 ar2 =
      match ar1,ar2 with
				(* Induction *)
				| (p1,b1::ar1 as x1),(p2,b2::ar2 as x2) ->
				  (
				    let v1 = rvb b1 in
				    let v2 = rvb b2 in
				    if v1<v2
				    then
				      let p1' = if alter_parity b1 then not p1 else p1 in
				      let remaining () = binary_boolean_operator (p1',ar1) x2 in
				      let middle = test (bob b1) p2 in
				      match test p1 p2,test p1' p2 with
								| false,false -> if middle then Iso v1::remaining () else         remaining ()
								| true ,true  -> if middle then         remaining () else Pun v1::remaining ()
								| _           -> if middle then Cls v1::remaining () else Opn v1::remaining ()
				    else
				      if v1>v2
				      then
								let p2' = if alter_parity b2 then not p2 else p2 in
								let remaining () = binary_boolean_operator x1 (p2',ar2) in
								let middle = test p1 (bob b2) in
								match test p1 p2,test p1 p2' with
								  | false,false -> if middle then Iso v2::remaining () else         remaining ()
								  | true ,true  -> if middle then         remaining () else Pun v2::remaining ()
								  | _           -> if middle then Cls v2::remaining () else Opn v2::remaining ()
				      else (*v1=v2*)
								let p1' = if alter_parity b1 then not p1 else p1 in
								let p2' = if alter_parity b2 then not p2 else p2 in
								let remaining () = binary_boolean_operator (p1',ar1) (p2',ar2) in
								let middle = test (bob b1) (bob b2) in
								match test p1 p2,test p1' p2' with
								  | false,false -> if middle then Iso v1::remaining () else         remaining ()
								  | true ,true  -> if middle then         remaining () else Pun v1::remaining ()
								  | _           -> if middle then Cls v1::remaining () else Opn v1::remaining ()
				  )
				(* Terminal *)
				| (p1,[]),(p2,ar2') ->
				  (
				    match test p1 true,test p1 false with
				      | true,false -> ar2'
				      | false,true -> List.map reverse_bound ar2'
				      | _   -> []
				  )
				| (p1,ar1'),(p2,[]) ->
				  (
				    match test true p2,test false p2 with
				      | true,false  -> ar1'
				      | false,true  -> List.map reverse_bound ar1'
				      | _   -> []
				  )
    in (*The first round should be treated separately*)
    fun ar1 ar2 -> match ar1,ar2 with
      | (b1::ar1 as x1'),(b2::ar2 as x2') ->
				let v1 = rvb b1 in
				let v2 = rvb b2 in
				if v1<v2
				then (
          let p1' = alter_parity b1 in
          let x2 = (false,x2') in
          let middle = test (bob b1) false in
          let remaining () = binary_boolean_operator (p1',ar1) x2 in
          if v1=zero
          then match middle,test p1' false with
            | false,false ->           remaining ()
            | true ,true  -> Cls zero::remaining ()
            | true ,false -> Iso zero::remaining ()
            | false,true  -> Opn zero::remaining ()
          else match test false false,test p1' false with
            | false,false ->
              if middle
              then           Iso v1::remaining ()
              else                   remaining ()
            | false,true  ->
              if middle
              then           Cls v1::remaining ()
              else           Opn v1::remaining ()
            | true ,true  -> (*first round specific*)
              if middle
              then Cls zero::        remaining ()
              else Cls zero::Pun v1::remaining ()
            | true,false  -> (*first round specific*)
              if middle
              then Cls zero::Cls v1::remaining ()
              else Cls zero::Opn v1::remaining ())
				else if v1>v2
				  then (
            let p2' = alter_parity b2 in
            let x1 = (false,x1') in
            let middle = test false (bob b2) in
            let remaining () = binary_boolean_operator x1 (p2',ar2) in
            if v2=zero
            then match middle,test false p2' with
              | false,false ->           remaining ()
              | true ,true  -> Cls zero::remaining ()
              | true ,false -> Iso zero::remaining ()
              | false,true  -> Opn zero::remaining ()
            else match test false false,test false p2' with
              | false,false ->
                if middle
                then           Iso v2::remaining ()
                else                   remaining ()
              | false,true  ->
                if middle
                then           Cls v2::remaining ()
                else           Opn v2::remaining ()
              | true ,true  -> (*first round specific*)
                if middle
                then Cls zero::        remaining ()
                else Cls zero::Pun v2::remaining ()
              | true,false  -> (*first round specific*)
                if middle
                then Cls zero::Cls v2::remaining ()
                else Cls zero::Opn v2::remaining ())
				  else ((*v1=v2*)
				      let p1' = alter_parity b1 in
				      let p2' = alter_parity b2 in
				      let middle = test (bob b1) (bob b2) in
				      let remaining () = binary_boolean_operator (p1',ar1) (p2',ar2) in
				      if v1=zero
				      then match middle,test p1' p2' with
                | false,false ->           remaining ()
                | true ,true  -> Cls zero::remaining ()
                | true ,false -> Iso zero::remaining ()
                | false,true  -> Opn zero::remaining ()
				      else match test false false,test p1' p2' with
                | false,false ->
                  if middle
                  then           Iso v1::remaining ()
                  else                   remaining ()
                | false,true  ->
                  if middle
                  then           Cls v1::remaining ()
                  else           Opn v1::remaining ()
                | true ,true  -> (*first round specific*)
                  if middle
                  then Cls zero::        remaining ()
                  else Cls zero::Pun v1::remaining ()
                | true,false  -> (*first round specific*)
                  if middle
                  then Cls zero::Cls v1::remaining ()
                  else Cls zero::Opn v1::remaining ())
		      | _ -> (*at least one of the arguments is empty*)
							let b = is_empty ar1 in
							match test (not b) b,test false false with
							  | true,false  -> if b then ar2 else ar1
							  | false,true  -> complement (if b then ar2 else ar1)
							  | true,true   -> full
							  | false,false -> empty

  let symmetric_difference = binary_boolean_operator (<>)

  let meet = binary_boolean_operator (&&)

  let join = binary_boolean_operator (||)

  let difference = binary_boolean_operator Pervasives.(>)

  let exists test =
    let rec exists ar1 ar2 =
      match ar1,ar2 with
	(* Induction *)
	| (p1,b1::ar1 as x1),(p2,b2::ar2 as x2) ->
	  (
	    let v1 = rvb b1 in
	    let v2 = rvb b2 in
	    if v1<v2
	    then
	      let p1' = if alter_parity b1 then not p1 else p1 in
	      let remaining () = exists (p1',ar1) x2 in
	      let middle = test (bob b1) p2 in
	      match test p1 p2,test p1' p2 with
		| false,false -> middle || remaining ()
		| _           -> true
	    else
	      if v1>v2
	      then
		let p2' = if alter_parity b2 then not p2 else p2 in
		let remaining () = exists x1 (p2',ar2) in
		let middle = test p1 (bob b2) in
		match test p1 p2,test p1 p2' with
		  | false,false -> middle || remaining ()
		  | _           -> true
	      else (*v1=v2*)
		let p1' = if alter_parity b1 then not p1 else p1 in
		let p2' = if alter_parity b2 then not p2 else p2 in
		let remaining () = exists (p1',ar1) (p2',ar2) in
		let middle = test (bob b1) (bob b2) in
		match test p1 p2,test p1' p2' with
		  | false,false -> middle || remaining ()
		  | _           -> true
	  )
	(* Terminal *)
	| (p1,[]),(p2,ar2') ->
	  (
	    match test p1 true,test p1 false with
	      | true,true   -> true
	      | false,false -> false
	      | _           -> ar2'<>[]
	  )
	| (p1,ar1'),(p2,[]) ->
	  (
	    match test true p2,test false p2 with
	      | true,true   -> true
	      | false,false -> false
	      | _           -> ar1'<>[]
	  )
    in (*The first round should be treated separately*)
    fun ar1 ar2 ->
      match ar1,ar2 with
	| (b1::ar1 as x1'),(b2::ar2 as x2') ->
	  let v1 = rvb b1 in
	  let v2 = rvb b2 in
	  if v1<v2
	  then
	    (
	      let p1' = alter_parity b1 in
	      let x2 = (false,x2') in
	      let middle = test (bob b1) false in
	      let remaining () = exists (p1',ar1) x2 in
	      if v1=zero
	      then
		match middle,test p1' false with
		  | false,false -> remaining ()
		  | _  -> true
	      else
		match test false false,test p1' false with
		  | false,false -> middle || remaining ()
		  | _  -> true
	    )
	  else
	    if v1>v2
	    then
	      (
		let p2' = alter_parity b2 in
		let x1 = (false,x1') in
		let middle = test false (bob b2) in
		let remaining () = exists x1 (p2',ar2) in
		if v2=zero
		then
		  match middle,test false p2' with
		    | false,false -> remaining ()
		    | _  -> true
		else
		  match test false false,test false p2' with
		    | false,false -> middle || remaining ()
		    | _  -> true
	      )
	    else (*v1=v2*)
	      (
		let p1' = alter_parity b1 in
		let p2' = alter_parity b2 in
		let middle = test (bob b1) (bob b2) in
		let remaining () = exists (p1',ar1) (p2',ar2) in
		if v1=zero
		then
		  match middle,test p1' p2' with
		    | false,false -> remaining ()
		    | _ -> true
		else
		  match test false false,test p1' p2' with
		    | false,false -> middle || remaining ()
		    | _  -> true
	      )
	| _ -> (*at least one of the arguments is empty*)
	  let b = is_empty ar1 in
	  match test (not b) b,test false false with
	    | true,false  -> not b || is_not_empty ar2
	    | false,true  -> (b && is_not_full ar2) || (not b && is_not_full ar1)
	    | true,true   -> true
	    | false,false -> false


  let for_all test =
    let rec for_all ar1 ar2 =
      match ar1,ar2 with
	(* Induction *)
	| (p1,b1::ar1 as x1),(p2,b2::ar2 as x2) ->
	  (
	    let v1 = rvb b1 in
	    let v2 = rvb b2 in
	    if v1<v2
	    then
	      let p1' = if alter_parity b1 then not p1 else p1 in
	      let remaining () = for_all (p1',ar1) x2 in
	      let middle = test (bob b1) p2 in
	      match test p1 p2,test p1' p2 with
		| true ,true -> middle && remaining ()
		| _          -> false
	    else
	      if v1>v2
	      then
		let p2' = if alter_parity b2 then not p2 else p2 in
		let remaining () = for_all x1 (p2',ar2) in
		let middle = test p1 (bob b2) in
		match test p1 p2,test p1 p2' with
		  | true ,true  -> middle && remaining ()
		  | _           -> false
	      else (*v1=v2*)
		let p1' = if alter_parity b1 then not p1 else p1 in
		let p2' = if alter_parity b2 then not p2 else p2 in
		let remaining () = for_all (p1',ar1) (p2',ar2) in
		let middle = test (bob b1) (bob b2) in
		match test p1 p2,test p1' p2' with
		  | true ,true -> middle && remaining ()
		  | _          -> false
	  )
	(* Terminal *)
	| (p1,[]),(p2,ar2') ->
	  (
	    match test p1 true,test p1 false with
	      | true,true   -> true
	      | false,false -> false
	      | _ -> ar2'=[]
	  )
	| (p1,ar1'),(p2,[]) ->
	  (
	    match test true p2,test false p2 with
	      | true,true   -> true
	      | false,false -> false
	      | _   -> ar1'=[]
	  )
    in (*The first round should be treated separately*)
    fun ar1 ar2 -> match ar1,ar2 with
      | (b1::ar1 as x1'),(b2::ar2 as x2') ->
	let v1 = rvb b1 in
	let v2 = rvb b2 in
	if v1<v2
	then
	  (
	    let p1' = alter_parity b1 in
	    let x2 = (false,x2') in
	    let middle = test (bob b1) false in
	    let remaining () = for_all (p1',ar1) x2 in
	    if v1=zero
	    then
	      match middle,test p1' false with
		| true ,true -> remaining ()
		| _ -> false
	    else
	      match test false false,test p1' false with
		| true ,true -> middle && remaining ()
		| _ -> false
	  )
	else
	  if v1>v2
	  then
	    (
	      let p2' = alter_parity b2 in
	      let x1 = (false,x1') in
	      let middle = test false (bob b2) in
	      let remaining () = for_all x1 (p2',ar2) in
	      if v2=zero
	      then
		match middle,test false p2' with
		  | true ,true  -> remaining ()
		  | _ -> false
	      else
		match test false false,test false p2' with
		  | true ,true  -> middle && remaining ()
		  | _ -> false
	    )
	  else (*v1=v2*)
	    (
	      let p1' = alter_parity b1 in
	      let p2' = alter_parity b2 in
	      let middle = test (bob b1) (bob b2) in
	      let remaining () = for_all (p1',ar1) (p2',ar2) in
	      if v1=zero
	      then
		match middle,test p1' p2' with
		  | true ,true -> remaining ()
		  | _ -> false
	      else
		match test false false,test p1' p2' with
		  | true ,true -> middle && remaining ()
		  | _ -> false
	    )
      | _ -> (*at least one of the arguments is empty*)
	let b = is_empty ar1 in
	match test (not b) b,test false false with
	  | true,false  -> if b then is_full ar2 else is_full ar1
	  | false,true  -> if b then is_not_empty ar2 else is_not_empty ar1
	  | true,true   -> true
	  | false,false -> false

  let is_included = for_all Pervasives.(<=)

  let is_not_included = exists Pervasives.(>)

  (* The compare function is actually a linear extension of both
     inclusion relation and linear order over the elements. It is
     based on the following theoretical definition:
     min(B\A) ⩽  min(A\B) <=> A ⩽ B *)

  let compare ar1 ar2 =
    let rec compare p ar1 ar2 = match ar1,ar2 with
      | (b1::ar1),(b2::ar2) ->
        let x1 = rvb b1 in
        let x2 = rvb b2 in
        if x1 < x2
        then
          if p then -1 else 1
        else
          if x2 < x1
          then
            if p then 1 else -1
          else
            (
              match b1,b2 with
              | Pun _,Iso _
              | Pun _,Opn _
              | Cls _,Iso _
              | Cls _,Opn _ ->  1
              | Iso _,Cls _
              | Iso _,Pun _
              | Opn _,Pun _
              | Opn _,Cls _ -> -1
              | Pun _,Cls _
              | Iso _,Opn _
              | Cls _,Pun _
              | Opn _,Iso _ -> if p then  1 else -1
              | Cls _,_
              | Opn _,_ -> compare (not p) ar1 ar2
              | _ -> compare p ar1 ar2
            )
      | [],ar2 ->
	if ar2<>[] (*the second argument is neither empty nor full*)
	then
	  if p then 1 (*full*) else -1 (*empty*)
	else 0
      (*the first argument is neither empty nor full*)
      | _ -> if p then -1 else 1
    in compare false ar1 ar2

  let add_zero a = match a with
    | Iso x :: _ | Cls x :: _ -> if x <> zero then Iso zero :: a else a
    | Opn x :: a' -> if x <> zero then Iso zero :: a else Cls zero :: a'
    | [] -> [Iso zero]
    | _ -> a

  let remove_zero a = match a with
    | Iso x::a' -> if x <> zero then a else a'
    | Cls x::a' -> if x <> zero then a else (Opn x) :: a'
    | _ -> a

let left_bound it = 
  match it with 
  | [Iso x] -> Cls x
  | [a] | [a;_] -> a
  | _ -> assert false

let right_bound it = 
  match it with
  | [Iso x] -> Cls x 
  | [_;b] -> b
  | [_] | [] -> raise Undefined
  | _ -> assert false


let initial_hull it = match it with
  | [_;b] -> [Cls zero;b]
  | [Iso x] -> if x = zero then it else [Cls zero;Cls x]
  | [_] -> full
  | [] -> it (*empty*)
  | _ -> assert false

let terminal_hull it = match it with
  | [a;_] -> [a]
  | [Iso x] -> [Cls x]  
  | [_] -> it
  | [] -> []
  | _ -> assert false

(* is it2 strictly after it1 ? *)

let ordered_disjoint it1 it2 = 
  try
    let b1 = right_bound it1 in
    let a2 = left_bound it2 in
    let y1 = rvb b1 in
    let x2 = rvb a2 in 
    (y1 < x2) || (y1 = x2 && (not (bob b1) || not (bob a2)))
  with Undefined -> false

let (<|<) = ordered_disjoint

let ordered_disconnected it1 it2 = 
  try
    let b1 = right_bound it1 in
    let a2 = left_bound it2 in
    let y1 = rvb b1 in
    let x2 = rvb a2 in 
    (y1 < x2) || (y1 = x2 && not (bob b1) && not (bob a2))
  with Undefined -> false

let (<-<) = ordered_disconnected

let disconnected it1 it2 = (it1 <-< it2) || (it2 <-< it1)

let is_in_the_initial_hull_of it1 it2 =
  (is_empty it1) || ((is_not_empty it2) &&
  try 
    let b2 = right_bound it2 in
    (try 
      let b1 = right_bound it1 in
      let y2 = rvb b2 in
      let y1 = rvb b1 in
      y1 < y2 || (y1 = y2 && (bob b2 || not (bob b1)))
    with Undefined -> false)
  with Undefined -> true)

let is_in_the_terminal_hull_of it1 it2 =
  (is_empty it1) || ((is_not_empty it2) &&
    let a2 = left_bound it2 in
    let a1 = left_bound it1 in
    let x2 = rvb a2 in
    let x1 = rvb a1 in
    x2 < x1 || (x2 = x1 && (bob a2 || not (bob a1))))
    
(*
  it1 << it2 signifie que l'ensemble des minorants stricts de it1 est 
  inclus dans celui de it2, et dans le cas où ils sont égaux, 
  le plus petit segment initial contenant it1 est inclus dans 
  le plus petit segment initial contenant it2.
*)

let compare_intervals it1 it2 = 
  if it1 = it2 then 0
  else if is_empty it1 then 1
    else if is_empty it2 then -1
      else if not (is_in_the_terminal_hull_of it2 it1) then 1
        else if not (is_in_the_terminal_hull_of it1 it2) then -1 
          else if is_in_the_initial_hull_of it1 it2 then -1
            else 1
  
   let (<<) it1 it2 = Pervasives.(<=) (compare_intervals it1 it2) 0

    
(*
  Idea: denote by at3 the union of at1 and at2. Every connected component it1 
  of at1 is included in a connected component it3 of at3. If it1 is the 
  leftmost connected component of at1 contained in it3, then the connected 
  component of (future_extension at1 at2) containing it1 is given by (meet 
  (terminal_hull it1) it3).
*)

let future_extension at1 at2 =
  let answer = ref [] in
  let at2 = ref (join at1 at2) in  
  let hnt1 = head_and_tail at1 in
  let it1 = ref (fst hnt1) in
  let at1 = ref (snd hnt1) in
  let hnt2 = head_and_tail !at2 in
  let () = at2 := snd hnt2 in
  let it2 = ref (fst hnt2) in
  let () =
    try
      while true do
        while !it2 <-< !it1 do
          let hnt2 = head_and_tail !at2 in
          it2 := fst hnt2;
          at2 := snd hnt2;
        done;
        answer := (meet !it2 (terminal_hull !it1)) :: !answer;
        try 
          while is_in_the_initial_hull_of !it1 !it2 do
            let hnt1 = head_and_tail !at1 in
            it1 := fst hnt1;
            at1 := snd hnt1;
          done;
        with Undefined -> raise Exit
      done 
    with Exit -> () in
  answer := List.rev !answer;
  of_intervals !answer

let future_extension at1 at2 =
  if is_empty at1 then empty 
  else future_extension at1 at2


let past_extension at1 at2 =
  let answer = ref [] in
  let at2 = ref (join at1 at2) in  
  let hnt1 = head_and_tail at1 in
  let it1 = ref (fst hnt1) in
  let at1 = ref (snd hnt1) in
  let hnt2 = head_and_tail !at2 in
  let () = at2 := snd hnt2 in
  let it2 = ref (fst hnt2) in 
  let () = 
    try
      while true do
        let () = 
          while !it2 <-< !it1 do
            let hnt2 = head_and_tail !at2 in
            it2 := fst hnt2;
            at2 := snd hnt2;
          done in
        let prev_it1 = ref !it1 in
        let prev_at1 = ref !at1 in
        let () = try
          while is_in_the_initial_hull_of !it1 !it2 do
            prev_it1 := !it1;
            prev_at1 := !at1;
            let hnt1 = head_and_tail !at1 in
            it1 := fst hnt1;
            at1 := snd hnt1
          done;
        with Undefined -> 
          answer := (meet !it2 (initial_hull !it1)) :: !answer;
          raise Exit in
        answer := (meet !it2 (initial_hull !prev_it1)) :: !answer
      done
    with Exit -> () in
  answer := List.rev !answer;
  of_intervals !answer

let past_extension at1 at2 =
  if is_empty at1 then empty 
  else past_extension at1 at2


  (*The name is not well chosen in the case where the underlying space is the circle*)
  (*The flag is set if the result is unbounded, and left as is otherwise*)

  let first_connected_component ?flag a =
    let set_flag () = match flag with
      | Some flag -> flag := true
      | None -> () in
    match a with
      | (Iso x as b) :: _ -> [b]
      | b :: Pun x :: _ -> [b ; Opn x]
      | b :: b' :: _ -> [b ; b']
      | [_] -> set_flag () ; a
      | [] -> []

  (* last_connected_component was not tested *)

  let rec last_connected_component ?(parity=false) a =
    match a with
      | [_;Pun x] -> [Opn x]
      | [_;Iso _ as answer] -> [answer]
      | [Pun x ; b'] -> [Opn x ; b']
      | [Iso _ ; b'] -> [b']
      | [b ; b'] -> if parity then [b'] else [b ; b']
      | ([Iso _] as answer)
      | ([Opn _] as answer)
      | ([Cls _] as answer) -> answer
      | b :: a ->
          if alter_parity b
          then last_connected_component ~parity:(not parity) a
          else last_connected_component ~parity a
      | _ -> []

  (* internal *)

  let rec end_bound ?lb a = match a with
    | (Opn x as b)::a
    | (Cls x as b)::a -> (
        match lb with
        | Some _ -> end_bound a
        | None -> end_bound ~lb:b a)
    | (Iso x as b)::a -> end_bound ~lb:b a
    | Pun _::a -> end_bound a
    | [] -> (
        match lb with
        | Some b -> b
        | None -> raise Undefined)

  let end_bound_below y a =
    let terminal lb =
      match lb with
      | Some b -> b
      | None -> Cls y in
    let rec end_bound_below ?lb a = match a with
      | (Opn x as b)::a
      | (Cls x as b)::a ->
          if x <= y
          then (
            match lb with
              | Some _ -> end_bound a
              | None -> end_bound_below ~lb:b a)
          else terminal lb
      | (Iso x as b)::a ->
          if x <= y
          then end_bound_below ~lb:b a
          else terminal lb
      | Pun x::a ->
          if x <= y
          then end_bound_below a
          else terminal lb
      | [] -> terminal lb in
    end_bound_below a

  (* Directed operators *)

  (* Subset of the points of the second argument that can be reached
     from a point of the first argument running along a directed path
     whose trace is included in the set theoretic union of both
     arguments. A.k.a in_the_future_of in the signature of Interval.*)

  (* the option flag is a boolean reference. When provided, it is set
     if the output of future_extension is unbounded and left as is
     otherwise. This side-effect is used to deduce the function
     future_extension on the directed circle from the function
     future_extension on the directed half-line.*)

(*Add any point p that can be reached by a directed path starting in x, 
arriving at p, and whose image is entirely contained in the union of x and {p} 
*)

  let future_closure ?(circle_mode=false) ar =
    let unbounded = ref false in
    let rec future_closure loading ar = match ar with
      | Iso b :: ar -> Iso b :: future_closure false ar
      | Pun b :: ar -> future_closure true ar
      | (Cls b as x) :: ar
      | (Opn b as x) :: ar ->
          if loading
          then Cls b :: (future_closure false ar)
          else x :: (future_closure true ar)
      | [] -> (unbounded := loading ; []) in
    let return = future_closure false ar in (
    if !unbounded && circle_mode
    then add_zero return
    else return) , !unbounded

(*Add any point p that can be reached by an antidirected path starting in x, 
arriving at p, and whose image is entirely contained in the union of x and {p} 
*)

  let past_closure ar =
    let unbounded = ref false in
    let rec past_closure loading ar = match ar with
      | Iso b :: ar -> Iso b :: past_closure false ar
      | Pun b :: ar -> past_closure true ar
      | (Cls b as x) :: ar
      | (Opn b as x) :: ar ->
          if loading
          then x :: past_closure false ar
          else Cls b :: past_closure true ar
      | [] -> (unbounded := loading ; []) in
    let return = past_closure false ar in
    return , !unbounded

(*
  TODO: implement meet, the template below is the code of future_extension
*)

let rightmost_left_bound it1 it2 =
  let a1 = left_bound it1 in
  let a2 = left_bound it2 in
  let x1 = rvb a1 in
  let x2 = rvb a2 in
  if x1 < x2 || (x1 = x2 && (bob a1)) 
  then a2 else a1


let leftmost_right_bound it1 it2 =
  try
    let b1 = right_bound it1 in (
    try
      let b2 = right_bound it2 in
      let y1 = rvb b1 in
      let y2 = rvb b2 in
      if y1 < y2 || (y1 = y2 && (bob b2))
      then b1 else b2
    with Undefined -> b1)
  with Undefined -> right_bound it2


let meet it1 it2 =
  if it1 <|< it2 || it2 <|< it1 
  then raise Undefined
  else 
    let a = rightmost_left_bound it1 it2 in (
    try
      let b = leftmost_right_bound it1 it2 in
      if a <> b then [a;b] else [Iso (rvb a)]
    with Undefined -> [a])
  

let meet at1 at2 =
  let answer = ref [] in
  let it1 = ref empty in
  let at1 = ref at1 in
  let it2 = ref empty in
  let at2 = ref at2 in  
  let () = pop it1 at1 ; pop it2 at2 in
  let () =
    try
      while true do
        let () = try answer := (meet !it1 !it2) :: !answer
          with Undefined -> () in
        if is_in_the_initial_hull_of !it1 !it2
        then pop it1 at1
        else pop it2 at2
      done 
    with Exit -> () in
  let () = answer := List.rev !answer in
  of_intervals !answer

let meet at1 at2 =
  if is_empty at1 || is_empty at2 then empty 
  else meet at1 at2


let leftmost_left_bound it1 it2 =
  let a1 = left_bound it1 in
  let a2 = left_bound it2 in
  let x1 = rvb a1 in
  let x2 = rvb a2 in
  if x1 < x2 || (x1 = x2 && (bob a1)) 
  then a1 else a2

let rightmost_right_bound it1 it2 =
  let b1 = right_bound it1 in 
  let b2 = right_bound it2 in
  let y1 = rvb b1 in
  let y2 = rvb b2 in
  if y1 < y2 || (y1 = y2 && (bob b2))
  then b2 else b1

let join it1 it2 =  
  if is_empty it1 then it2
  else if is_empty it2 then it1
    else (
      if (it1 <-< it2) || (it2 <-< it1) then raise Undefined
        else
          let a = leftmost_left_bound it1 it2 in 
          try
            let b = rightmost_right_bound it1 it2 in
            if a <> b then [a;b] else [Iso (rvb a)]
          with Undefined -> [a])

(* More efficient implementation under the hypothesis that it2 is nonempty and 
if it1 is nonempty, then it1 << it2. *)

let ordered_join it1 it2 = 
  if is_empty it1 then it2
  (*else if is_empty it2 then it1*)
    else (
      if (it1 <-< it2) (*|| (it2 <-< it1)*) then raise Undefined
        else
          let a = (*leftmost_*)left_bound it1 (*it2*) in 
          try
            let b = rightmost_right_bound it1 it2 in
            if a <> b then [a;b] else [Iso (rvb a)]
          with Undefined -> [a])


let join at1 at2 =
  let answer = ref [] in
  let at1 = ref at1 in
  let at2 = ref at2 in
  let accu = ref empty in
  let update it at at' =
    try accu := ordered_join !accu it; at := at'  
    with Undefined -> (
      push !accu answer;
      accu := it;
      at := at') in
  let () =
    try
      while true do
        let (it1,at3) = try head_and_tail !at1 
          with Undefined -> (empty,empty) in
        let (it2,at4) = try head_and_tail !at2 
          with Undefined -> (empty,empty) in
        if is_empty it1 && is_empty it2 then raise Exit;
        if it1 << it2
        then update it1 at1 at3
        else update it2 at2 at4
      done 
    with Exit -> () in
  of_intervals (List.rev (!accu::!answer))

let future_extension at1 at2 = 
  let answer = ref [] in
  let first_operand = ref false in
  let loading = ref false in
  let at1 = ref at1 in
  let at2 = ref at2 in
  let accu = ref empty in
  let update it at at' =
    try 
      let x = ordered_join !accu it in 
      if !first_operand && (not !loading)
      then (loading := true; accu := meet (terminal_hull it) x) 
      else accu := x; 
      at := at'
    with Undefined -> 
      if !loading then push !accu answer;
      accu := it; loading := !first_operand; at := at' in
  let () =
    try
      while true do
        let (it1,at3) = try head_and_tail !at1
          with Undefined -> (empty,empty) in
        let (it2,at4) = try head_and_tail !at2
          with Undefined -> (empty,empty) in
        if is_empty it1 && is_empty it2 then raise Exit;
        if it1 << it2
        then (first_operand := true; update it1 at1 at3)
        else (first_operand := false; update it2 at2 at4)
      done
    with Exit -> () in
  of_intervals (List.rev (if !loading then !accu::!answer else !answer))











let future_extension at1 at2 =
  let answer = ref [] in
  let first_operand = ref false in
  let at1 = ref at1 in
  let at2 = ref at2 in
  let first_it1 = ref empty in
  let accu = ref empty in
  let update it at at' =
    try
      accu := ordered_join !accu it; at := at'; 
      if !first_operand && is_empty !first_it1 then first_it1 := it;  
    with Undefined -> 
      push (meet (terminal_hull !first_it1) !accu) answer;
      accu := it;
      (if !first_operand then first_it1 := it else first_it1 := empty);
      at := at' in
  let () =
    try
      while true do
        let (it1,at3) = try head_and_tail !at1 
          with Undefined -> (empty,empty) in
        let (it2,at4) = try head_and_tail !at2 
          with Undefined -> (empty,empty) in
        if is_empty it1 && is_empty it2 then raise Exit;
        if it1 << it2
        then (first_operand := true; update it1 at1 at3)
        else (first_operand := false; update it2 at2 at4)
      done 
    with Exit -> () in
  of_intervals (List.rev (((meet (terminal_hull !first_it1) !accu))::!answer))




let past_extension at1 at2 =
  let answer = ref [] in
  let first_operand = ref false in
  let at1 = ref at1 in
  let at2 = ref at2 in
  let last_it1 = ref empty in
  let accu = ref empty in
  let update it at at' =
    try 
      accu := ordered_join !accu it; at := at'; 
      if !first_operand then last_it1 := it;  
    with Undefined -> 
      push (meet (initial_hull !last_it1) !accu) answer;
      accu := it;
      if !first_operand then last_it1 := it;
      at := at' in
  let () =
    try
      while true do
        let (it1,at3) = try head_and_tail !at1 
          with Undefined -> (empty,empty) in
        let (it2,at4) = try head_and_tail !at2 
          with Undefined -> (empty,empty) in
        if is_empty it1 && is_empty it2 then raise Exit;
        if it1 << it2
        then (first_operand := true; update it1 at1 at3)
        else (first_operand := false; update it2 at2 at4)
      done 
    with Exit -> () in
  of_intervals (List.rev (((meet (initial_hull !last_it1) !accu))::!answer))

  module type DirectedTopology = sig
    val string_of: t -> string
    val interior: t -> t
    val closure: t -> t
    val future_extension: t -> t -> t
    val past_extension: t -> t -> t
  end

module HalfLine
	=
struct

  (* Tests *)

  let is_bounded a = parity true a

  let is_not_bounded a = parity false a

  let string_of ?(empty_set_denotation="Ø") ?(infinity_denotation="+oo") ?(open_infinity=true) a =
    let rec string_of p a = match a with
      |	Cls x::a ->
					if p
					then (B.string_of x)^(if a<>[] then "] " else "]")^(string_of (not p) a)
					else "["^(B.string_of x)^","^(string_of (not p) a)
      |	Opn x::a ->
					if p
					then (B.string_of x)^(if a<>[] then "[ " else "[")^(string_of (not p) a)
					else "]"^(B.string_of x)^","^(string_of (not p) a)
      |	Iso x::a -> "{"^(B.string_of x)^(if a <>[] then "} " else "}")^(string_of p a)
      |	Pun x::a -> let b = B.string_of x in b^"[ ]"^b^","^(string_of p a)
      | _ ->
					if p
					then if open_infinity then infinity_denotation^"[" else infinity_denotation^"]"
					else if open_infinity then "" else " {"^infinity_denotation^"}" in
    let answer = string_of false a in
    if answer <> "" then answer else empty_set_denotation

  let string_of x = string_of x

  let closure_contains_zero a =
    match a with 
      | x::_ -> rvb x = zero
      | [] -> false
  
  let interior_contains_zero a =
    match a with 
      | x::_ -> x = Cls zero
      | [] -> false

  let interior_does_not_contain_zero a =
    match a with 
      | x::_ -> x <> Cls zero
      | [] -> true

  let glb a = match a with
    | b::a -> rvb b
    | [] -> raise Undefined

  let rec lub a = 
    match a with
    | [] -> zero
    | [Iso x] -> x
    | [Opn _] | [Cls _] | [Pun _] -> raise Undefined
    | [_;Opn x] | [_;Cls x] -> x
    | (Iso x) :: a -> lub a
    | _ :: (Opn x) :: a | _ :: (Cls x) :: a -> lub a
    | _ :: (((Pun _) :: a) as b) -> lub b
    | _ -> assert false

  (* Warning: If zero belongs to some subset of the
     topological space [0,+oo[ then zero also belong to its
     interior *)

  (* Topological operators *)

  let interior a =
    let rec interior a = match a with
      | Iso _::a -> interior a
      | Cls x::a -> Opn x::interior a
      | b::a -> b::interior a
      | [] -> []
    in
    match a with
      | Cls x::a -> if x<>zero then Opn x::interior a else Cls zero::interior a
      | _ -> interior a


  let closure ?unbounded_answer a = (*enhanced version*)
    let rec closure p a = match a with
      | Pun _::a -> closure p a
      | Opn x::a -> Cls x::closure (not p) a
      | Iso _ as b::a -> b::closure p a
      | Cls _ as b::a -> b::closure (not p) a
      | [] -> (match unbounded_answer with | Some br -> br := p | _ -> ()); []
    in
    closure false a

  let rec closure a = match a with (*basic version*)
    | Pun _::a -> closure a
    | Opn x::a -> Cls x::closure a
    | b::a -> b::closure a
    | [] -> []

  let future_closure ar = future_closure ~circle_mode:false ar

  let future_extension ar1 ar2 = future_extension ar1 ar2
  let past_extension   ar1 ar2 = past_extension   ar1 ar2

  let boundary a = match a with
    | Cls x::a ->
        if x<>zero
        then Iso x::List.map (fun b -> Iso (rvb b)) a
        else List.map (fun b -> Iso (rvb b)) a
    | b::a -> Iso (rvb b)::List.map (fun b -> Iso (rvb b)) a
    | [] -> []

  let rec connected_components a = match a with
    | [] -> []
    | [Opn x] 
    | [Cls x] -> [a]
    | Iso x :: a -> [Iso x] :: connected_components a
    | Opn x :: Opn y :: a -> [Opn x ; Opn y] :: connected_components a
    | Opn x :: Pun y :: a -> [Opn x ; Opn y] :: connected_components ((Opn y) :: a)
    | Opn x :: Cls y :: a -> [Opn x ; Cls y] :: connected_components a
    | Cls x :: Opn y :: a -> [Cls x ; Opn y] :: connected_components a
    | Cls x :: Pun y :: a -> [Cls x ; Opn y] :: connected_components ((Opn y) :: a)
    | Cls x :: Cls y :: a -> [Cls x ; Cls y] :: connected_components a
    | _ -> invalid_arg "HalfLine.connected_components"

end(*HalfLine*)


module Circle
	=
struct

  (* Tests *)

  let closure_contains_zero a =
    parity false a || 
    (match a with 
      | x::_ -> rvb x = zero
      | [] -> false)
  
    
	(*Added on Wednesday, the 23th of November 2011*)
  (*Rewritten on Sunday, the 28th of May 2017*)
  (*I'm a bit doubtful about this function e.g. [] or [Cls 1;Cls 2] *)
	let boundary_contains_zero a =
    match a with 
      | x::_ -> (
          x = Opn zero
          || (rvb x <> zero && parity false a)
          || (x = Cls zero && parity true a))
      | [] -> false
  
	(*Added on Wednesday, the 23th of November 2011*)
	let boundary_does_not_contain_zero a =
    match a with 
      | x::_ -> (
          x <> Opn zero
          && (rvb x = zero || parity true  a)
          && (x <> Cls zero || parity false a))
      | [] -> true


  let interior_contains_zero a =
    parity false a && List.hd a = Cls zero

  let interior_does_not_contain_zero a =
    parity true a || List.hd a <> Cls zero

  (* Display *)

  let string_of ?(empty_set_denotation="Ø") ?(full_set_denotation="S¹") a =
    let raw_string_of = string_of a in
    let last_bound = ref "" in
    let rec string_of p a = match a with
      | Cls x::a ->
					if a <> []
					then
					  if p
					  then (B.string_of x)^")"^(string_of false a)
					  else " ("^(B.string_of x)^","^(string_of true a)
					else
					  if p
					  then (B.string_of x)^")"
					  else (last_bound := "("^(B.string_of x)^"," ;"")
      | Opn x::a ->
					if a <> []
					then
					  if p
					  then (B.string_of x)^"("^(string_of false a)
					  else " )"^(B.string_of x)^","^(string_of true a)
					else
					  if p
					  then (B.string_of x)^"("
					  else (last_bound := ")"^(B.string_of x)^"," ;"")
      | Pun x::a ->
					if a <> []
					then (B.string_of x)^"( )"^(B.string_of x)^","^(string_of true a)
					else (last_bound := ")"^(B.string_of x)^"," ; (B.string_of x)^"(")
      | Iso x::a -> " {"^(B.string_of x)^"}"^(string_of false a)
      | [] -> ""
    in
    match a with
      | Iso x::a ->
					let answer = string_of false a in
					if !last_bound <> ""
					then
					  if x <> zero
					  then !last_bound^(B.string_of zero)^"( {"^(B.string_of x)^"}"^answer
					  else !last_bound^(B.string_of zero)^")"^answer
					else
					  "{"^(B.string_of x)^"}"^answer
      | [Cls x] ->
					if x<>zero
					then Printf.sprintf "(%s,%s(" (B.string_of x) (B.string_of zero)
					else full_set_denotation
      | [Cls x;Pun y] ->
					if x<>zero
					then (Printf.sprintf ")%s,%s( (%s,%s(" (B.string_of y) (B.string_of zero) (B.string_of x) (B.string_of y))
					else Printf.sprintf "%s\\{%s}" full_set_denotation (B.string_of y)
      | Cls x::a ->
					let answer = string_of true a in
					if !last_bound <> ""
					then
					  if x <> zero
					  then !last_bound^(B.string_of zero)^"( ("^(B.string_of x)^","^answer
					  else !last_bound^answer
					else "("^(B.string_of x)^","^answer
      | [Opn x] ->
					if x<>zero
					then Printf.sprintf ")%s,%s(" (B.string_of x) (B.string_of zero)
					else Printf.sprintf "%s\\{%s}" full_set_denotation (B.string_of x)
      | Opn x::a ->
					let answer = string_of true a in
					if !last_bound <> ""
					then
					  if x <> zero
					  then !last_bound^(B.string_of zero)^"( )"^(B.string_of x)^","^answer
					  else !last_bound^(B.string_of zero)^"( )"^(B.string_of zero)^","^answer
					else ")"^(B.string_of x)^","^answer
      | [] -> empty_set_denotation
      | _ ->
	(
	  Printf.sprintf
	    "Invalid form: oda.ml: Circle.string_of: Pun x is not allowed at the head: %s"
	    raw_string_of
	)

  let string_of x = string_of x

  (* Topological operators *)

  let interior a =
    let unbounded = ref false in
    let rec interior u a = match a with
      | Iso _::a -> interior u a
      | Cls x::a | Opn x::a -> Opn x::interior (not u) a
      | Pun x::a -> Pun x::interior u a
      | [] -> unbounded:=u;[]
    in
    let answer = interior false a in
    match a with
      | Cls x::_ -> if x=zero && !unbounded then Cls x::(List.tl answer) else answer
      | _ -> answer

  let closure a =
    let unbounded = ref false in
    let rec closure u a = match a with
      | Pun _::a -> closure u a
      | Opn x::a | Cls x::a -> Cls x::closure (not u) a
      | Iso x::a -> Iso x::closure u a
      | [] -> unbounded:=u;[]
    in
    let answer = closure false a in
    if !unbounded && (try List.hd answer <> Cls zero with | _ -> (print_endline "Iso zero should be added" ; false))
    then Iso zero::answer
    else answer

  let boundary a =
    let argument_contains_a_right_neighborhood_of_zero = ref false in
    let inside = ref false in
    let parity_update b = if alter_parity b then inside := not !inside in
    let boundary = match a with
      | Cls x::a -> inside := true;
	if x<>zero
	then
	  Iso x::List.map (fun b -> parity_update b ; Iso (rvb b)) a
	else
	  (
	    argument_contains_a_right_neighborhood_of_zero := true ;
	    List.map (fun b -> parity_update b ; Iso (rvb b)) a
	  )
      | b::a -> parity_update b;
	Iso (rvb b)::List.map (fun b -> parity_update b ; Iso (rvb b)) a
      | [] -> []
    in
    if
      (!inside && not !argument_contains_a_right_neighborhood_of_zero)
      || (not !inside && !argument_contains_a_right_neighborhood_of_zero)
    then Iso zero::boundary
    else boundary

  (* Directed operators *)

  (*If at2 starts with [0,... and HalfLine.future_extension at1 at2 or
    at1 is unbounded, then [0,... has to be "added" to the result. In
    particular, it requires future_extension to be able to tell when
    the result is unbounded. It is the case iff at2 is unbounded and
    (the last bound of at1 is in touch with the last bound of
    at2). This can be done by passing an optional reference to
    HalfLine.future_extension.*)

  let future_extension at1 at2 =
    let at3 = future_extension at1 at2 in
    if HalfLine.is_bounded at3 || does_not_contain_zero at2
    then at3
    else join (first_connected_component at2) at3

  let future_closure at = future_closure ~circle_mode:true at

  (*If at1 or HalfLine.past_extension at1 at2 contains zero, then the
    unbounded connected component of at2, if any, is included in the
    result. In particular, it requires that HalfLine.past_extension to
    be able to systematically add the unbounded connected component if
    it is asked to. This can be done by using an option.*)

  let past_extension at1 at2 =
    let at3 = past_extension at1 at2 in
    if contains_zero at3 && HalfLine.is_not_bounded at2
    then join at3 (last_connected_component at2)
    else at3

end(*Circle*)

(* Enumerator / Iterator *)

(* Iterators/Enumerators *)

  (* Énumère les intervalles *)
  (* The function next is supposed to raise Exit if one is beyond the last 
  element of the enumeration *)
  let next_interval next it = 
    let next_bound b = match b with
      | Opn y -> Cls y
      | Cls y -> Opn (next y)
      | _ -> assert false in
    let answer = 
    match it with
      | [Iso x] -> (try [Cls x; Opn (next x)] with Exit -> [Cls x])
      | [((Opn x) as a); b]  
      | [((Cls x) as a); b] -> (try [a; next_bound b] with Exit -> [a]) 
      | [Cls x] -> (try [Opn x; Opn (next x)] with Exit -> [Opn x])
      | [Opn x] -> (try [Iso (next x)] with Exit -> [])
      | [] -> raise Exit
      | _ -> assert false
    in 
    answer

  let nonempty_disconnected_next next it =
    let answer =
    match it with
      | [Opn _] | [Cls _] -> raise Exit
      | [Opn _; Opn x] | [Cls _; Opn x] -> (
          try [Opn x; Opn (next x)]
          with Exit -> [Opn x])
      | [Iso x] | [Opn _;Cls x] | [Cls _;Cls x] -> [Iso (next x)]
      | [] -> raise Exit
      | _ -> assert false in
    answer
    
  
  (* Une région est ici représentée par une liste décroissante d'intervalles, 
  l'ordre I < J signifiant que tout point de I est strictement inférieur à 
  tout point de J et que I et J sont déconnectés. Pour des raisons pratiques, 
  la fonction next region prend les listes renversées. La région vide est 
  représentée par la liste vide. Aucun intervalle de la liste n'est vide. *)
  
  let lub_region re = 
    let rec lub_region re = 
      match re with 
        | [Cls x] | [Opn x] | [Pun x] | [Iso x] -> x 
        | b :: re -> lub_region re
        | [] -> raise Undefined in
    if HalfLine.is_bounded re && re <> []
    then lub_region re
    else raise Undefined
    
  let next_region next_value re = 
    let next it = 
      let next = next_interval next_value it in
      if next <> [] then next else raise Exit in
    let next_with_lesser_lub it =
      let x = ref it in
      let y = ref (next it) in
      while (
        (HalfLine.is_bounded !x) && (not (HalfLine.is_bounded !y)) || 
        (HalfLine.is_bounded !x && HalfLine.is_bounded !y && HalfLine.lub !x <= HalfLine.lub !y))
      do x := !y ; y := next !y
      done;
      !y in
    let rec init k re =
      if (Pervasives.(>)) k 0 
      then 
        init (pred k) (
          match re with 
            | it :: _ -> nonempty_disconnected_next next_value it :: re
            | []      -> [atom zero])
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
        | [] -> [atom zero]
        | _ -> next_region (List.length re) re 
    with Exit -> init (succ (List.length re)) []

(* A region is an increasing sequence of disconnected intervals *)

  let next next_value at = 
    let re = intervals_of at in
    let re = next_region next_value re in
    of_intervals re

  let iterator next_value = 
    let current = ref empty in
    fun () -> (
      let answer = !current in
      current := next next_value !current;
      answer)

end (* BooleanAlgebra *)

module Make(B:Bound.S): S with type value = B.t 
  = Raw (B:Bound.S)


(*

TODO: Une version basée uniquement sur les intervalles. Déterminer une 
signature minimale pour le «foncteur intervalle» telle que l'on puisse écrire 
un «foncteur région» uniquement dessus.

*)
