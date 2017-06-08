  module Integer =
  struct

    type t = Numerix.Slong.t
    let zero = Numerix.Slong.zero
    let one = Numerix.Slong.one
    let add = Numerix.Slong.add
    let sub = Numerix.Slong.sub
    let mul = Numerix.Slong.mul
    let div = Numerix.Slong.quo
    let neg = Numerix.Slong.neg
    let of_string = Numerix.Slong.of_string
    let string_of = Numerix.Slong.string_of
    let rst = Numerix.Slong.modulo
    let is_prime x =
      match Numerix.Slong.isprime x with
	| Numerix.True -> true
	| _ -> false

    let rec valence ?(pa=0) x d =
      if Numerix.Slong.modulo x d = zero
      then valence ~pa:(pa+1) (Numerix.Slong.quo x d) d
      else (pa,x)

    exception Exit of ((t*int)*t)

    let find_prime_divisor x =
      if Numerix.Slong.mod_1 x 2 = 0
      then
	let (fv,sv) = valence x (Numerix.Slong.of_int 2)
	in
	  (((Numerix.Slong.of_int 2),fv),sv)
      else
	(
	  try
	    let d= Numerix.Slong.make_ref (Numerix.Slong.of_int 3)
	    in
	    let upper_bound = Numerix.Slong.add_1 (Numerix.Slong.sqrt x) 1
	    in
	      while Numerix.Slong.supeq upper_bound  (Numerix.Slong.look d)
		(* for d=3 to (sqrt x)+1 *)
	      do
		let (fv,sv) = valence x (Numerix.Slong.look d)
		in
		  if fv <> 0
		  then
		    raise (Exit (((Numerix.Slong.look d),fv),sv))
		  else
		    Numerix.Slong.add_1_in d (Numerix.Slong.look d) 2
	      done
	      ;
	      ((x,1),one)
	  with
	    | (Exit out) -> out
	)

    let rec decompose x =
      if x = zero
      then raise Algebra.Zero
      else
	if x = one
	then []
	else
	  let d = find_prime_divisor x
	  in (fst d)::(decompose (snd d))

    let name = Numerix.Slong.name

    let compare = Numerix.Slong.cmp

    let default = zero

  end (* Integer *)
