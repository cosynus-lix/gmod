exception Does_not_know
exception Zero

module UTF8 =
struct

  let cartesian = "×"
  let big_cartesian = "⨉"
  let tensor = "⊗"
  let big_tensor = "⨂"
  let direct_sum = "⊕"
  let big_direct_sum = "⨁"
  let vee = "⋁"
  let wedge = "⋀"
  let union = "⋃"
  let intersection = "⋂"
  let negation = "¬"
  let lambda = "λ"
  let cdot = "⋅"

let superscript_digit d = match d with
  | '0' -> "⁰" | '1' -> "¹" | '2' -> "²" | '3' -> "³" | '4' -> "⁴"
  | '5' -> "⁵" | '6' -> "⁶" | '7' -> "⁷" | '8' -> "⁸" | '9' -> "⁹"
  | _   -> ""

let lowerscript_digit d = match d with
  | '0' -> "₀" | '1' -> "₁" | '2' -> "₂" | '3' -> "₃" | '4' -> "₄"
  | '5' -> "₅" | '6' -> "₆" | '7' -> "₇" | '8' -> "₈" | '9' -> "₉"
  | _   -> ""

let superscript_int ?(utf8=true) n =
  let pattern = string_of_int n in
  if utf8
  then Array.fold_left (fun accu d -> accu ^ d) ""
    (Array.init (String.length pattern) (fun k -> superscript_digit pattern.[k]))
  else "^" ^ pattern

let lowerscript_int ?(utf8=true) n =
  let pattern = string_of_int n in
  if utf8
  then Array.fold_left (fun accu d -> accu^d) ""
    (Array.init (String.length pattern) (fun k -> lowerscript_digit pattern.[k]))
  else "_" ^ pattern

end

module BooleanArray =
struct

  let init bin_op b b' =
    let f k = bin_op (Array.get b k) (Array.get b' k) in
    Array.init (Array.length b) f

  let weight ?below ba =
    match below with
      | None -> Array.fold_left (fun accu b -> if b then succ accu else accu) 0 ba
      | Some n ->
        let accu = ref 0 in
        let () = for i = 0 to n - 1 do
          if ba.(i) then incr accu
        done in
        !accu

  let coweight ?below ba =
    match below with
      | None -> Array.fold_left (fun accu b -> if not b then succ accu else accu) 0 ba
      | Some n ->
        let accu = ref 0 in
        let () = for i = 0 to n - 1 do
          if not ba.(i) then incr accu
        done in
        !accu

  let extract ba a =
    let n = Array.length a in
    let pos = ref (-1) in
    let next () = 
      incr pos; 
      while not (Array.get ba !pos) do 
        incr pos done in
    Array.init (weight ba) 
      (fun k -> next (); 
        if !pos < n then Array.get a !pos
        else failwith "extract")

  let extract_complement ba a =
    let n = Array.length a in
    let pos = ref (-1) in
    let next () = 
      incr pos ; 
      while (Array.get ba !pos) do 
        incr pos done in
    Array.init ((Array.length a)-(weight ba)) 
      (fun k -> next (); 
        if !pos < n then Array.get a !pos
        else failwith "extract")
        
  let first_occurrence b ba =
    let ans = ref 0 in
    let () = while ba.(!ans) = not b do
      incr ans
    done in
    !ans

  let last_occurrence b ba =
    let ans = ref ((Array.length ba) - 1) in
    let () = while ba.(!ans) = not b do
      decr ans
    done in
    !ans

  let next ba =
    let k = first_occurrence false ba in
    let f x = if x < k then false else (if x = k then true else Array.get ba x) in
    Array.init (Array.length ba) f

  let previous ba =
    let k = first_occurrence true ba in
    let f x = if x < k then true else (if x = k then false else Array.get ba x) in
    Array.init (Array.length ba) f

  let next_side_effect ba =
    let i = first_occurrence false ba in
    let () = for j = 0 to i - 1 do
      Array.set ba j false
    done in
    Array.set ba i true

  let previous_side_effect ba =
    let i = first_occurrence true ba in
    let () = for j = 0 to i - 1 do
      Array.set ba j true
    done in
    Array.set ba i false

  (* internal *)

  let first_pushable ba =
    let l = (Array.length ba)-1 in
    let ans = ref 0 in
    let () = while if !ans < l then not ((ba.(!ans)) && (not (ba.(succ !ans)))) else raise Exit do
      incr ans
    done in
    !ans

  let first_pullable ba =
    let l = (Array.length ba)-1 in
    let ans = ref 0 in
    let () = while if !ans < l then not ((ba.(succ !ans)) && (not (ba.(!ans)))) else raise Exit do
      incr ans
    done in
    !ans

  let next_on_its_level ba =
    let i = first_pushable ba in
    let l = Array.length ba in
    let w = weight ~below:i ba in
    let f k = if k < w || k = succ i then true else (if k <= i then false else ba.(k) ) in
    Array.init l f

  let previous_on_its_level ba =
    let i = first_pullable ba in
    let l = Array.length ba in
    let w = coweight ~below:i ba in
    let f k = if k < w || k = succ i then false else (if k <= i then true else ba.(k) ) in
    Array.init l f

  let next_on_its_level_side_effect ba =
    let i = first_pushable ba in
    let w = weight ~below:i ba in
    let f k b = if k < w || k = succ i then ba.(k) <- true else (if k <= i then ba.(k) <- false) in
    Array.iteri f ba

  let previous_on_its_level_side_effect ba =
    let i = first_pullable ba in
    let w = coweight ~below:i ba in
    Array.iteri (fun k b -> if k<w || k=succ i then ba.(k) <- false else (if k <= i then ba.(k) <- true)) ba

  let next_levelwise ba =
    let l = Array.length ba in
    let w = weight ba in
    if w < l
    then
      try next_on_its_level ba
      with Exit -> Array.init (Array.length ba) (fun i -> i <= w)
    else raise Exit

  let next_levelwise_side_effect ba =
    let l = Array.length ba in
    let w = weight ba in
    if w < l
    then
      try next_on_its_level_side_effect ba
      with Exit -> for i = 0 to l - 1 do ba.(i) <- (i <= w) done
    else raise Exit

  let previous_levelwise_side_effect ba =
    let l = Array.length ba in
    let w = coweight ba in
    if w < l
    then
      try previous_on_its_level_side_effect ba
      with Exit -> for i = 0 to l-1 do ba.(i) <- (i > w) done
    else raise Exit

  let previous_levelwise ba =
    let l = Array.length ba in
    let w = coweight ba in
    if w < l
    then
      try previous_on_its_level ba
      with Exit -> Array.init (Array.length ba) (fun i -> not(i <= w))
    else raise Exit

  let string_of ?rev ?(t="1") ?(f="0") ba =
    let lgt = Array.length ba in
    let aux = match rev with
      | None ->    Array.init lgt (fun k -> if ba.(k) then t else f)
      | Some () -> Array.init lgt (fun k -> if ba.(lgt-k-1) then t else f) in
    String.concat "" (Array.to_list aux)

  (*Returns the list of term indices which are true. If the option
    negative is set then the list of term indices which are false is
    returned.*)

  let support_of ?negative ba =
    let counter = ref (Array.length ba) in
    Array.fold_right
      (
      match negative with
        | None    -> fun b accu -> decr counter;if b then !counter::accu else accu
        | Some () -> fun b accu -> decr counter;if b then accu else !counter::accu
      ) ba []

  (* The function string_list of can be deduced from int_list_of by
     string_list_of ba = String.map string_of_int (int_list_of ba) *)

  let string_of_support ?(left="{") ?(separator=",") ?(right="}") ?negative ba =
    let counter = ref (Array.length ba) in
    let support =
      Array.fold_right
  (
    match negative with
      | None -> fun b accu -> decr counter; if b then (string_of_int !counter) :: accu else accu
      | Some () -> fun b accu -> decr counter; if b then accu else (string_of_int !counter) :: accu
  ) ba []
    in
    Printf.sprintf "%s%s%s" left (String.concat "," support)  right

  (*The factors list fl should be given in unpacked form*)

  let string_of_factors ?(left="") ?(separator=" ") ?(right="") fl =
    left^(String.concat separator (List.rev_map string_of_support fl))^right

  (*Returns the array of boolean b such that length b = s and
    b.(x)=true iff x appears in the list il*)

  let of_support il s =
    let rec of_int_list pos il = match il with
      | x::il -> Array.init ((succ x)-pos) (fun k -> k=x-pos)::of_int_list (succ x) il
      | []    -> [Array.make (max 0 (s-pos)) false]
    in
    Array.concat (of_int_list 0 il)

  (* to be described *)

  (*

     lba is a list of Boolean arrays that has to satisfy the following
     property : the length of an element in the number of occurences
     of "false" in the preceding one.

  *)

  let unpack lba =
    match lba with
      | x :: _ -> (
        let len = Array.length x in
        let pos = ref (-1) in
        let mask = Array.make len false in
        let f accu ba =
          let () = pos := -1 in
          let f k =
            if mask.(k)
            then false
            else (incr pos; if ba.(!pos) then (mask.(k) <- true; true) else false) in
            (Array.init len f) :: accu in
        List.fold_left f [] lba)
      | _ -> []

  (* On doit avoir length(aoba) = length(aow)-1 *)

  let interlaced_append aoba aow =
    let length_of_aow = Array.length aow in
    let positions = Array.make length_of_aow (ref (-1)) in
    Array.init
      (Array.fold_left (fun accu w -> accu+(Array.length w)) 0 aow) (* length of the answer *)
      (
        fun i ->
          let j = (* look up the index j such that i belongs to aos.(j) *)
            let k = ref 0
            in
            while (
              try not (aoba.(!k)).(i) 
              with Invalid_argument _ -> false)
            do
              incr k
            done;
            !k
          in
    (
      incr (Array.get positions j)
      ;
      (aow.(j)).(!(positions.(j)))
    )
      )

  let rec fold st f s a =
    let a' = f s a in
    try fold st f (st s) a'
    with Exit -> a'

  let rec fold_up f s a =
    let a' = f s a in
    try fold_up f (next s) a'
    with Exit -> a'

  let rec fold_down f s a =
    let a' = f s a in
    try fold_down f (previous s) a'
    with Exit -> a'

  let rec fold_up_levelwise f s a =
    let a' = f s a in
    try fold_up_levelwise f (next_levelwise s) a'
    with Exit -> a'

  let rec fold_down_levelwise f s a =
    let a' = f s a in
    try fold_down_levelwise f (previous_levelwise s) a'
    with Exit -> a'

  let rec iter st f s =
    let () = f s in
    try iter st f (st s)
    with Exit -> ()

  let rec iter_up f s =
    let () = f s in
    try iter_up f (next s)
    with Exit -> ()

  let rec iter_down f s =
    let () = f s in
    try iter_down f (previous s)
    with Exit -> ()

  let rec iter_up_levelwise f s =
    let () = f s in
    try iter_up_levelwise f (next_levelwise s)
    with Exit -> ()

  let rec iter_down_levelwise f s  =
    let () = f s in
    try iter_down_levelwise f (previous_levelwise s)
    with Exit -> ()

end (* BooleanArray *)

module Soi =
struct
  include Set.Make (struct type t = int let compare = Pervasives.compare end)

  (*

    segments : renvoie une liste d'intervalles 2 à 2 disjoints dont la
    réunion est l'ensemble passé en argument.

  *)

  (* Cette fonction lève l'exception Invalid_argument "index out of
     bounds" lorsque (a) ne contient que des valeurs (true) *)

  (* to be done ? *)

  let fold_subsets phi n a =
    let counter = Array.make n false in
    let answer = ref a in
    try
      let () = while true do
        answer := phi counter !answer;
        BooleanArray.next_side_effect counter
      done in
      !answer (* En fait cette ligne n'est jamais atteinte *)
    with Invalid_argument _ -> !answer

  (* To be tested *)

  (*

    interlaced_append renvoie un tableau de longueur length(a)+length(b)
    dont le terme d'indice i est ...

  *)

  let interlaced_append s a b =
    let position_a = ref 0 in
    let position_b = ref 0 in
      Array.init
  ((Array.length a)+(Array.length b))
  (
    fun i ->
      if mem i s
      then
        let current_a = !position_a in
    (position_a := succ current_a; Array.get a current_a)
      else
        let current_b = !position_b in
    (position_b := succ current_b; Array.get b current_b)
  )

  (* A finir *)

(*

   aos : est un tableau d'ensemble d'entiers positifs

   aow : est un ensemble de mots

   On doit avoir length(aos)=length(aow)-1

*)

  let interlaced_append_generalized aos aow =
    let length_of_aow = Array.length aow
    in
    let positions = Array.make length_of_aow (ref (-1))
    in
      Array.init
  (Array.fold_left (fun accu w -> accu+(Array.length w)) 0 aow) (* length of the answer *)
  (
    fun i ->
      let j = (* look up the index j such that i belongs to aos.(j) *)
        let k = ref 0
        in
    while (try not(mem i aos.(!k)) with | Invalid_argument _ -> false)
    do
      incr k
    done
    ;
    !k
      in
        (
    incr (Array.get positions j)
    ;
    (aow.(j)).(!(positions.(j)))
    (* Array.get (Array.get aow j) !(Array.get positions j) *)
        )
  )

  let of_boolean_array ba =
    let answer = ref empty in
      Array.iteri (fun pos b -> if b then answer := add pos !answer) ba

  let to_boolean_array n s =
    let answer = Array.make n false in
      iter (fun i -> Array.set answer i true) s

  let extract s a =
    let answer = 
      try Array.make (cardinal s) (Array.get a 0) 
      with Invalid_argument _ -> [||]
    and pos = ref 0 in
      iter (fun i -> (Array.set answer !pos (Array.get a i) ; incr pos)) s;
      answer

  (* Ne retient que les indices contenus dans {0,...,n-1}\s *)

  let extract_complement n s a =
    let answer = 
      try Array.make (n-(cardinal s)) (Array.get a 0) 
      with Invalid_argument _ -> [||] in
    let pos = ref 0 in
      for i = 0 to n-1 do
        if not(mem i s) 
        then (Array.set answer !pos (Array.get a i) ; incr pos)
      done
      ;
      answer

end (* Soi *)


(* Attention, on doit avoir card(l)=length(a) *)
(* On peut coder ça de façon beaucoup plus efficace avec des listes *)

module Integer =
struct

    type t = int
    let zero = 0
    let one = 1
    let neg x = -x
    let add x y = x + y
    let sub x y = x - y
    let mul x y = x * y
    let exp x n = (* On a bien la convention "0 puissance 0 = 1" *)
      let ans = ref 1 in
      let () = for i = 1 to n do
        ans := !ans*x done in
      !ans
    let div x y = x / y
    let rst x y = x mod y
    let string_of x = string_of_int x
    let of_string s = int_of_string s
    let of_list l = List.fold_left (fun accu (x,n) -> add accu (exp x n))
    let is_prime x =
      if x mod 2 = 0
      then false
      else
  (
    try
      for d=3 to succ (int_of_float(sqrt(float_of_int x)))
      do
        if (x mod d) = 0
        then
    raise Zero
        else
    ()
      done
      ;
      true
    with
      | Zero -> false
  )

    (* Renvoie la valence de d dans x (notée v) et le résultat de
       x/d^v *)

    let rec valence ?(pa=0) x d = if x mod d = 0 then valence ~pa:(pa+1) (x/d) d else (pa,x)

    exception MyExit of ((int*int)*int)

    let find_prime_divisor x =
      if x mod 2 = 0
      then let fv,sv = valence x 2 in (2,fv),sv
      else
  try
    for d=3 to succ (int_of_float(sqrt(float_of_int x)))
    do
      let (fv,sv) = valence x d in
      if fv <> 0
      then raise (MyExit ((d,fv),sv))
    done ;
    (x,1),1
  with | MyExit out -> out

    let rec factorization x =
      match x with
  | 0 -> raise Zero
  | 1 -> []
  | x -> let d = find_prime_divisor x in fst d::factorization (snd d)

    let factorization n =
      let n = ref n in
      let factors = ref [] in
      let p = ref 3 in
      let valence = ref 0 in
      while !n mod 2 = 0 do incr valence ; n := !n/2 done ;
      if !valence <> 0 then factors := [2,!valence] ;
      valence := 0 ;
      while float_of_int !p <= sqrt(float_of_int !n)
      do
  while !n mod !p = 0 do incr valence ; n := !n/(!p) done;
  if !valence <> 0 then factors := (!p,!valence)::!factors;
  valence:=0 ; p:=!p+2
      done;
      List.rev (if !n<>1 then (!n,1)::!factors else !factors)

    let string_of_factorization ?(utf8=false) factors =
      String.concat (" "^(if utf8 then UTF8.cartesian else "*")^" ")
  (
    List.map
      (
        fun (p,o) -> Printf.sprintf "%i%s" p (if o>1 then (UTF8.superscript_int ~utf8 o) else "")
      )
      factors
  )

    let name () = "Algebra.Integer.Standard"

    let default = zero

    let compare = Pervasives.compare

end

module Real =
struct
  type t = float
  let of_string x = float_of_string x
  let zero = 0.0
  let one = 1.0
  let neg x = -.x
  let add x y = x +. y
  let sub x y = x -. y
  let mul x y = x *. y
  let is_prime x = false
  let factorization x = []
  let string_of_factorization ?(utf8=false) fs = ""
  let div x y = x /. y
  let rst x y = 0.0
  let inv x = 1.0/.x
  let name () = "Algebra.Real"
  let default = zero
  let compare = Pervasives.compare

end

module Fraction(R:Sig.FactorialRing) =
struct
  type t = {num:R.t;den:R.t}
  let compare {num=a;den=b} {num=c;den=d} =
    let diff = R.compare a c in
      if diff <> 0 then diff else R.compare b d
  let zero = {num=R.zero;den=R.one}
  let one = {num=R.one;den=R.one}
  let mul {num=a;den=b} {num=c;den=d} = {num=R.mul a c;den=R.mul b d}
  let add {num=a;den=b} {num=c;den=d} = {num=R.add (R.mul a d) (R.mul b c);den=R.mul b d}
  let sub {num=a;den=b} {num=c;den=d} = {num=R.sub (R.mul a d) (R.mul b c);den=R.mul b d}
  let neg {num=a;den=b} = {num=R.neg a;den=b}
  let inv {num=a;den=b} = if a <> R.zero then {num=b;den=a} else raise Zero
  let div {num=a;den=b} {num=c;den=d} = if c <> R.zero then {num=R.mul a d;den=R.mul b c} else raise Zero
  let is_prime x = false
  let default = zero
  let rst x y = zero
  let factorization x = []
  let string_of_factorization ?(utf8=false) fs = ""
  let name () = "Fraction"
  let of_string s = failwith "Algebra.Fraction.of_string : NIY"
end

module Polynomial =
struct

  module ByArray =

  struct

    module Ring(R:Sig.Ring) =
    struct


      type t = R.t array
      let pointwise op poly1 poly2 =
  let degree = ref ((max (Array.length poly1) (Array.length poly2))-1) in
  let ans =  Array.init
    !degree
    (
      fun k ->
        try
    let c1 = poly1.(k)
    in
      (
        try
          let ans = op c1 poly2.(k)
          in
      if ans <> R.zero then (degree:=k;ans) else ans
        with
          | Invalid_argument _ -> c1

      )
        with
    | Invalid_argument _ -> poly2.(k)
    )
  in
    Array.sub ans 0 (succ !degree)

      let of_string s = failwith "Polynomial.Ring.of_string : not implemented yet"
      let string_of s = failwith "Polynomial.Ring.string_of : not implemented yet"
      let zero = [||]
      let one = [|R.one|]
      let normalize poly =
  let last_non_zero =
    let ans = ref ((Array.length poly)-1)
    in
      while poly.(!ans) = R.zero
      do
        ans := !ans-1
      done
      ;
      !ans
  in
    Array.sub poly 0 (succ last_non_zero)
      let degree poly = if poly = zero then raise Zero else Array.length poly
      let neg poly = Array.map (fun coef -> R.neg coef) poly
      let add poly1 poly2 = normalize (pointwise R.add poly1 poly2)
      let sub poly1 poly2 = normalize (pointwise R.sub poly1 poly2)
      let mul poly1 poly2 =
  Array.init
    ((Array.length poly1)+(Array.length poly2)-2)
    (
      fun k ->
        let accu = ref (R.mul poly1.(0) poly2.(k))
        in
    (
      for i=1 to k
      do
        accu := R.add !accu
          (
      try
        (R.mul poly1.(i) poly2.(k-i))
      with
        | Invalid_argument _ -> R.zero
          )
      done
      ;
      !accu
    )
    )


      let monomial ?(coef=R.one) n = Array.init n (fun k -> (if k<>n then R.zero else coef))
      let coefficient n poly = try poly.(n) with Invalid_argument _ -> R.zero
      let name () = ""
      let default = zero
      let compare poly1 poly2 =
  let k = ref 0
  and diff = ref 0
  and d1 = degree poly1
  and d2 = degree poly2
  in
    (
      while ((!k <= d1)||(!k <= d2))&&(!diff = 0)
      do
        let c1 = try poly1.(!k) with | Invalid_argument _ -> R.zero
        and c2 = try poly2.(!k) with | Invalid_argument _ -> R.zero
        in
    (
      diff := R.compare c1 c2 ;
      incr k
    )
      done
      ;
      !diff
    )

      let default = zero

    end (* ByArray.Ring *)

    module FactorialRing(R:Sig.FactorialRing) =
    struct
      include Ring(R)
      let of_string s = failwith "Polynomial.Ring.of_string : not implemented yet"
      let string_of s = failwith "Polynomial.Ring.string_of : not implemented yet"
      let is_prime poly = raise Does_not_know
      let factorization poly = raise Does_not_know
      let string_of_factorization ?(utf8=false) fs = raise Does_not_know
      let name () = ""

    end (* ByArray.FactorialRing *)

  end (* ByArray *)

  module ByMap =

  struct

    module Ring(R:Sig.Ring) =
    struct


      module P = Map.Make
  (
    struct
      type t = int
      let compare a b = a - b
      let default = 0
    end
  )
      type t = R.t P.t
      let of_string s = failwith "Polynomial.Ring.of_string : not implemented yet"
      let zero = P.empty
      let one = P.add 0 R.one P.empty
      let degree poly =
  if poly = zero
  then raise Zero
  else
    P.fold
      (
        fun key r accu ->
    try
      if ((P.find key poly) <> R.zero )&&(accu<key)
      then key
      else accu
    with | Not_found -> accu
      )
      poly
      0
      let pointwise op poly1 poly2 =
  P.fold
    (
      fun k c2 accu ->
        try
    let already_there = P.find k accu
    in
      if already_there <> R.zero
      then
        accu
      else
        P.remove k accu
        with
    | Not_found -> P.add k c2 accu
    )
    poly2
    (
      P.fold
        (
    fun k c1 accu -> P.add k (try op c1 (P.find k poly2) with | Not_found -> c1) accu
        ) poly1 P.empty
    )
      let neg poly = P.map (fun coef -> R.neg coef) poly
      let add poly1 poly2 = pointwise R.add poly1 poly2
      let sub poly1 poly2 = pointwise R.sub poly1 poly2

      let mul poly1 poly2 =
  let convolution k =
    let ans = ref
      (
        R.mul
    (try P.find 0 poly1 with | Not_found -> R.zero)
    (try P.find k poly2 with | Not_found -> R.zero)
      )
    in
      for i=1 to k
      do
        ans :=
    R.add
      !ans
      (
        R.mul
          (try P.find  i    poly1 with | Not_found -> R.zero)
          (try P.find (k-i) poly2 with | Not_found -> R.zero)
      )
      done
      ;
      !ans
  in
  let rec aux k accu =
    if k>((degree poly1)+(degree poly2))
    then accu
    else
      (
        let to_be_added = convolution k
        in
    if to_be_added <> R.zero
    then
      aux (succ k) (P.add k to_be_added accu)
    else
      aux (succ k) accu
      )
  in
    aux 0 zero

      let monomial ?(coef=R.one) n = P.add n coef (P.empty)
      let coefficient n poly = try P.find n poly with | Not_found -> R.zero
      let name () = ""
      let default = zero
      let compare poly1 poly2 =
  let k = ref 0
  and diff = ref 0
  and d1 = degree poly1
  and d2 = degree poly2
  in
    (
      while ((!k <= d1)||(!k <= d2))&&(!diff = 0)
      do
        let c1 = try P.find !k poly1 with | Not_found -> R.zero
        and c2 = try P.find !k poly2 with | Not_found -> R.zero
        in
    (
      diff := R.compare c1 c2 ;
      incr k
    )
      done
      ;
      !diff
    )




    end (* ByMap.Ring *)

    module FactorialRing(R:Sig.FactorialRing) =
    struct
      include Ring(R)
      let is_prime poly = raise Does_not_know
      let factorization poly = raise Does_not_know
      let string_of_factorization ?(utf8=false) fs = raise Does_not_know

    end (* ByMap.FactorialRing *)

  end (* ByMap *)

end (* Polynomial *)

module Matrix(R:Sig.Ring)(D:sig val initial:int end) =
struct

  type s = R.t

  type t = R.t array array

  let size = ref D.initial

  let add m1 m2 =
    Array.init !size (fun y -> (Array.init !size (fun x -> R.add m1.(x).(y) m2.(x).(y))))

  let sub m1 m2 =
    Array.init !size (fun y -> (Array.init !size (fun x -> R.sub m1.(x).(y) m2.(x).(y))))

  let scp s m =
    Array.init !size (fun y -> (Array.init !size (fun x -> R.mul s m.(x).(y))))

  let neg m =
    Array.init !size (fun y -> (Array.init !size (fun x -> R.neg m.(x).(y))))

  let one = Array.init !size (fun y -> (Array.init !size (fun x -> if x=y then R.one else R.zero)))

  let zero = Array.init !size (fun y -> (Array.init !size (fun x -> R.zero)))

  let of_string = failwith "Algebra.Matrix.of_string : not implemented yet."

  let mul m1 m2 =
    let aux x y =
      let ans = ref R.zero
      in
  for i = 0 to !size-1
  do
    ans := R.add !ans (R.mul m1.(x).(i) m2.(i).(y))
  done
  ;
  !ans
    in
      Array.init !size (fun y -> (Array.init !size (fun x -> aux x y)))

  let entry x y m = m.(x).(y)

  let trans m =
    Array.init !size (fun y -> (Array.init !size (fun x -> R.neg m.(y).(x))))

  let name () = "Algebra.Matrix"

  let default = zero

  exception AnotherExit of int

  let compare m1 m2 =
    let diff = ref 0
    in
      try
  (
    for i=0 to (!size-1)
    do
      for j=0 to (!size-1)
      do
        diff := R.compare m1.(i).(j) m2.(i).(j)
        ;
        if !diff <> 0
        then raise (AnotherExit !diff)
        else ()
      done
    done
    ;
    0
  )
      with
  | (AnotherExit diff) -> diff

end

module Category =
struct

  module Linear(R:Sig.Ring) =
  struct

    module Representation(V:Graph.Sig.COMPARABLE) =
    struct
      include (Graph.Persistent.Digraph.ConcreteLabeled(V))(R)
      module Morphism =
      struct

  type t = E.t array

  let src p = E.src (p.(0))

  let dst p = E.dst (p.((Array.length p)-1))

  let check p =
    let i = ref 0 and stop = (Array.length p)-1 and ans = ref true
    in
      while (!i < stop)&&(!ans)
      do
        ans := (E.dst p.(!i)=E.src p.(Pervasives.succ !i)) ;
        incr i
      done
      ;
      !ans

  let compose p =
    try
      let accu = ref (E.label p.(0))
      in
        for i = 1 to Pervasives.pred (Array.length p)
        do
    accu := R.mul !accu (E.label p.(i))
        done
        ;
        !accu
    with
      | Invalid_argument _ -> R.one

  let compare p1 p2 =
    try
      let x1 = p1.(0) and x2 = p2.(0) and i = ref 1
      in
      let cmp_s = ref (V.compare (E.src x1)   (E.src x2)  )
      and cmp_d = ref (V.compare (E.dst x1)   (E.dst x2)  )
      and cmp_e = ref (R.compare (E.label x1) (E.label x2))
      in
        (
    try
      while (!cmp_s = 0) && (!cmp_d = 0) && (!cmp_e = 0)
      do
        let x1 = p1.(!i) and x2 = p2.(!i)
        in
          cmp_s := !cmp_d                              ;
          cmp_d := V.compare (E.dst x1)   (E.dst x2)   ;
          cmp_e := R.compare (E.label x1) (E.label x2) ;
          incr i
      done
    with
      | Invalid_argument _ ->
          cmp_s := (Array.length p1)-(Array.length p2)
        )
        ;
        if !cmp_s <> 0
        then !cmp_s
        else
    if !cmp_d <> 0
    then !cmp_d
    else !cmp_e
    with
      | Invalid_argument _ -> (Array.length p1)-(Array.length p2)

  let lazy_compare p1 p2 = R.compare (compose p1) (compose p2)

      end

    end

  end

end


(*

   Attention, une algèbre de matrices n'est jamais commutative, même
   si l'anneau dans lequel ses coefficients sont pris l'est (à moins
   bien sûr que ce ne soit l'algèbre des matrices 1x1).

*)

(*

  Berlekamp

  Cantor-Zassenhaus

  A coder : l'algorithme de factorisation dans K[X] sachant que K est un
  corps (commutatif).

  Si A est un anneau factoriel, l'anneau des polynômes à une
  indéterminée A[X] l'est aussi. Ceci est essentiellement basé sur le
  fait de plonger A[X] dans K[X] où K est le corps des fractions de A.

  Question : Cet algorithme existe-t-il ? Si oui on sait décomposer
  tout polynôme de Q[X] et donc en particulier décider si polynôme à
  coefficient entiers et une indéterminée admet des racines et
  lesquelles. Cependant cela ne laisse rien supposer du cas où l'on a
  plusieurs indéterminées.

*)

(*

  Algorithme général de factorisation dans l'anneau des polynômes d'un
  anneau factoriel F[X]. On suppose que F est infini et on note K le
  corps des fractions de F. Soit P un polynôme de degré d. Pour
  trouver des facteurs de degré k (où k est inférieur ou égal à d/2)
  on choisit x0,...,xk éléments de F deux à deux distincts et on
  détermine P(x0),...,P(xk). On décompose en facteurs premiers.

*)



(*
  let _ =
  match (*Numerix.Slong.of_string "7446"*) (Numerix.Slong.of_int 7446) with
  | zero -> print_endline "There is a bug."
  | _    -> print_endline ("7446 is not zero.")
*)

(*
  let _ =
  if Numerix.Slong.eq (Numerix.Slong.of_string "7446924528496") Numerix.Slong.zero
  then print_endline "There is a bug."
  else print_endline "7446924528496 is not zero."
*)

(*
  let _ =
  let to_be_decomposed = "24071975"
  in
  (
  print_endline (to_be_decomposed^" = ") ;
  List.iter
  (fun (p,v) -> print_endline ((Numerix.Slong.string_of p)^"^"^(string_of_int v)))
  (Integer.Numerix.decompose (Numerix.Slong.of_string to_be_decomposed))
  )
*)

(*
  let _ = match Numerix.Slong.isprime (Numerix.Slong.of_int 18) with
  | Numerix.True -> print_endline (string_of_bool true)
  | _ -> print_endline (string_of_bool false)
*)

(* ——————————————————————————————————————————————————————————————— *)

(* Factorisation dans un semi treillis gradué libre *)

(*

   Soit un monoïde libre W. On note B(W) l'ensemble des fonctions de W
   dans B nulle (i.e. prenant la valeur "false") sauf sur une
   sous-partie finie de W. La structure Booléenne de B s'étend point à
   point à B(W). Un élément f de B(W) est dit homogène de degré n
   lorsque pour tout élément w de W on a (f(w)=true) => (lgth(w) = n).

   La loi interne de W, c'est-à-dire la concaténation, s'étend
   également à B(W) de sorte que si deux éléments de B(W) sont
   homogènes, alors leur produit l'est aussi.

   On peut ainsi considérer la sous-structure HB(W) en ne retenant que
   les éléments homogènes de B(W). La structure obtenue est graduée.

*)

(*

  Let W be a free monoid (i.e. the monoid of words over an alphabet
  A). Denote by B(W) the collection of finite set of words. There is
  an obvious bijection between the collection of such sets and the
  collection of mappings from W to {true,false} which is false almost
  everywhere. An element f taken from B(W) is said to be homogeneous
  of degree n when for all word w (f(w)=true) => (lgth(w) = n).

  The internal law of W (i.e. concatenation) extends to B(W). If f and
  g are homogeneous of degrees n and m, then their product if still
  homogeneous of degre n+m.

  The substructure HB(W) is obtained by considering all the
  homogeneous elements of B(W).

*)

(* En fait on travaille plutôt avec des produits tensoriels le sup-demi-treillis *)

module Free =

struct

  module Graded =

  struct

    module SemiLattice
      (
      Ord:
      sig
        type t
        val compare: t -> t -> int
        val string_of: t -> string
      end
      )
      =
    struct

      type word = Ord.t array

      let string_of_word w = String.concat "" (Array.to_list (Array.map Ord.string_of w))

      module P = Set.Make(struct type t = word let compare = compare end)

      type polynomial = P.t

      let zero = P.empty

      let add = P.add

      let degree p =
        try Array.length (P.choose p)
        with Not_found -> raise Zero (* in this case we actually have p = zero *)

      let homogeneous p =
        try
          let degree = Array.length (P.choose p) in
            P.for_all (fun w -> (Array.length w)=degree) p
        with Not_found -> true (* in this case we actually have p = zero *)

      let one = P.singleton [||]

      let join = P.union

      let meet = P.inter

      let product p1 p2 =
        let f w1 w2 accu2 = P.add (Array.append w1 w2) accu2 in
        let f w1 accu1 = P.fold (f w1) p2 accu1 in
        P.fold f p1 zero

      (* La valeur de s doit être un ensemble de cardinal length(p) dont les
   éléments sont des entiers inférieurs à length(p)+length(q)-1 *)

      let interlaced_product s p q =
        let f s wp wq accuq = P.add (Soi.interlaced_append s wp wq) accuq in
        let f wp accup = P.fold (f s wp) q accup in
        P.fold f p zero

      (* find_a_factor : try to a rank which can be factorized. May raise Not_found. Otherwise
   it return a triple (poly_before,letter or word,poly_after) *)

      let residu ba w p =
        let f cw accu =
          if BooleanArray.extract ba cw = w
          then P.add (BooleanArray.extract_complement ba cw) accu
          else accu in
        P.fold f p zero

      let does_it_factorize ba p =
        let a_word = P.choose p in
        let reference = residu ba (BooleanArray.extract ba a_word) p in
        let f w accu = P.add (BooleanArray.extract ba w) accu in
        let test word = (residu ba word p = reference) in
        let set_of_words_found_at_s = P.fold f p P.empty in
        (a_word <> [||]) && (P.for_all test set_of_words_found_at_s)

      (* %! ça veut dire "affiche maintenant" *)

      (* factorize returns a list of Boolean arrays *)

      let f counter w accu = P.add (BooleanArray.extract_complement counter w) accu

      let rec factorize ?(accu=[]) ?(lvl=1) ?counter p =
        try (
          let lp = Array.length (P.choose p) in
          let counter = match counter with
            | Some ba -> ba
            | None    -> Array.init lp (fun k -> k < lvl) in
          if does_it_factorize counter p
          then factorize ~accu:(counter :: accu) (P.fold (f counter) p P.empty)
          else
            try
              let counter = BooleanArray.next_on_its_level counter in
              factorize ~accu ~lvl ~counter p
            with Exit ->
              if lvl < lp / 2
              then factorize ~accu ~lvl:(succ lvl) p
              else
                if lp <> 0 || accu = []
                then (Array.make lp true) :: accu
                else accu)
        with Not_found -> accu

      let factorize ?degree ?(unpacked=false) p =
        if p <> zero
        then
          let factors = factorize p in
          if unpacked
          then BooleanArray.unpack (List.rev factors)
          else factors
        else
          match degree with
            | Some d -> print_endline "Warning: factoring zero\n" ; [Array.make d true]
            | None -> raise (Invalid_argument "factorize [Algebra.Free.SemiLattice]")

      let string_of p = String.concat "\n" (List.map string_of_word (P.elements p))

      let make wl = List.fold_left (fun accu w -> add w accu) zero wl

    end (* Semilattice *)

  end (* Graded *)

end (* Free *)

module Monoid =
struct
  module Philosopher =
  struct

  (* Implémentation du monoïde des mots sur N où deux lettres a et b
     commutent dès que |b-a|>1 *)

    let string_of word = String.concat "." (List.map (fun letter -> string_of_int letter) (Array.to_list word))

    let string_of_class low = String.concat " | " (List.map string_of low)

    let max_letter word = Array.fold_left (fun accu x -> max accu x) 0 word

    let normalize w = ()

    let number_of_maximal_paths_up_to_dihomotopy n = ()

    let exchange i j word =
      Array.init
        (Array.length word)
        (
          fun k ->
            if k = i
            then word.(j)
            else
              if k = j
              then word.(i)
              else word.(k)
        )

    let exchange i j word =
      let answer = Array.copy word in
      let () = answer.(i) <- word.(j) in
      let () = answer.(j) <- word.(i) in
      answer

    let counter = ref 0

(* bound is the number of philosophers minus one *)

    let equivalence_class ?bound word =
      let answer = ref []
      and current_wave = ref [word]
      and next_wave = ref []
      and len = Array.length word
      and bound = match bound with
  | Some x -> x
  | None -> max_letter word
      in
      let one_step_permutation wordbis =
  let answer_bis = ref []
  in
    for i=0 to len-2
    do
      if let aux = abs(wordbis.(i)-wordbis.(succ i)) in ((aux > 1)&&(aux<>bound))
      then
        (
    let aux = exchange i (succ i) wordbis
    in
      if (not(List.mem aux !answer)) && (not(List.mem aux !next_wave))
      then
        (
          (if false then print_endline ("  added "^(string_of aux)))
          ;
          answer_bis := aux::!answer_bis
        )
      else
        (if false then print_endline ("  "^(string_of aux)^" is already taken"))
        )
      else
        (if false then Printf.printf "  %i and %i does not commute\n" wordbis.(i) wordbis.(succ i))
    done
    ;
    !answer_bis
      in
  while !current_wave <> []
  do
    answer := (!answer)@(!current_wave)
    ;
    (
      if false then
        (
    incr counter
    ;
    print_endline ("\nWave "^(string_of_int !counter)^"\n")
    ;
    print_endline ("Current wave "^(string_of_class !current_wave))
        )
    )
    ;
    (if false then print_endline ("Answer "^(string_of_class !answer)))
    ;
    next_wave := []
    ;
    List.iter
      (
        fun w ->
    (
      (if false then print_endline ("Treating "^(string_of w)))
      ;
      next_wave := (one_step_permutation w)@(!next_wave)
    )
      ) !current_wave
    ;
    current_wave := !next_wave
  done
  ;
  !answer

    let circular_action_on_word ?bound word =
      let n = (Array.length word)
      and bound = match bound with
  | Some x -> succ x
  | None -> (max_letter word)+1
      in
  Array.init n (fun k -> (succ word.(k)) mod bound)

    (* Two equivalence classes are the same iff they share one element *)

    let same_classes cl cl' = List.exists (fun x -> (List.exists (fun x' -> (x = x')) cl')) cl

    (* May raise the exception failure "hd" if the empty list is given as an argument *)

    (* The argument bound is the number of philosphers minus one.  *)

    let circular_action_on_class ?bound eqcl =
      let bound = match bound with
  | Some x -> x
  | None -> List.fold_left (fun accu letter -> max letter accu) 0 (List.map max_letter eqcl)
      and current = ref eqcl
      and answer = ref []
      in
  for i = 0 to bound
  do
    begin
      if List.for_all (fun eqcl' -> not(same_classes !current eqcl')) !answer
      then
        answer := !current::!answer
      else
        ()
    end
    ;
    current := List.map (fun w -> circular_action_on_word ~bound:bound  w) !current
  done
  ;
  !answer

    (* increasing_word : Ok *)

    let increasing_word first_letter length = Array.init length (fun k -> first_letter+k)

(* is_strictly_decreasing : Ok *)

    let is_strictly_decreasing ?(from=0) word =
      let n = (Array.length word)-1 and pos = ref from
      in
  while (!pos < n) && (word.(!pos) > word.(succ !pos))
  do
    incr pos
  done
  ;
  !pos>=n

    let to_be_exchanged word =
      let pos = ref ((Array.length word)-1)
      in
  while (!pos>=0)&&(is_strictly_decreasing ~from:!pos word)
  do
    decr pos
  done
  ;
  !pos

    let least_letter_position ?(from=0) ?(lowbound=0) word =
      let answer = ref from
      and current_min = ref word.(from)
      and n = (Array.length word)-1
      in
  for i = from to n
  do
    let x = word.(i)
    in
      if (lowbound < x) && (x < !current_min)
      then
        (answer:=i ; current_min := x)
      else
        ()
  done
  ;
  !answer

(* next_anagram : Ok *)

  let next_anagram word =
    let n = to_be_exchanged word
    and len = Array.length word
    in
      if n >= 0
      then
  (
    let swap = least_letter_position ~from:(n+1) ~lowbound:(word.(n)) word
    in
    let x = word.(swap)
    in
    let aux = Array.sub word (n+1) (len-(n+1))
    in
    let _ = (aux.(swap-(n+1)) <- word.(n)) ; Array.sort Pervasives.compare aux
    in
      Array.concat [(Array.sub word 0 n);[|x|];aux]
  )
      else
  raise Exit

  let traces size =
    let word = ref (increasing_word 0 (size))
    and answer = ref []
    in
      begin
  try
    while true
    do
      (
        if List.for_all (fun eqcl -> not(List.mem !word eqcl)) (!answer)
        then
    (
      let aux = (equivalence_class !word)
      in
        (* print_endline (string_of_class aux); *)
        answer := aux::(!answer)
    )
        else
    ()
      )
      ;
      word := (next_anagram !word)
    done
  with
    | Exit -> ()
      end
      ;
      !answer

  let orbits size =
    let word = ref (increasing_word 0 (size)) in
    let answer = ref [] in
    begin
      try
        while true do
          if List.for_all (fun orbit -> List.for_all (fun eqcl -> not(List.mem !word eqcl)) orbit) (!answer)
          then (
            let aux = circular_action_on_class (equivalence_class !word) in
            (* print_endline (string_of_class aux); *)
            answer := aux::(!answer));
          word := (next_anagram !word)
        done
      with Exit -> ()
    end;
    !answer

  end (* Philosopher *)

end (* Monoid *)

module SequencesOfIntegers =
struct

let first_list ?(start=0) k =
  let k = ref (k + start) in
  let answer = ref [] in
  while !k > start do
  decr k; answer := !k :: !answer
  done; !answer

let next_list l n =
  let long = List.length l in
  let rec aux l n long = match l with
    | [] -> raise Not_found
    | x::s ->
      if x <= n - long
      then
        try x :: (aux s n (long - 1))
        with Not_found -> (x + 1) :: (first_list ~start:(x + 2) (long - 1))
      else raise Not_found in
  aux l n long

let fold_list ?(start=0) f d (*length or dimension*) n (*upper bound*) a (*accumulator*) =
  if d <> 0 && n < start + (pred d)
  then a
  else
    let l = ref (first_list ~start d) in
    let a = ref a in
    let keep_on = ref true in
    let () =
      while !keep_on do
        a := f !l !a ; try l := next_list !l n with Not_found -> keep_on := false
      done in
    !a

let iter_list ?(start=0) f k (*length*) n (*upper bound*) =
  if k = 0 || n >= start + (pred k)
  then
    let l = ref (first_list ~start k) in
    let keep_on = ref true in
      while !keep_on do
        f !l ; try l := next_list !l n with Not_found -> keep_on := false
      done

let rec weight_up_to n aaa = if n < 0 then 0 else (weight_up_to (n - 1) aaa) + aaa.(n)

let rec weight aaa = weight_up_to (pred (Array.length aaa)) aaa

let first_nonzero size int_seq pos =
(*
  let n = Array.length int_seq in
*)
  let rec first_nonzero current =
    if current < size && int_seq.(current) = 0
    then first_nonzero (current + 1)
    else current (*if current < n then current else size*) in
  (*try*) first_nonzero pos
  (*with Invalid_argument "index out of bounds" -> size*)

(*
let first_array_with_highest_weight g l w a =
  let w = ref w in
  Array.init l
    (fun n -> let x = min ( max 0 !w ) (g.(n) + a.(n)) in let () = w := !w - x in x)

let first_array length weight' aaa =
  let accu = ref 0 in
  let answer = Array.init length
    (fun n ->
      let x = min (max 0 (weight' - (weight_up_to (n - 1) aaa))) aaa.(n) in
      (accu := !accu + x ; x)) in
  if !accu = weight' then answer else raise Not_found
*)

(* displaying internals *)

let print_array a = print_string "[| " ; Array.iter (fun x -> Printf.printf "%i " x) a ; print_endline "|]"

(* end of displaying internals *)

let first_array ?g l w a =
  let min_load = ref (match g with
    | Some g -> Array.fold_left (fun accu x -> accu + x) 0 g
    | None -> 0) in
  let max_load = Array.fold_left (fun accu x -> accu + x) !min_load a in
  let w = ref w in
  let g = match g with
    | Some g -> g
    | None -> Array.make l 0 in
  let () = if !min_load > !w || max_load < !w then raise Not_found in
  let f i =
    let () = min_load := !min_load - g.(i) in
    let return = min (g.(i) + a.(i)) (!w - !min_load) in
(*
    let () = Printf.printf "w = %i min_load = %i return = %i\n" !w !min_load return in
*)
    let () = w := !w - return in
    return in
  Array.init l f

(*Some dummy tests

let g = [|3;2;1|]

let l = 3

let w = 11

let a = [|2;0;3|]

let () =
  (try print_array (first_array ~g l 5 a) with Not_found -> print_endline "Does not exist") ;
  (try print_array (first_array ~g l 6 a) with Not_found -> print_endline "Does not exist") ;
  (try print_array (first_array ~g l 7 a) with Not_found -> print_endline "Does not exist") ;
  (try print_array (first_array ~g l 8 a) with Not_found -> print_endline "Does not exist") ;
  (try print_array (first_array ~g l 9 a) with Not_found -> print_endline "Does not exist") ;
  (try print_array (first_array ~g l 10 a) with Not_found -> print_endline "Does not exist") ;
  (try print_array (first_array ~g l 11 a) with Not_found -> print_endline "Does not exist") ;
  (try print_array (first_array ~g l 12 a) with Not_found -> print_endline "Does not exist")

End of dummy tests*)

let next_array length int_seq aaa =
  let fafs = (*first available free space*)
    let n = ref 0 in
    let only_met_zero = ref true in
    let () = while
      (try int_seq.(!n) >= aaa.(!n) || !only_met_zero
        with Invalid_argument _ -> false) do
      incr n; only_met_zero := !only_met_zero && int_seq.(!n - 1) = 0
    done in
    !n in
  let () = if fafs = length then raise Not_found in
  let remaining_level = (*mass to be allocated*)
    let accu = ref 0 in
    let () = for n = 0 to fafs - 1
      do accu := !accu + int_seq.(n) done in
    !accu - 1 in
  (*One unit has to be picked from the mass contained before the first available space and send to "the level
  above"*)
  let answer = first_array length remaining_level aaa in
  let () = answer.(fafs) <- int_seq.(fafs) + 1 in (*if fafs=size, then an exception is raised*)
  let () = for n = fafs + 1 to length - 1
  do answer.(n) <- int_seq.(n) done in
  answer

let next_array ?g length int_seq aaa =
  let g = match g with
    | Some g -> g
    | None -> Array.make length 0 in
  let fafs = (*first available free space*)
    let n = ref 0 in
    let only_met_ground = ref true in
    let () = while
      ((*try*) !n < length && (int_seq.(!n) >= aaa.(!n) + g.(!n) || !only_met_ground)
        (*with Invalid_argument "index out of bounds" -> false*)) do
      incr n; only_met_ground := !only_met_ground && int_seq.(pred !n) = g.(pred !n)
    done in
    !n in
  let () = if fafs = length then raise Not_found in
  let remaining_level = (*mass to be allocated*)
    let accu = ref 0 in
    let () = for n = 0 to pred fafs
      do accu := !accu + int_seq.(n) done in
    pred !accu in
  (*One unit has to be picked from the mass contained before the first available space and send to "the level
  above"*)
  let answer = first_array ~g:(Array.sub g 0 fafs) fafs remaining_level aaa in
  let answer = Array.append answer (Array.sub int_seq fafs (length - fafs)) in
  answer.(fafs) <- succ answer.(fafs) ;
  answer

(* a dummy test

let () =
  let aaa = [|0;1;2;3;4|] in
  let g =   [|1;1;1;1;3|] in
  let weight' = 8 in
  let length = 5 in
  let fa = ref [first_array ~g length weight' aaa] in
  let () = print_string "first = " ; print_array (List.hd !fa) in
  try (
  while true do
  fa := next_array ~g length (List.hd !fa) aaa :: !fa ;
  (*print_array !fa*) done )
  with Not_found -> print_endline "Over" ;
  fa := List.sort (fun x y -> Pervasives.compare y x) !fa ;
  List.iter print_array !fa ; print_endline "coucou"

end of the dummy test *)

let fold_array ?g f w a accu =
  let l = Array.length a in
(*
  let g' = match g with
    | Some g -> g
    | None -> Array.make l 0 in
*)
  try
    let current = ref (first_array ?g l w a) in
    let accu' = ref accu in
    (
      try
        while true do
          (*print_string "current = " ; print_array !current ;*)
          accu' := f !current !accu' ;
          current := next_array ?g l !current a
        done ;
        !accu'
      with Not_found -> !accu'
    )
  with Not_found -> accu

(* a dummy test

let () =
  let a = [|0;1;2;3;4|] in
  let g =   [|1;1;1;1;3|] in
  let w = 7 in
  let count = fold_array ~g (fun _ a -> succ a) w a 0 in
  if count = 0
  then print_endline "There is no sequence"
  else
    if count = 1
    then print_endline "There is one sequence"
    else Printf.printf "There are %i sequences\n" count

 end of the dummy test *)

let iter_array ?g f w a =
  let l = Array.length a in
  (*let g' = match g with
    | Some g -> g
    | None -> Array.make l 0 in*)
  try
    let current = ref (first_array ?g l w a) in
    (
      try
        while true do
          let () = f !current in
          current := next_array ?g l !current a
        done
      with Not_found -> ()
    )
  with Not_found -> ()

(* a dummy test

let () =
  let a = [|0;1;2;3;4|] in
  let g =   [|1;1;1;1;3|] in
  let w = 8 in
  iter_array ~g print_array w a

end of the dummy test *)

end(*SequencesOfIntegers*)


module PCombinator =
struct

open SequencesOfIntegers

(*Assume A is an array of length ℓ whose kth-term A_k belongs to some boolean algebra 𝔹_k. For each subset
S⊆{0,…,ℓ-1} of cardinal n, we denote by α_S the ℓ-fold product C_0×⋅⋅⋅×C_(ℓ-1) where each C_k is A_k if k∈S and
(full A_k) otherwise, (full A_k) denoting the greatest element of the boolean algebra A_k belongs to. The function
generalized_product then return the union –least upper bound– of the family A_S for S⊆{0,…,ℓ-1} of cardinal n*)

let empty zero product_fun aaa =
  let n = Array.length aaa in
  product_fun (fun k -> if k < n then aaa.(k).(0) else zero) 0 n

let levelwise_product ~zero ~union ~product_fun ?g level aaa =
  let g = match g with
    | Some g -> g
    | None -> Array.map (fun _ -> 0) aaa in
  let b = pred (Array.length aaa) in
  let aaa' = Array.(map (fun aa -> pred (length aa) ) aaa) in
  fold_array
    ~g
    (fun current accu -> union (product_fun (fun n -> aaa.(n).(current.(n) - g.(n))) 0 b) accu)
    level
    aaa'
    (empty zero product_fun aaa)

let generalized_product ~zero ~full ~union ~product_fun n aa =
  Array.(levelwise_product ~zero ~union ~product_fun n (map (fun x -> [|full x ; x|]) aa))

let locus_higher_than ~zero ~full ~union ~product_fun ?(filtration=false) ?g level functions =
  let g = match g with
    | Some g -> g
    | None -> Array.(make (length functions) 0) in
  let aaa =
    if filtration
    then functions
    else
      let f line =
        for i = (Array.length line) - 2 downto 0 do
        line.(i) <- union line.(i) line.(succ i)
        done in
      let aux = Array.copy functions in
      let () = Array.iter f aux in
      aux in
  if level > weight g
  then levelwise_product ~zero ~union ~product_fun ~g level aaa
  else product_fun (fun k -> full aaa.(k).(0)) 0 (pred (Array.length aaa))

let locus_lower_than ~zero ~full ~union ~product_fun ?(chain=false) ?g level functions =
  let g = match g with
    | Some g -> g
    | None -> Array.(make (length functions) 0) in
  let aaa =
    if chain
    then functions
    else
      let f line =
        for i = 0 to (Array.length line) - 2 do
        line.(i) <- union line.(i) line.(succ i)
        done in
      let aux = Array.copy functions in
      let () = Array.iter f aux in
      aux in
  if level < weight g + Array.(fold_left (fun accu x -> accu + (pred (length x))) 0 aaa)
  then levelwise_product ~zero ~union ~product_fun ~g level aaa
  else product_fun (fun k -> full aaa.(k).(0)) 0 (pred (Array.length aaa))

end (*PCombinator*)

module Combinator
(X:
sig
  type t
  val zero: t
  val full: t -> t
  val union: t -> t -> t
  val product: t -> t -> t
  val product_list: t list -> t
  val product_array: t array -> t
  val product_fun: (int -> t) -> int -> int -> t
  val normalize: t -> t
  val string_of: t -> string
end) =
struct

open SequencesOfIntegers

let empty aaa = Array.fold_left (fun accu x -> X.product accu x.(0)) X.zero aaa

(*Assume A is an array of length ℓ whose kth-term A_k belongs to some boolean algebra 𝔹_k. For each subset
S⊆{0,…,ℓ-1} of cardinal n, we denote by α_S the ℓ-fold product C_0×⋅⋅⋅×C_(ℓ-1) where each C_k is A_k if k∈S and
(full A_k) otherwise, (full A_k) denoting the greatest element of the boolean algebra A_k belongs to. The function
generalized_product then return the union –least upper bound– of the family A_S for S⊆{0,…,ℓ-1} of cardinal n*)

let levelwise_product ?g level aaa =
  let g = match g with
    | Some g -> g
    | None -> Array.map (fun _ -> 0) aaa in
  let b = pred (Array.length aaa) in
  let aaa' = Array.(map (fun aa -> pred (length aa) ) aaa) in
  fold_array
    ~g
    (fun current accu -> X.(union (product_fun (fun n -> aaa.(n).(current.(n) - g.(n))) 0 b) accu))
    level
    aaa'
    (empty aaa)

let generalized_product n aa =
  Array.(levelwise_product n (map (fun x -> [|X.full x ; x|]) aa))

let locus_higher_than ?(filtration=false) ?g level functions =
  let g = match g with
    | Some g -> g
    | None -> Array.(make (length functions) 0) in
  let aaa =
    if filtration
    then functions
    else
      let f line =
        for i = (Array.length line) - 2 downto 0 do
        line.(i) <- X.union line.(i) line.(succ i)
        done in
      let aux = Array.copy functions in
      let () = Array.iter f aux in
      aux in
  if level > weight g
  then levelwise_product ~g level aaa
  else X.product_array Array.(init (length aaa) (fun k -> X.full aaa.(k).(0)))

let locus_lower_than ?(chain=false) ?g level functions =
  let g = match g with
    | Some g -> g
    | None -> Array.(make (length functions) 0) in
  let aaa =
    if chain
    then functions
    else
      let f line =
        for i = 0 to (Array.length line) - 2 do
        line.(i) <- X.union line.(i) line.(succ i)
        done in
      let aux = Array.copy functions in
      let () = Array.iter f aux in
      aux in
  if level < weight g + Array.(fold_left (fun accu x -> accu + (pred (length x))) 0 aaa)
  then levelwise_product ~g level aaa
  else X.product_array Array.(init (length aaa) (fun k -> X.full aaa.(k).(0)))

end (*Combinator*)
