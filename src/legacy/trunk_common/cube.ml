(** Graded Algebra of Cubes. The slice of rank n is the (complete)
    lattice of subcubes of the n-dimensional positive cube (i.e. the n-fold product of copies of the positive 
    halfline) n being taken in the set of nonnegative integers. The elements of the slice of rank n are the n-fold 
    products of intervals i.e. the n-cubes. *)

module type In =
sig
  module IntSet:
    sig 
      include Set.S
      val segment_initial: ?start:t -> int -> t
    end with type elt = int
  module Array:
    sig
      val cartesian_next_array_consumer: int array -> int -> unit
    end
end with type IntSet.elt = int

module type S =
sig
  type interval
  type point 
  type underlying
  type regular_value
  type ints
  type t
  exception Base2D of ((t list)*(t list)) 
  exception Undefined
  val of_interval: interval -> t
  val compare_points: point -> point -> int
  val above: int -> regular_value -> t -> t
  val affine_dimension: t -> int
  val after: int -> t -> t 
  val atom: point -> t
  val are_cofinal: t -> t -> bool
  val are_coinitial: t -> t -> bool
  val back: int -> t -> t
  val back_generalized: bool array -> t -> t
  val base_coordinates: t -> ints (*Requirements.Soi.t*)
  val base_dimension: t -> int
  val below: int -> regular_value -> t -> t
  val before: int -> t -> t 
  val belongs_to: point -> t -> bool 
  val belongs_to_boundary: point -> t -> bool 
  val bounded: bool -> bool -> regular_value -> regular_value -> t 
  val cast_shadow_after: int -> t -> t
  val cast_shadow_before: int -> t -> t
  val closure: t -> t 
  val compare: t -> t -> int
  val compare_interval: interval -> interval -> int
  val complement: t -> (t list)
  val cylinder: int -> ((int*interval) array) -> t
  val dimension: t -> int
  val downward: ?ai:(int array) -> t -> t 
  val degenerate_coordinates: t -> ints (*Common.Soi.t*)
  val disjoint: t -> t -> bool
  val during: int -> t -> t
  val empty: int -> t
  val face: bool -> int -> t -> t 
  val front: int -> t -> t 
  val front_generalized: bool array -> t -> t
  val full: int -> t 
  val ginzu_complement: t -> (t list) 
  val cset2D_complement: t -> (t list)
  val glb: t -> point
  val greatest_regular_value: t -> regular_value 
  val in_the_future_of: ?it:bool -> t -> t -> t
  val in_the_past_of: ?it:bool -> t -> t -> t
  val in_touch: t -> t -> bool
  val initial: bool -> regular_value -> t
  val interior: t -> t
  val is_included: t -> t -> bool
  val is_empty: t -> bool 
  val is_not_empty: t -> bool 
  val is_full: t -> bool 
  val is_followed_by: bool array -> t -> t -> bool
  val is_not_full: t -> bool 
  val join: t -> t -> t  
  val lub: t -> point 
  val meet: t -> t -> t 
  val non_degenerate_coordinates: t -> ints (*Common.Soi.t*)
  val not_disjoint: t -> t -> bool
  val normalize: t -> t 
  val origin: int -> point 
  val of_list: (interval list) -> t
  val of_array: (interval array) -> t
  val to_list: t -> interval list
  val to_array: t -> interval array
  val product: t -> t -> t 
  val projection: (*Common.Soi.t*) ints -> t -> t
  val slash: int -> regular_value -> t -> t
  val slice: int -> t -> interval
  val strictly_above: int -> regular_value -> t -> t
  val strictly_below: int -> regular_value -> t -> t
  val string_of: t -> string
  val string_of_interval: interval -> string
  val terminal: bool -> regular_value -> t
  val underlying: t -> underlying
  val upward: ?ai:(int array) -> t -> t
end

module Make(Overall:In)(I:Interval.S):
(S with type regular_value = I.regular_value
  and type point = I.regular_value array
  and type interval = I.t
  and type ints = Overall.IntSet.t)
=
struct

  exception Undefined

  exception Empty_interval

  exception Exit_compare of int

  type ints = Overall.IntSet.t

  type regular_value = I.regular_value

  type point = regular_value array

  type underlying = I.underlying array      

  let origin d = Array.make d (I.least_regular_value)

  let compare_points p1 p2 =
    let n = Array.length p1 in
    let () = assert (n = Array.length p2) in
    let delta = ref 0 in
    let k = ref 0 in
    while !delta = 0 && !k < n do
      delta := I.compare_regular_values p1.(!k) p2.(!k);
      incr k
    done;
    !delta

  type interval = I.t

  let compare_interval = I.compare

  let string_of_interval i = I.string_of i

  type t = 
    | Cb of (interval array) 
    | Ec of int

  let of_interval it = 
    if I.is_not_empty it
    then Cb [|it|]
    else Ec 1

  let to_array c = match c with 
    | Cb(ari) -> ari
    | Ec(_)   -> raise Undefined

  let to_list c = 
    match c with 
      | Cb(ari) -> Array.fold_right (fun i accu -> i::accu) ari []
      | Ec(_)   -> raise Undefined
  
  (* Cb([||]) représente l'élément neutre du produit cartésien dans le
     trellis "gradué" des cubes. *)

  exception Base2D of ((t list)*(t list)) 

  let equal c1 c2 = (c1=c2)

  let dimension c = match c with 
    | Cb(ari) -> Array.length ari
    | Ec(n)   -> n

  let underlying c = Array.make (dimension c) (I.underlying I.empty)

  let affine_dimension c = match c with 
    | Cb(ari) -> 
  (
    Array.fold_left 
      (
        fun accu it -> if (I.is_atomic it) then accu else (accu+1)
      ) 
      1 
      ari
  )
    | Ec(n)   -> 0 

  let test_dimension c1 c2 str = 
    let d1 = dimension c1 in
    let d2 = dimension c2 in
      if d1 = d2 
      then d1
      else failwith ("The arguments of "^str^" are heterogeneous.") 

  let empty n = Ec(n)
  
  let full n = Cb(Array.make n I.full)

  let normalize c =
    match c with
      | Cb(ari) -> (
        let n = Array.length ari in
        try 
          for i = 0 to n - 1 do
            let current = I.normalize (ari.(i)) in
            if I.is_empty current then raise Exit;
            ari.(i) <- current
          done;
          c
        with Exit -> Ec n)
      | _ -> c

  let not_disjoint c1 c2 = match c1,c2 with
    | Cb(ari1),Cb(ari2) -> 
      (try
         let n = ref 0 in
         let () = while I.not_disjoint ari1.(!n) ari2.(!n) do incr n done in
         false
       with _ -> true)
    | _ -> false

  let disjoint c1 c2 = match c1,c2 with
    | Cb(ari1),Cb(ari2) -> 
      (try
         let n = ref 0 in
         let () = while I.not_disjoint ari1.(!n) ari2.(!n) do incr n done in
         true
       with _ -> false)
    | _ -> true

  let degenerate_coordinates c =
    match (normalize c) with 
      | Cb(ari) -> 
    (
      let answer = ref Overall.IntSet.empty
      in
        Array.iteri
    (fun k i -> (if (I.is_atomic i) then (answer := Overall.IntSet.add k !answer) else ())) 
    ari
        ;
        !answer
    )
      | Ec(_) -> raise Undefined

(*
  30/05/2017 : suppression de la normalisation des arguments dans la fonction 
  compare. Les arguments doivent donc être normalisés.
*)

  let compare c1 c2 =
    match (c1,c2) with 
      | (Ec(d),Ec(d'))  -> d - d'      
      | (Ec(d),Cb(ara)) -> 
        let d' = Array.length ara in
        if (d > d') then d else -(d' + 1) (* On ajoute 1 pour le cas où l'on est en dimension 0. *)
      | (Cb(ara),Ec(d)) -> 
        let d' = Array.length ara in
        if d > d' then -d else d' + 1 (* On ajoute 1 pour le cas où l'on est en dimension 0. *)
      | (Cb(ara1),Cb(ara2)) ->
        let k_max = pred (min (Array.length ara1) (Array.length ara2)) in
        let difference = ref 0 in (
        try
          for k = 0 to k_max do
            difference := I.compare ara1.(k) ara2.(k);
            if !difference <> 0 then raise Exit
          done;
          Array.length ara1 - Array.length ara2
        with Exit -> !difference
        )

  let string_of c =
    match c with 
      | Cb(ai) -> (
        if ai <> [||] then
          let first = I.string_of ai.(0) in 
          Array.fold_left (fun s i -> s^"*"^(I.string_of i)) first (Array.sub ai 1 ((Array.length ai)-1))
        else "Zero dimension non empty cube" (* full cube of dimension 0 *))
      | Ec(n)  -> "@("^(string_of_int n)^")"

  let of_list il = 
    let rec aux il' answer = 
      match il' with 
  | i::il'' ->
      (
        if (i <> I.empty)
        then aux il'' (Array.append answer [|i|]) (* la concaténation doit de faire dans cet ordre *)
        else Ec(List.length il)
      )
  | [] -> 
      Cb(answer) 
    in
      aux (List.map I.normalize il) [||]

(* La fonction of_array n'a pas été testée *)

  let of_array ar =
    let pos = ref 0 in 
    try
      while ar.(!pos) <> I.empty
      do incr pos done;
      Ec(Array.length ar)
    with
      | _ -> Cb(ar)

  let atom p = Cb(Array.map (I.atom) p)
    

  let initial f b = of_list [I.initial f b]
    

  let terminal f b = of_list [I.terminal f b]
    

  let bounded lf rf lb rb = of_list [I.bounded lf rf lb rb]
    

  let glb c =
    let d = dimension c in
      match c with   
        | Cb(ari) ->
            (
            let p = Array.make d I.least_regular_value in
              try
                let () =
                  for k = 0 to (d - 1) do 
                    p.(k) <- I.glb ari.(k) done in
                p
              with I.Undefined -> raise Undefined
            )
        | _ -> raise Undefined

  let lub c =
    let d = dimension c in
    match c with   
      | Cb(ari) ->
          (
          let p = Array.make d I.least_regular_value in
            try
              let () = 
                for k = 0 to d - 1 do 
                p.(k) <- I.lub ari.(k) done in
              p
            with I.Undefined -> raise Undefined (* Array.make d I.min_regular_value *)
          )
      | Ec(n) -> Array.make n I.least_regular_value

  (* safe (and slow) version *)

  let is_not_empty_version_1 c = ((normalize c) <> Ec(dimension c))

  let is_empty_version_1 c = ((normalize c) = Ec(dimension c))

  (* quick (and unsafe) version *)

  let is_not_empty_version_2 c = 
    match c with 
      | Ec(_) -> false 
      | _     -> true

  let is_empty_version_2 c = 
    match c with 
      | Ec(_) -> true
      | _     -> false

  let is_not_empty = is_not_empty_version_2

  let is_empty = is_empty_version_2

  let is_full c = match c with 
    | Ec(_)   -> false
    | Cb(ari) -> Array.fold_left (fun accu i -> (accu && (i = I.full))) true ari

  let is_not_full c = match c with 
    | Ec(_)   -> true
    | Cb(ari) -> Array.fold_left (fun accu i -> (accu || (i <> I.full))) false ari 


  (* Requires OCaml 4.03 of higher *)

  let is_included c1 c2 = 
    let _ = test_dimension c1 c2 "(is included)" in
      match c1,c2 with 
        | Cb(ari1),Cb(ari2) -> (
          try 
            Array.iter2 (fun it1 it2 -> if not (I.is_included it1 it2) then raise Exit) ari1 ari2;
            true
          with Exit -> false)
        | Ec(_),_ -> true
        | _ -> false


  let meet c1 c2 = 
    let d = test_dimension c1 c2 "(meet)" in 
      match (c1,c2) with 
        | (Cb(ari1),Cb(ari2)) -> (
          try Cb(Array.init d (
            fun k -> 
              let it = I.meet ari1.(k) ari2.(k) in 
              if I.is_not_empty it then it else raise Exit))
          with Exit -> Ec d)
        | _ -> Ec(d)
    
      
  let join c1 c2 =
    let d = test_dimension c1 c2 "(join)" in 
    match (c1,c2) with 
      | Cb(ari1),Cb(ari2) -> 
          Cb(Array.init d (fun k -> I.join ari1.(k) ari2.(k)))
      | Ec(_),z 
      | z,Ec(_) -> z

  (* Quelle est la définition mathématique de la fonction face, selon
     ce que l'on a choisi, les faces de l'ensemble vide peuvent-être
     définies ou non. *)

  let face e k c =
    (* Attention, il faut faire une copie de ari avant de le modifier *)
    match (normalize c) with 
      | Cb(ari) -> 
    (
      let ari' = Array.copy ari in
        try
    if e  
    then
      ((ari'.(k) <- I.atom (I.lub ari.(k))) ; Cb(ari'))
    else 
      ((ari'.(k) <- I.atom (I.glb ari.(k))) ; Cb(ari'))
        with
    | I.Undefined -> Ec(dimension c) (* raise Undefined *)
    )
      | Ec(n) -> Ec(n) (* raise Undefined *)

  let front k c = face false k c and back k c = face true k c

  let front_generalized ba c = match c with
    | Cb(ari) -> 
  begin
    let ari' = Array.copy ari in
      try
        Array.iteri (fun i b -> if b then ari'.(i) <- I.atom (I.lub ari.(i)) else ()) ba ;
        Cb(ari')
      with
        | I.Undefined -> Ec(dimension c)
  end
    | Ec(n) -> Ec(n)

  let back_generalized ba c = match c with
    | Cb(ari) -> 
  begin
    let ari' = Array.copy ari in
      try
        Array.iteri (fun i b -> if b then ari'.(i) <- I.atom (I.glb ari.(i)) else ()) ba ;
        Cb(ari')
      with
        | I.Undefined -> failwith "Cube.generalized_back: the cube given in argument was not a normal form."
  end
    | Ec(n) -> Ec(n)

  let is_followed_by ba c1 c2 = (front_generalized ba c1 = back_generalized ba c2)

  let above k x c =
    match (normalize c) with 
      | Cb(ari) -> 
    (
      let ari' = Array.copy ari in
      let it = I.above x ari.(k) in
        if I.is_not_empty it 
        then 
    ((ari'.(k) <- it) ; Cb(ari'))
        else
    Ec(dimension c)
    )
      | Ec(n) -> Ec(n)

  let below k x c =
    match (normalize c) with 
      | Cb(ari) -> 
    (
      let ari' = Array.copy ari in
      let it = I.below x ari.(k) in
        if I.is_not_empty it 
        then 
    ((ari'.(k) <- it) ; Cb(ari'))
        else
    Ec(dimension c)
    )
      | Ec(n) -> Ec(n)

  let strictly_above k x c =
    match (normalize c) with 
      | Cb(ari) -> 
    (
      let ari' = Array.copy ari in
      let it = I.strictly_above x ari.(k) in
        if I.is_not_empty it 
        then 
    ((ari'.(k) <- it) ; Cb(ari'))
        else
    Ec(dimension c)
    )
      | Ec(n) -> Ec(n)

(* has not been tested *)

  let cast_shadow_before k c =
    match (normalize c) with 
      | Cb(ari) -> 
    (
      let ari' = Array.copy ari in
      let it = I.before ari.(k) in
        if I.is_not_empty it 
        then 
    ((ari'.(k) <- it) ; Cb(ari'))
        else
    Ec(dimension c)
    )
      | Ec(n) -> Ec(n)

(* has not been tested *)

  let cast_shadow_after k c =
    match (normalize c) with 
      | Cb(ari) -> 
    (
      let ari' = Array.copy ari in
      let it = I.after ari.(k) in
        if I.is_not_empty it 
        then 
    ((ari'.(k) <- it) ; Cb(ari'))
        else
    Ec(dimension c)
    )
      | Ec(n) -> Ec(n)

  let strictly_below k x c =
    match (normalize c) with 
      | Cb(ari) -> 
    (
      let ari' = Array.copy ari in
      let it = I.strictly_below x ari.(k) in
        if I.is_not_empty it 
        then 
    ((ari'.(k) <- it) ; Cb(ari'))
        else
    Ec(dimension c)
    )
      | Ec(n) -> Ec(n)

  let slash k x c =
    match (normalize c) with 
      | Cb(ari) -> 
    (
      let 
    ari' = Array.copy ari 
      in
        if (I.belongs_to x ari.(k))
        then 
    ((ari'.(k) <- I.atom(x)) ; Cb(ari'))
        else
    Ec(dimension c)
    )
      | Ec(n) -> Ec(n)

  let before k c =
    match (normalize c) with 
      | Cb(ari) ->
    (
      if ari <> [||]
      then 
        let ari' = Array.make (dimension c) I.full
        in
    ((ari'.(k) <- I.before ari.(k)) ; (normalize (Cb(ari'))))
      else
        Ec(0)
    )     
      | Ec(n) -> (full n)

  let after k c =
    match (normalize c) with 
      | Cb(ari) ->
    (
      if ari <> [||]
      then
        let ari' = Array.make (dimension c) I.full
        in
    ((ari'.(k) <- I.after ari.(k)) ; (normalize (Cb(ari'))))
      else
        Ec(0)
    ) 
      | Ec(n) -> (full n)

  let during k c =
    match (normalize c) with 
      | Cb(ari) ->
    (
      if ari <> [||]
      then
        let ari' = Array.make (dimension c) I.full
        in
    (ari'.(k) <- ari.(k) ; (normalize (Cb(ari'))))
      else
        Cb([||])
    ) 
      | Ec(n) -> Ec(n)

  (*improvement 2011/04/22: the case where the interior of some entry is
    empty was taken into account by the function normalize*)

  let interior c =
    match c with 
      | Cb ari -> 
  (
    try
      Cb
        (
    Array.map 
      (
        fun it -> 
          let aux = I.interior it in 
          if I.is_not_empty aux 
          then aux 
          else raise Exit
      ) ari
        )
    with
      | Exit -> Ec(Array.length ari)
  )
      | Ec _ -> c

  let closure c =
    match c with 
      | Cb ari -> Cb(Array.map I.closure ari)
      | Ec _   -> c

  (* Par définition même de la connexité *)

  let in_touch c1 c2 = (is_not_empty (meet (closure c1) c2)) || (is_not_empty (meet c1 (closure c2)))

  let belongs_to p c =
    let () = assert(Array.length p = dimension c) in
    match c with 
      | Cb(ari) -> (
          try 
            Array.iter2 (
              fun p it -> 
                if not (I.belongs_to p it) then raise Exit) 
              p ari;
            true
          with Exit -> false)
      | Ec(_) -> false

(*
  let belongs_to_boundary_naive p c =
    (belongs_to p (closure c)) && not(belongs_to p (interior c))
*)

  let belongs_to_boundary p c =
    let () = assert (Array.length p = dimension c) in
    let not_in_interior = ref false in
      match c with 
        | Cb(ari) -> (
          if p <> [||] then (
            let k = ref 0 in
            let current_point = ref p.(0) in
            let current_closure = ref (I.closure ari.(0)) in
              while (!k < Array.length p && I.belongs_to !current_point !current_closure) do
                not_in_interior := !not_in_interior || not (I.belongs_to !current_point (I.interior !current_closure));
                incr k;
                current_point := p.(!k) ; 
                current_closure := (I.closure ari.(!k))
              done;
              !not_in_interior)
          else false)
        | Ec(_) -> false


  (* Attention, les fonctions downward et upward peuvent maintenant
     soulever des exceptions *)

  let downward ?ai c = 
    match ai with 
      | None ->
    (
      match c with
        | Cb(ari) -> normalize (Cb(Array.map I.downward ari))
        | Ec(n) -> Ec(n)
    )
      | Some(ai) ->
    (
      match c with
        | Cb(ari) -> 
      let ari' = Array.copy ari
      in
        normalize 
          (Cb
       (
         Array.iter 
           (
             fun i -> (ari'.(i) <- I.downward ari'.(i))
           ) ai
         ;
         ari'
       )
          )
        | Ec(n) -> Ec(n)
    )

  let upward ?ai c = 
    match ai with 
      | None ->
    (
      match c with
        | Cb(ari) -> normalize (Cb(Array.map I.upward ari))
        | Ec(n) -> Ec(n)
    )
      | Some(ai) ->
    (
      match c with
        | Cb(ari) -> 
      let ari' = Array.copy ari
      in
        normalize 
          (Cb
       (
         Array.iter 
           (
             fun i -> (ari'.(i) <- I.upward ari'.(i))
           ) ai
         ;
         ari'
       )
          )
        | Ec(n) -> Ec(n)
    )

  let in_the_future_of ?it c1 c2 =
    if 
      (
  match it with 
    | None   -> in_touch c1 c2
    | Some b -> b
      )
    then 
      match c1 with
  | Cb(ari1) ->
      (
        match c2 with
    | Cb(ari2) ->
        (
          let dim = Array.length ari1 (* i.e. dimension c1 *) 
          in 
          let answer = Array.make dim I.empty
          in
      (
        try
          let i = ref 0
          in
          let future = ref (I.in_the_future_of ari1.(0) ari2.(0))
          in
            (
        while !future <> I.empty
        do
          answer.(!i) <- !future ;
          i := !i + 1 ;
          future := I.in_the_future_of ari1.(!i) ari2.(!i) 
        done
        ;
        Ec(dim)
            )
        with
          | _ -> Cb(answer)
      )
        )
    | Ec(n) -> Ec(n)
      )
  | Ec(n) -> Ec(n)
    else
      Ec(dimension c1)


  let in_the_past_of ?it c1 c2 =
    if 
      (
  match it with 
    | None   -> in_touch c1 c2
    | Some b -> b
      )
    then
      match c1 with
  | Cb(ari1) ->
      (
        match c2 with
    | Cb(ari2) ->
        (
          let dim = Array.length ari1 (* i.e. dimension c1 *)
          in
          let answer = Array.make dim I.empty
          in
      (
        try
          let i = ref 0
          in
          let past = ref (I.in_the_past_of ari1.(0) ari2.(0))
          in
            (
        while !past <> I.empty
        do
          answer.(!i) <- !past ;
          i := !i + 1 ;
          past := I.in_the_past_of ari1.(!i) ari2.(!i)
        done
        ;
        Ec(dim)
            )
        with
          | _ -> Cb(answer)
      )
        )
    | Ec(n) -> Ec(n)
      )
  | Ec(n) -> Ec(n)
    else
      Ec(dimension c1)

  let product c1 c2 =
    match (c1,c2) with 
      | (Cb(ari1),Cb(ari2)) -> normalize (Cb(Array.append ari1 ari2))
      | (Ec(n1)  ,Cb(ari2)) -> Ec(n1 + (Array.length ari2))    
      | (Cb(ari1),Ec(n2)  ) -> Ec((Array.length ari1) + n2)    
      | (Ec(n1)  ,Ec(n2)  ) -> Ec(n1 + n2)

  (* this function may be buggy *)

  (* this function is based on the fact that iter treates the elements of the 
  set of integers following the increasing order *)

  let projection soi c = 
    let n = Overall.IntSet.cardinal soi in
    match c with
      | Cb ari ->
        let k = ref 0 in
        let projected_ari = Array.make n I.full in
        Overall.IntSet.iter (fun i -> projected_ari.(!k) <- ari.(i); incr k) soi;
        Cb(projected_ari)
      | Ec _ -> Ec n

  let projection_list loi c = 
    let n = List.length loi in
    match c with
      | Cb ari ->
        let k = ref 0 in
        let projected_ari = Array.make n I.full in
        List.iter (fun i -> projected_ari.(!k) <- ari.(i); incr k) loi;
        Cb(projected_ari)
      | Ec _ -> Ec n

  let cylinder d iia =
    let answer = Array.make d I.full
    in
      (
  Array.iter (fun cpl -> answer.(fst cpl) <- (snd cpl)) iia ; Cb(answer)
      )

  let cylinder_list d iil =
    let answer = Array.make d I.full
    in
      (
  List.iter (fun cpl -> answer.(fst cpl) <- (snd cpl)) iil ; Cb(answer)
      )

  let base_partition c =
    match c with 
      | Cb(ari) ->
    let dim = dimension c
    in
      Overall.IntSet.partition (fun k -> ari.(k) <> I.full) (Overall.IntSet.segment_initial dim)
      | Ec(n) -> (Overall.IntSet.empty,Overall.IntSet.segment_initial n)

  let base_coordinates c =
    match (normalize c) with 
      | Cb(ari) -> 
    (
      let answer = ref Overall.IntSet.empty
      in
        Array.iteri
    (fun k i -> (if (i <> I.full) then (answer := Overall.IntSet.add k !answer) else ())) 
    ari
        ;
        !answer
    )
      | Ec(_) -> raise Undefined

  let base_coordinates_array c =
    match (normalize c) with 
      | Cb(ari) -> 
    (
      let answer = ref [||]
      in
        Array.iteri
    (fun k i -> (if (i <> I.full) then (answer := Array.append !answer [|k|]) else ())) 
    ari
        ;
        !answer
    )
      | Ec(_) -> raise Undefined


  let base_coordinates_list c =
    match (normalize c) with 
      | Cb(ari) -> 
    (
      let answer = ref []
      in
        Array.iteri
    (fun k i -> (if (i <> I.full) then (answer := List.append !answer [k]) else ())) 
    ari
        ;
        !answer
    )
      | Ec(_) -> raise Undefined



    
  let base_dimension c =
    Overall.IntSet.cardinal (base_coordinates c)
      
  let slice n c = match c with
    | Cb(ari) -> 
  (
    try
      ari.(n)
    with
      | _ -> (raise Undefined)  
  )
    | Ec(d) -> if (n <(*=*) d) then I.empty else (raise Undefined) (* 2009/10/02 small change *)

  let greatest_regular_value c = 
    match c with
      | Cb(ari) -> 
    (
      Array.fold_left 
        (
    fun accu i -> 
      (
        try
          I.max_regular_value (I.glb i) accu
        with
          | _ -> accu
      )
        ) 
        I.least_regular_value 
        ari 
    )
      | Ec(_)    -> (raise Undefined) 

  let non_degenerate_coordinates c =
    match (normalize c) with 
      | Cb(ari) -> 
    (
      let answer = ref Overall.IntSet.empty
      in
        Array.iteri
    (fun k i -> (if (not(I.is_atomic i)) then (answer := Overall.IntSet.add k !answer) else ())) 
    ari
        ;
        !answer
    )
      | Ec(_) -> raise Undefined

  (* Cette fonction est peut-être buggée...*)

  let complement c = 
    let answer = ref []
    in
      (
  for i=0 to (dimension c)-1
  do
    answer := (before i c)::(after i c)::!answer
  done
  ;
  !answer
      )

  let are_cofinal c0 c1 = let c2= meet c0 c1 in (in_the_past_of c2 c0 = c0) && (in_the_past_of c2 c1 = c1)

  let are_coinitial c0 c1 = let c2= meet c0 c1 in (in_the_future_of c2 c0 = c0) && (in_the_future_of c2 c1 = c1)

  let ginzu_complement_aux c = 
    match c with 
      | Cb(ari) ->
          (
            let dim = dimension c in
            let current = Array.make dim 0 in
            let answer = ref [] in
            let aux k itv = 
              match k with 
                | 0 -> let local = (I.before itv) in 
                    if local <> I.empty then local else raise Empty_interval  
                | 1 -> let local = (I.after  itv) in 
                    if local <> I.empty then local else raise Empty_interval
                | _ -> itv in 
            try
              while true do
                  answer := (
                    try
                      (Cb(Array.mapi (fun i itv -> aux (current.(i)) itv) ari))::!answer
                    with Empty_interval -> !answer);
                  Overall.Array.cartesian_next_array_consumer current 2 
              done;
              !answer
            with Exit -> (List.tl !answer)
          )
      | Ec(n) -> [full n]

  let ginzu_complement c =
    match c with 
      | Cb(ari) ->
    let dim = dimension c in
    let lbc = base_coordinates_list c in
    let c' = projection_list lbc c in
      List.map (
        fun x -> match x with 
          | Cb(x') -> cylinder_list dim (List.combine lbc (Array.to_list x'))
          | _      -> Ec(dim)) 
        (ginzu_complement_aux c')
      | Ec(n) -> [full n]

  let cset2D_complement c =
    let base = base_coordinates c
    in
      if (Overall.IntSet.cardinal base) <> 2
      then
  ginzu_complement c
      else
  (
    match c with
      | Cb(ari) ->
    let i = Overall.IntSet.min_elt base 
    and j = Overall.IntSet.max_elt base 
    and dim = Array.length ari 
    and a00 = Array.copy ari
    and a01 = Array.copy ari 
    and a10 = Array.copy ari
    and a11 = Array.copy ari 
    in
    let hrz = ari.(i)
    and vrt = ari.(j)
    in
    let c00 = (a00.(i)<-I.before   hrz ; a00.(j)<-I.before   vrt ; (normalize (Cb(a00))))
    and c11 = (a11.(i)<-I.after    hrz ; a11.(j)<-I.after    vrt ; (normalize (Cb(a11))))
    and c01 = 
      (
        a01.(i)<-
          (
      if I.rgt_bool_of_bound hrz 
      then I.downward (I.interior hrz) 
      else I.downward (I.closure hrz)
          ) 
        ;
        a01.(j)<-
          (
      if I.lft_bool_of_bound vrt 
      then I.upward   (I.interior vrt) 
      else I.upward   (I.closure vrt)
          ) 
        ; 
        (normalize (Cb(a01)))
      )
    and c10 = 
      (
        a10.(i)<-
          (
      if I.lft_bool_of_bound hrz 
      then I.upward   (I.interior hrz) 
      else I.upward   (I.closure hrz)
          ) 
        ; 
        a10.(j)<-
          (
      if I.rgt_bool_of_bound vrt 
      then I.downward (I.interior vrt) 
      else I.downward (I.closure vrt)
          ) 
        ; 
        (normalize (Cb(a10)))
      )
    in
    let codim0 = List.filter 
      (fun x -> match x with | Cb(_) -> true | _ -> false) 
      [c00;c01;c10;c11] 
    in
    let codim1 = List.filter 
      (fun x -> (x<>(empty dim))) 
      [back i c00;back j c00;meet (back i c01) (after j c);meet (back j c10) (after i c)]
    in
      raise (Base2D(codim0,codim1))
      | Ec(n) -> [full n] (*this case should not occur*)
  )

end


