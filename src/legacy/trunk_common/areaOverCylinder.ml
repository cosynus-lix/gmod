(***********************************************************************)
(*                                                                     *)
(*                                GMOD                                 *)
(*                                                                     *)
(*                         AreaOverCylinder.ml                         *)
(*                                                                     *)
(***********************************************************************)


module type In =
sig
  module IntSet:Set.S with type elt = int
  module Array:
    sig
      val for_all: ('a -> bool) -> ('a array) -> bool  
    end
  module List:
    sig
      val initial_segment: ?start:int -> int -> int list    
    end
end with type IntSet.elt = int

module type S = Area.S

  module Make(Overall:In)(X:Cylinder.S) =
  struct

    exception Undefined

    exception Exit

    module Soc = Set.Make(X)

    module Points =
    struct
      type t = X.point
      let compare = X.compare_points
    end

    module Sop = Set.Make(Points)

    type regular_value = X.regular_value 
    
    type point = X.point 
    
    type generator = X.generator
    
    type brick = X.t

    type underlying = X.underlying

    type ints = Overall.IntSet.t

    type t = Soc.t

    let choose = Soc.choose (*added on 15/03/2012*)
    let remove = Soc.remove (*added on 16/03/2012*)
    let exists = Soc.exists
    let add = Soc.add
    let cardinal = Soc.cardinal
    let mem = Soc.mem
    let compare = Soc.compare
    let fold = Soc.fold
    let iter = Soc.iter
    let min_elt = Soc.min_elt

    let underlying a =
      try X.underlying (Soc.choose a)
      with Not_found -> failwith "Attention : L'ensemble vide ne représente aucune région cubique (Area.underlying)."

    let dimension a =
      try X.dimension (Soc.choose a)
      with Not_found -> failwith "Attention : L'ensemble vide ne représente aucune région cubique (Area.dimension)."

    let remove_useless_empty_brick a =
      let a' = Soc.remove (X.empty (underlying a)) a in
      if a' <> Soc.empty then a' else a

    let is_empty a = Soc.for_all (fun c -> X.is_empty c) a

    let is_not_empty a = Soc.exists (fun c -> X.is_not_empty c) a

    (* Attention : l'argument des deux fonctions suivantes doit être
       en forme normale *)

    let is_full a = Soc.exists (fun c -> X.is_full c) a

    let is_not_full a = Soc.for_all (fun c -> X.is_not_full c) a

    let string_of ?(gui=true) a =
      try
  let a_cube = Soc.choose a
  in
  let a' = Soc.remove a_cube a
  in
    (
      if gui
      then
        (
    let aux c s = ((X.string_of c)^" \n| "^s)
    in
      Soc.fold aux a' (X.string_of a_cube)
        )
      else
        (
    let aux c s = (s^" \n| "^(X.string_of c))
    in
      Soc.fold aux a' (X.string_of a_cube)
        )
    )
      with
  | Not_found -> "string_of : the empty set does not represent any area"


    let print a =
      try
  let a_cube = Soc.choose a
  in
  let a' = Soc.remove a_cube a
  in
  let aux c = print_string ("| "^(X.string_of c)^"\n")
  in
    ((print_string ("  "^(X.string_of a_cube)^"\n")) ; (Soc.iter aux a'))
      with
  | Not_found -> print_string "The empty set of cubes does not represent any area"

    let next c a =
      let rc = ref c
      in
  (
    (
      Soc.iter
        (
    fun c' ->
      if (((X.compare c c') < 0) && ((X.compare c' !rc) < 0))
      then
        (rc := c')
      else
        ()
        )
        a
    )
    ;
    !rc
  )

    let of_cube c = Soc.singleton c

    let empty ?u ?(d=0) () =
      match u with
        | Some v -> of_cube (X.empty v)
        | None   -> failwith "Area.OverCyle.empty: the underlying space has to be specified"

    let zero = empty ~u:X.zero ()

    let full ?u ?(d=0) () =
      match u with
      | Some v -> of_cube (X.full v)
      | None   -> failwith "Area.OverCyle.full: the underlying space has to be specified"

    let one = full ~u:X.zero ()

    let atom ?u p =
      match u with
      | Some v -> of_cube (X.atom v p)
      | None   -> failwith "Area.OverCyle.atom: the underlying space has to be specified"

    (*  let atom u p = of_cube (X.atom u p) *)

    let make ml =
      if ml <> []
      then List.fold_right (fun m accu -> (Soc.add m accu)) ml Soc.empty
      else raise (Invalid_argument "make: the empty set does not represent a cubical area")

    let join a1 a2 = Soc.union a1 a2

    let rec join_list l = match l with
      | [x] -> x
      | x :: l -> join x (join_list l)
      | [] -> raise Undefined
      
    let join_array a = match a with
      | [||] -> raise Undefined
      | _ -> Array.fold_left (fun accu x -> join accu x) a.(0) Array.(sub a 1 (pred (length a)))

    let meet a1 a2 =
      if (a1 = (Soc.empty) || a2 = (Soc.empty))
      then
  raise (Invalid_argument "intersection: the empty set does not represent a cubical area")
      else
  (
    let answer = ref (of_cube(X.empty ((*dimension*)underlying a1)))
    and which_have_to_be_added = ref []
    in
    let add_intersection_to_answer m1 m2 =
      (
        which_have_to_be_added := (X.intersection m1 m2) (* Attention : ici on a désormais une liste de cubes qui peut contenir plus d'un élément *)
        ;
        (
    let scanner elt_answer =
      if List.exists (fun m -> X.is_included elt_answer m) !which_have_to_be_added
      then
        (answer := Soc.remove elt_answer !answer)
      else
        (which_have_to_be_added := List.filter (fun m -> (not(X.is_included m elt_answer))) !which_have_to_be_added)
    in
      (
        Soc.iter scanner !answer
        ;
        (List.iter (fun m -> (answer := Soc.add m !answer)) !which_have_to_be_added)
      )
        )
      )
    in
      Soc.iter
        (
    fun m1 ->
      (
        Soc.iter
          (fun m2 -> add_intersection_to_answer m1 m2)
          a2
      )
        )
        a1
      ;
      !answer
  )
  
      let rec meet_list l = match l with
      | [x] -> x
      | x :: l -> meet x (meet_list l)
      | [] -> raise Undefined
      
    let meet_array a = match a with
      | [||] -> raise Undefined
      | _ -> Array.fold_left (fun accu x -> meet accu x) a.(0) Array.(sub a 1 (pred (length a)))

    (* Le complémentaire d'une réunion est l'intersection des
       complémentaires. Par ailleurs, l'intersection est associative. *)

    let complement a =
      try
        let d = underlying a in
        let aux m accu = meet (make (X.complement m)) accu in
        Soc.fold aux a (of_cube (X.full d))
      with Not_found -> raise (Invalid_argument "complement : the empty set does not represent a cubical area")
   
    let is_included a1 a2 = Soc.for_all (fun m1 -> Soc.exists (fun m2 -> X.is_included m1 m2) a2) a1

    let closure a = Soc.fold (fun m accu -> Soc.add (X.closure m) accu) a Soc.empty

    let normalize a = complement (complement a)
    
    let interior_version_safe a = Soc.fold (fun m accu -> Soc.add (X.interior m) accu) (normalize a) Soc.empty

    let interior_version_unsafe a = Soc.fold (fun m accu -> Soc.add (X.interior m) accu) a Soc.empty

    (* Certains algorithmes nécessitent plusieurs fois l'emploi de la forme normlisée, il peut donc être intéressant *)
    (* de la calculer une bonne fois pour toute et de la réutiliser, sachant que son obtention est très coûteuse en *)
    (* temps de calcul. *)

    let interior a = interior_version_unsafe a

    let difference a1 a2 = meet a1 (complement a2)



    (*Provide the least representation. Very expensive operation. Should only be use for display*)
    let minimize a =
      let mandatory c = is_not_empty (difference (of_cube c) (Soc.remove c a)) in
      Soc.filter mandatory a


    let compress a = Soc.filter (fun c -> (Soc.for_all (fun c' -> (not(X.is_included c c'))||(c=c')) a)) a

    let boundary a = difference (closure a) (interior a)

    (* past_cone et future_cone sont à revoir dans le cas tore et cyle *)

    let future_cone departure a =
      try
  let d = underlying a
  in
  let current_wave = ref departure
  in
  let global_answer = ref (empty ~u:d ())
  in
  let next_wave w =
    let local_answer = ref (empty ~u:d ())
    in
      (
        (
    Soc.iter
      (
        fun cw ->
          Soc.iter
      (
        fun ca ->
          (
            if (X.in_touch cw ca) (* Le test de contact doit être fait car la fonction in_the_future_of du module Cube ne respecte pas sa spécification, des modifications viennent d'être faites pour y remédier. *)
            then
        (
          local_answer := Soc.add (X.in_the_future_of cw ca) !local_answer
        )
            else
        ()
          )
      )
      a
      )
      w
        )
        ;
        !local_answer
      )
  in
    (
      while not(is_included !current_wave !global_answer)
      do
        global_answer := join !global_answer !current_wave ;
        current_wave := next_wave !current_wave
      done
      ;
      !global_answer
    )
      with
  | Not_found -> failwith "future_cone : the empty set does not represent any area"

    (*  let future_cone_point p a = future_cone (atom p) a *)

    let past_cone arrival a =
      try
  let d = underlying a
  in
  let current_wave = ref arrival
  in
  let global_answer = ref (empty ~u:d ())
  in
  let next_wave w =
    let local_answer = ref (empty ~u:d ())
    in
      (
        (
    Soc.iter
      (
        fun cw ->
          Soc.iter
      (
        fun ca ->
          (
            if (X.in_touch cw ca)
            then
        (
          local_answer := Soc.add (X.in_the_past_of cw ca) !local_answer
        )
            else
        ()
          )
      )
      a
      )
      w
        )
        ;
        !local_answer
      )
  in
    (
      while not(is_included !current_wave !global_answer)
      do
        global_answer := join !global_answer !current_wave ;
        current_wave := next_wave !current_wave
      done
      ;
      !global_answer
    )
      with
  | Not_found -> failwith "past_cone : the empty set does not represent any area"

    let coinitial a1 a2 = let b = meet a1 a2 in (future_cone b a1 = a1) && (future_cone b a2 = a2)
    let cofinal a1 a2 = let b = meet a1 a2 in (past_cone b a1 = a1) && (past_cone b a2 = a2)
              
    (* let past_cone_point p a = past_cone (atom p) a *)

    let product a1 a2 =
      Soc.fold
  (
    fun m1 answer ->
      (
        Soc.fold
    (
      fun m2 answer' -> Soc.add (X.product m1 m2) answer'
    )
    a2
    answer
      )
  )
  a1
  Soc.empty


    let rec product_list l = match l with
      | x :: l -> product x (product_list l)
      | [] -> one
      
    let product_array a = Array.fold_left (fun accu x -> product accu x) one a

    let rec product_fun f a b = if a > b then one else product (f a) (product_fun f (succ a) b)

    let rec product_fun_left f a b = if a > b then one else product (f a) (product_fun_left f (succ a) b)

    let rec product_fun_right f a b = if a > b then one else product (product_fun_right f a (pred b)) (f b)

    let rec exponent a n =
      if n <= 0
      then of_cube (X.full X.zero)
      else product a (exponent a (n - 1))

module AC = Algebra.Combinator
(
  struct
    type t = Soc.t
    let zero = zero
    let one = one
    let full a = full ~u:(underlying a) ~d:(dimension a) ()
    let union = join
    let product = product
    let product_array = product_array
    let product_list = product_list
    let product_fun = product_fun
    let normalize = normalize
    let compress = compress
    let string_of x = string_of x
  end
)  

let generalized_product = AC.generalized_product
let levelwise_product ?u ?g level aaa = AC.levelwise_product ?g level aaa
let locus_higher_than = AC.locus_higher_than
let locus_lower_than = AC.locus_lower_than



    let belongs_to p a =
      Soc.exists (fun c -> X.belongs_to p c) a

    let belongs_to_closure p a =
      Soc.exists (fun c -> X.belongs_to p (X.closure c)) a

    let deadlocks a =
      try
  let d = underlying a
  in
    (
      Soc.fold
        (
    fun c accu ->
      try
        let p = X.lub c
        in
          (
      if
        (
          (
            (not(belongs_to p a))
            ||
        (
          let aux = atom ~u:d p (* of_cube (X.atom ~u:d p) *)
          in
            remove_useless_empty_brick (future_cone aux a) = aux 
        )
          ) 
          && 
            (
        Soc.for_all 
          (
            fun c' -> 
              (
          (not(X.belongs_to p (X.closure c'))) ||
            (
              try 
                (X.lub c' = p) 
              with 
                | _ -> false
            ) ||
            (not(X.in_touch c c'))
              )
          ) 
          a
            )
        )
      then
        Soc.add (X.atom d p) accu
      else
        accu
          )
      with
        | _ -> accu
        )
        a
        (empty ~u:d ())
    )
      with
  | Not_found -> failwith "deadlocks : the empty set does not represent any area"


    let sources a =
      try
  let d = underlying a
  in
    (
      Soc.fold
        (
    fun c accu ->
      try
        let p = X.glb c
        in
          (
      if 
        (
          (
            (not(belongs_to p a))
            ||
        (
          let aux = atom ~u:d p
          in
            remove_useless_empty_brick (past_cone aux a) = aux 
        )
          ) 
          &&
            (
        Soc.for_all 
          (
            fun c' -> 
              (
          (not(X.belongs_to p (X.closure c'))) ||
            (
              try 
                (X.glb c' = p) 
              with 
                | _ -> false
            ) ||
            (not(X.in_touch c c'))
              )
          ) 
          a
            )
        )
      then
        Soc.add (X.atom d p) accu
      else
        accu
          )
      with
        | _ -> accu
        )
        a
        (empty ~u:d ())
    )
      with
  | Not_found -> failwith "sources : the empty set does not represent any area"



    let might_go_infinity a =
      try
  let d = underlying a
  in
  let unbounded_sub_area =
    Soc.fold
      (
        fun c accu ->
    try (* ce critère n'est pas suffisant *)
      let _ = X.lub c
      in
        accu
    with
      | X.Undefined -> (Soc.add c accu)
      )
      a
      (empty ~u:d ())
  in
    past_cone unbounded_sub_area a
      with
  | Not_found -> failwith "might_come_to_infinity : the empty set does not represent any area"

    let deadlock_attractor a = difference a (might_go_infinity a)

    let might_go_deadlock a = past_cone (deadlocks a) a

    let infinity_attractor a = difference a (might_go_deadlock a)

    let in_touch a1 a2 = 
      (is_not_empty(meet(closure a1)a2)) || (is_not_empty(meet(closure a2)a1))

    let departure a =
      Soc.fold
  (
    fun c accu ->
      if Soc.for_all (fun c' -> X.is_included (X.in_the_past_of c c') c) a
      then
        Soc.add c accu
      else
        accu
  ) 
  a
  Soc.empty

    let arrival a = 
      Soc.fold
  (
    fun c accu ->
      if Soc.for_all (fun c' -> X.is_included (X.in_the_future_of c c') c) a
      then
        Soc.add c accu
      else
        accu
  ) 
  a
  Soc.empty



    module FGSL = Algebra.Free.Graded.SemiLattice
      (
  struct 
    type t = X.generator
    let compare = fun g -> X.compare_generator g 
    let string_of = X.string_of_generator
  end
      )

    (* Directement importé du cas cubique, n'a pas été testé *)

    let connected_component c a =
      let rec aux accu has_increased =
  if has_increased
  then
    let augmented_accu =
      (
        Soc.fold
    (
      fun c local_accu ->
        (Soc.union local_accu (Soc.filter (fun x -> X.in_touch c x) a))
    )
    accu
    Soc.empty
      )
    in
      aux augmented_accu (not (augmented_accu = accu))
  else
    accu
      in
  aux (of_cube c) true

    let of_list cl =
      let answer = List.fold_right (fun c accu -> (Soc.add c accu)) cl Soc.empty
      in
  if answer <> Soc.empty
  then
    answer
  else
    raise (Invalid_argument "make : the empty set does not represent a torical area")

    let to_list a = Soc.elements a

    (* let to_array a = Soc.fold (fun c accu -> Array.append accu [|c|]) a [||] *)

    let of_array ca = 
      let answer = Array.fold_right (fun c accu -> (Soc.add c accu)) ca Soc.empty
      in
  if answer <> Soc.empty
  then
    answer
  else
    raise (Invalid_argument "make : the empty set does not represent a cubical area")

    let to_array a = 
      let answer = Array.make (Soc.cardinal a) (X.empty X.zero) in
      let i = ref (-1) in
  Soc.iter (fun c -> (incr i ; Array.set answer !i c)) a
  ; 
  answer


    let connected a = ((connected_component (Soc.choose a) a) = a)

    let factorize  ?(unpacked=true) a = FGSL.factorize ~unpacked:true (Soc.fold (fun c accu -> FGSL.add (X.to_array c) accu) a FGSL.zero)
    let cset2D_of_complement_of_area a = failwith "OverCyle.cset2D : NIY"
    let upward a = failwith "OverCyle.upward : NIY"
    let upper_corners_area a = failwith "OverCyle.upper_corner_area : NIY"
    let terminal b r = failwith "OverCyle.terminal : NIY"
    let strictly_below k r a = failwith "OverCyle.strictly_below : NIY"
    let strictly_above k r a = failwith "OverCyle.strictly_above : NIY"
    let reachable a = failwith "OverCyle.reachable : NIY"
    let past_cone_point p a = failwith "OverCyle.past_cone_point : NIY"
    let local_deadlock_attractor s a = failwith "OverCyle.local_deadlock_attractor : NIY"
    let local_might_go_infinity s a = failwith "OverCyle.local_might_go_infinity : NIY"
    let lub a = failwith "OverCyle.lub : NIY"
    let initial  b r = failwith "OverCyle.initial : NIY"
    let glb a = failwith "OverCyle.glb : NIY" 
    let ginzu a = failwith "OverCyle.ginzu : NIY"
    let future_cone_point  p a = failwith "OverCyle.future_cone_point : NIY"
    let filter = Soc.filter
    let face b k a = failwith "OverCyle.face : NIY"
    let downward a = failwith "OverCyle.upward : NIY"
    let cubical_order_convex a = failwith "OverCyle.cubical_order_çconvex : NIY"
    let cubic_hull a = failwith "OverCyle.cubic_hull : NIY"
    let complement_of_cube c = make (X.complement c)
    let common a = failwith "OverCyle.common : NIY"
    let generalized_common ?d a = failwith "OverCyle.generalized_common is not implemented yet"
    let bounded b1 b2 r1 r2 = failwith "OverCyle.bounded : NIY"
      (*let belongs_to_closure p a = failwith "OverCyle.belongs_to_closure : NIY"
  let belongs_to p a = failwith "OverCyle.belongs_to : NIY"*)
    let below k r a = failwith "OverCyle.below : NIY"
    let base a = failwith "OverCyle.base : NIY"
    let above k r a = failwith "OverCyle.above : NIY"

  end (* Make *)
