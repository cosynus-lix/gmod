(***********************************************************************)
(*                                                                     *)
(*                               ALCOOL                                *)
(*                                                                     *)
(*                              CEA LIST                               *)
(*                                                                     *)
(*                          AreaOverTorus.ml                           *)
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

module Make(Overall:In)(T:Torus.S) =
  struct

    exception Undefined

    exception Exit

    module Soc = Set.Make(T)

    module Points =
    struct
      type t = T.point
      let compare = T.compare_points
    end

    module Sop = Set.Make(Points)

    type regular_value = T.regular_value 
    type point = T.point 
    type generator = T.arc 
    type brick = T.t 
    type underlying = T.underlying

    type ints = Overall.IntSet.t

    type t = Soc.t
  
    let add = Soc.add

    let choose = Soc.choose (*added on 15/03/2012*)
    let remove = Soc.remove (*added on 16/03/2012*)

    let cardinal = Soc.cardinal

    let underlying a =
      try T.underlying (Soc.choose a)
      with Not_found -> failwith "Attention : L'ensemble vide ne représente aucune région cubique (Area.underlying)."

    let dimension a =
      try T.dimension (Soc.choose a)
      with Not_found -> failwith "Attention : L'ensemble vide ne représente aucune région cubique (Area.dimension)."

    let string_of ?(gui=false) a = 
      try
        let a_cube = Soc.choose a in
        let a' = Soc.remove a_cube a in
          (
            if gui
            then
              let aux c s = ((T.string_of c)^" \n| "^s) in
                Soc.fold aux a' (T.string_of a_cube)
            else
              let aux c s = (s^" \n| "^(T.string_of c)) in
                Soc.fold aux a' (T.string_of a_cube))
      with Not_found -> let () = print_endline "Not_found" in "string_of : the empty set does not represent any area"


    let remove_useless_empty_brick a =
      let a' = Soc.remove (T.empty (dimension a)) a in
        if a' <> Soc.empty then a' else a

    let next c a =
      let rc = ref c
      in
  (
    (
      Soc.iter
        (
    fun c' ->
      if (((T.compare c c') < 0) && ((T.compare c' !rc) < 0))
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

    let is_empty a = Soc.for_all (fun c -> T.is_empty c) a

    let is_not_empty a = Soc.exists (fun c -> T.is_not_empty c) a

    (* Attention : l'argument des deux fonctions suivantes doit être
       en forme normale *)

    let is_full a = Soc.exists (fun c -> T.is_full c) a

    let is_not_full a = Soc.for_all (fun c -> T.is_not_full c) a

    let empty ?u ?(d=0) () = of_cube (T.empty d)

    let zero = empty ()

    let full ?u ?(d=0) () = of_cube (T.full d)

    let one = full ()

    let atom ?u p = of_cube (T.atom p)

    (* Transforme une liste de cubes en ensemble de cubes *)

    let make cl =
      if cl <> []
      then List.fold_right (fun c accu -> (Soc.add c accu)) cl Soc.empty
      else raise (Invalid_argument "make : the empty set does not represent a cubical area")

    let join a1 a2 = Soc.union a1 a2

    let rec join_list l = match l with
      | [x] -> x
      | x :: l -> join x (join_list l)
      | [] -> raise Undefined
      
    let join_array a = match a with
      | [||] -> raise Undefined
      | _ -> Array.fold_left (fun accu x -> join accu x) a.(0) Array.(sub a 1 (pred (length a)))

    (* N'a pas encore été testée *)

    (* Pose visiblement un problème *)

    (* cf complémentaire de la croix suisse *)

    let meet a1 a2 =
      if a1 = Soc.empty || a2 = Soc.empty
      then raise (Invalid_argument "intersection : the empty set does not represent a cubical area")
      else (
        let answer = ref (of_cube(T.empty (T.dimension (Soc.choose a1)))) in
        let which_have_to_be_added = ref [] in
        let scanner elt_answer =
          if List.exists (fun c -> T.is_included elt_answer c) !which_have_to_be_added
          then answer := Soc.remove elt_answer !answer
          else 
            which_have_to_be_added := 
              List.filter (
                fun c -> not(T.is_included c elt_answer)) 
                !which_have_to_be_added in        
        let add_intersection_to_answer c1 c2 =
          which_have_to_be_added := T.intersection c1 c2; (* Attention : ici on a désormais une liste de cubes qui peut contenir plus d'un élément *)
          Soc.iter scanner !answer;
          List.iter (
            fun c -> (answer := Soc.add c !answer)) 
            !which_have_to_be_added in
        let () = 
          Soc.iter (
            fun c1 -> (
              Soc.iter (
                fun c2 -> add_intersection_to_answer c1 c2)
                a2))
            a1 in
        !answer)

  let meet a1 a2 = 
    let output = meet a1 a2 in
    let () = if false then Printf.printf "AreaOverTorus.meet %s %s\n=\n%s\n%!" 
      (string_of a1) 
      (string_of a2) 
      (string_of output) in
    output
  

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
      try (
        let d = T.dimension (Soc.choose a) in
        let aux c accu = meet (make (T.complement c)) accu in
        Soc.fold aux a (of_cube (T.full d)) )
      with Not_found -> raise (Invalid_argument "complement : the empty set does not represent a cubical area")

 
    let complement c = 
      let output = complement c in
      let () = if false then 
        Printf.printf "AreaOfTorus.complement %s\n=\n%s\n%!" (string_of c) (string_of output) in
      output


    let is_included a1 a2 =
      Soc.for_all (
        fun c1 -> Soc.exists (
          fun c2 -> T.is_included c1 c2) 
          a2) 
        a1

    let closure a = Soc.fold (fun c accu -> Soc.add (T.closure c) accu) a Soc.empty

    let interior a = Soc.fold (fun c accu -> Soc.add (T.interior c) accu) a Soc.empty

    (* Certains algorithmes nécessitent plusieurs fois l'emploi de la forme normlisée, il peut donc être intéressant *)
    (* de la calculer une bonne fois pour toute et de la réutiliser, sachant que son obtention est très coûteuse en *)
    (* temps de calcul. *)

  let difference a1 a2 = meet a1 (complement a2)

  let normalize a = complement (complement a)

  let normalize a =
    let output = normalize a in
    let () = if false then Printf.printf "AreaOverTorus.normalize %s\n=\n%s\n%!" (string_of a) (string_of output) in
    output


  (*Provide the least representation. Very expensive operation. Should only be use for display*)
  let minimize a =
    let mandatory c = is_not_empty (difference (of_cube c) (Soc.remove c a)) in
    Soc.filter mandatory a


  let compress a = Soc.filter (fun c -> (Soc.for_all (fun c' -> (not(T.is_included c c'))||(c=c')) a)) a

  let boundary a = difference (closure a) (interior a)

    let future_cone departure a =
      try
  let d = T.dimension (Soc.choose a)
  in
  let current_wave = ref departure
  in
  let global_answer = ref (empty ~d:d ())
  in
  let next_wave w =
    let local_answer = ref (empty ~d:d ())
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
            if (T.in_touch cw ca) (* Ce test devrait désormais être inutile : à vérifier *)
            then
        (
          local_answer := Soc.add (T.in_the_future_of cw ca) !local_answer
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

    let future_cone_point p a = future_cone (atom p) a

    let past_cone arrival a =
      try
  let d = T.dimension (Soc.choose a)
  in
  let current_wave = ref arrival
  in
  let global_answer = ref (empty ~d:d ())
  in
  let next_wave w = 
    let local_answer = ref (empty ~d:d ())
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
            if (T.in_touch cw ca)  (* Ce test devrait désormais être inutile : à vérifier *)
            then
        (
          local_answer := Soc.add (T.in_the_past_of cw ca) !local_answer
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

    let past_cone_point p a = past_cone (atom p) a

    let coinitial a1 a2 = let b = meet a1 a2 in (future_cone b a1 = a1) && (future_cone b a2 = a2)
    let cofinal a1 a2 = let b = meet a1 a2 in (past_cone b a1 = a1) && (past_cone b a2 = a2)

    (* Il est judicieux d'avoir normalisé les régions avant d'effectuer le produit *)

    let product a1 a2 =
      Soc.fold
  (
    fun c1 answer ->
      (
        Soc.fold
    (
      fun c2 answer' -> Soc.add (T.product c1 c2) answer'
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
      if n<=0
      then of_cube (T.full 0)
      else product a (exponent a (n-1))

    (*------------------------------------------------------------------------------------------------*)
    (*                                   generalized_product                                          *)
    (*------------------------------------------------------------------------------------------------*)

    (*
      Cette fonction est à rapprocher de la définition de produit extérieur en calcul différentiel.
    *)
    
    module AC = Algebra.Combinator 
    (struct 
    type t = Soc.t 
    let zero = zero 
    let one = one 
    let full a = full ~d:(dimension a) () 
    let product = product 
    let product_array = product_array
    let product_list = product_list
    let product_fun = product_fun
    let union = join
    let normalize = normalize
    let compress = compress
    let string_of x = string_of x
    end)
    
(*
    let generalized_product n aa = 
      let full a = full ~d:(dimension a) () in
      Algebra.PCombinator.generalized_product n zero one full join product aa
*)
    
    
    (* Attention, la fonction "levelwise_product" n'a pas encore été testée *)

    (* L'argument "aaa" est un tableau de tableau de région linéaire
       i.e. de dimension 1, autrement dit une matrice de régions
       linéaires. A chaque ligne est attribuée une fonction en escalier
       définie sur la demi-droite réelle positive et à valeurs dans
       l'ensemble des entiers naturels. Etant donnée une ligne, la k-ème
       case contient le lieu des points de la demi-droite réelle positive
       pour lesquels la valeur de la fonction est k+1. En effet,
       l'indexation des tableaux en OCaml fait que la première case porte
       l'indice 0. En supposant que la matrice possède n lignes et que l'on
       note f_0,...,f_{n-1} les fonctions qui correspondent aux différents
       lignes de la matrice, on associe à chaque point x du produit de n
       copies de la demi-droite réelle positive la somme des valeurs des
       fonctions f_0,...,f_{n-1} au point x, c'est-à-dire
       f_0(x)+...+f_{n-1}(x). L'appel à la fonction avec l'argument "level"
       calcul alors le lieu des points où cette somme est supérieure ou
       égale à la valeur "level". *)

(*

    let levelwise_product ?u ?(level=0) (* niveau d'occupation dont on cherche la région *) aaa (* array of array of areas *) =
      let rec sum_till n =
  if n<0
  then 0
  else ( (sum_till (n-1)) + (Array.length (aaa.(n))) )
      in
      let dimension = Array.length aaa
      in
      let initial_sequence (* sequence *) level =
  Array.init
    dimension
          (fun n -> min (max 0 (level-(sum_till (n-1)))) (Array.length (aaa.(n))) )
      in
      let next_sequence int_seq =
  let fafs = (* first available free space *)
          (
            let n = ref 0 and only_met_zero = ref true
            in
              while
    (
                  try
        (
                      ((int_seq.(!n)>=(Array.length (aaa.(!n)))) || (!only_met_zero))
        )
      with
        | Invalid_argument "index out of bounds" -> false
    )
              do
    n := (!n)+1
    ;
    (only_met_zero := (!only_met_zero && (int_seq.(!n-1)=0)))
              done
        ;
        !n
    )
  in
  let remaining_level = (* masse qu'il faut répartir *)
    (
      let accu = ref 0
      in
        for n = 0 to (fafs-1)
        do
    accu := (!accu) + int_seq.(n)
        done
        ;
        (!accu - 1) (* dans la masse contenue avant le premier emplacement disponible, il faut prélever une unité que l'on fait passer au "niveau du dessus" *)
    )
  in
  let answer = initial_sequence remaining_level
  in
    (
      answer.(fafs) <- (int_seq.(fafs)+1) (* dans le cas où fafs=dimension, une exception est levée *)
      ;
      for n = (fafs + 1) to (dimension - 1)
      do
        answer.(n) <- int_seq.(n)
      done
      ;
      answer
    )
      in
      let first_nonzero int_seq pos =
  (
    let rec aux current =
      if int_seq.(current) = 0
      then
        aux (current+1)
      else
        current
    in
      try
        aux pos
      with | Invalid_argument "index out of bounds" -> dimension
  )
      in
      let local_product int_seq =
  let n = ref (first_nonzero int_seq 0)
  in
  let accumulator = ref (of_cube (T.full !n))
  in
          (
            while !n < dimension
      do
              let k = (first_nonzero int_seq (!n+1))
        in
    (
      accumulator :=
        (
          product
      (!accumulator)
      (
        product
          (aaa.(!n).((int_seq.(!n))-1)) (* int_seq.(!n) <> 0 car !n est une valeur issue de la fonction first_nonzero *)
          (of_cube (T.full (k-(!n)-1)))
      )
        )
      ;
      (n := k)
    )
      done
      ;
      !accumulator
          )
      in
      let accumulator = ref (empty ~d:dimension ())
      in
  (
    if level > sum_till (dimension-1)
    then
      !accumulator
    else
      (
        let current = ref (initial_sequence level)
        in
    (
      try
        while true
        do
          accumulator := join !accumulator (local_product (!current))
          ;
          current := next_sequence !current
        done
      with
                    | Invalid_argument "index out of bounds" -> ()
    )
    ;
    !accumulator
      )
  )

    (*

      let locus_higher_than ?(cochain=false) ?(ground=(fun x -> 0)) level functions = (* To be done *)
      failwith "Area.OverTore.locus_higher_than is not implemented yet"

    *)


    let locus_higher_than ?(cochain=false) ?(ground=(fun x -> 0)) level functions =
      let aaa =
  if cochain
  then
    functions
  else
    let aux = (Array.copy functions)
    in
      (
        Array.iter
    (
      fun line ->
        (
          for i = (Array.length line)-2 downto 0
          do
      line.(i) <- (join (line.(i)) (line.(i+1)))
          done
        )
    )
    aux
        ;
        aux
      )
      in
      let dim = Array.fold_left (fun accu a -> accu + (dimension (a.(0)))) 0 aaa
      in
      let rec sum_till n =
  if n<0
  then 0
  else ( (sum_till (n-1)) + (Array.length (aaa.(n))) )
      in
      let initial_sequence level =
  Array.init
    (Array.length aaa)
          (fun n -> min (max 0 (level-(sum_till (n-1)))) (Array.length (aaa.(n))) )
      in
      let next_sequence int_seq =
  let fafs =
          (
            let n = ref 0 and only_met_zero = ref true
            in
              while
    (
                  try
        (
                      (int_seq.(!n)>=(Array.length (aaa.(!n)))) || (!only_met_zero)
        )
      with
        | Invalid_argument "index out of bounds" -> false
    )
              do
    n := (!n)+1
    ;
    (only_met_zero := (!only_met_zero && (int_seq.(!n-1)=0)))
              done
        ;
        !n
    )
  in
  let remaining_level =
    (
      let accu = ref 0
      in
        for n = 0 to (fafs-1)
        do
    accu := (!accu) + int_seq.(n)
        done
        ;
        (!accu - 1)
    )
  in
  let answer = initial_sequence remaining_level
  in
    (
      answer.(fafs) <- (int_seq.(fafs)+1)
      ;
      for n = (fafs + 1) to ((Array.length aaa) - 1)
      do
        answer.(n) <- int_seq.(n)
      done
      ;
      answer
    )
      in
      let local_product int_seq =
  Array.fold_left (fun accu a -> product accu a)
    (full ~d:0 ())
    (Array.mapi (fun n k -> aaa.(n).((max (k-1) 0))) int_seq) (* ici ? *)
      in
      let relative_level = Array.fold_left (fun accu x -> accu-x) level (Array.init (Array.length aaa) ground)
      in
      let accumulator = ref (empty ~d:dim ())
      in
  (
    if (relative_level > sum_till ((Array.length aaa)-1))
    then
      !accumulator
    else
      (
        let current = ref (initial_sequence relative_level)
        in
    (
      try
        while true
        do
          accumulator := join !accumulator (local_product (!current))
          ;
          current := next_sequence !current
        done
      with
                    | Invalid_argument "index out of bounds" -> ()
    )
    ;
    !accumulator
      )
  )
*)


let generalized_product = AC.generalized_product
let levelwise_product ?u ?g level aaa = AC.levelwise_product ?g level aaa
let locus_higher_than = AC.locus_higher_than
let locus_lower_than = AC.locus_lower_than


    let print a =
      try
  let a_cube = Soc.choose a
  in
  let a' = Soc.remove a_cube a
  in
  let aux c = print_string ("| "^(T.string_of c)^"\n")
  in
    ((print_string ("  "^(T.string_of a_cube)^"\n")) ; (Soc.iter aux a'))
      with
  | Not_found -> print_string "The empty set of cubes does not represent any area"

(*

   Les fonctions qui suivent sont requises par la signature Area.S,
   cependant, afin d'obtenir une signature commune aux modules
   Area.Make.OverCube et Area.Make.OverTore, il faudra regarder de
   près celles qui sont utiles et celles qui le sont moins.

  En fait on va mettre les particularités à part.

*)

    let upward a = failwith "upward is pointless in a torus"
    let upper_corners_area a = failwith "upper_corners_area is pointless in a torus"
    let terminal lf lb = of_cube (T.terminal lf lb)
    let strictly_below n r a = failwith "strictly_below is pointless in a torus"
    let strictly_above n r a = failwith "strictly_above is pointless in a torus"



    let belongs_to p a =
      Soc.exists (fun c -> T.belongs_to p c) a


    let belongs_to_closure p a =
      Soc.exists (fun c -> T.belongs_to p (T.closure c)) a


    let deadlocks a =
      try
  let d = dimension a
  in
    (
      Soc.fold
        (
    fun c accu ->
      try
        let p = T.lub c
        in
          (
      if
        (
          (
            (not(belongs_to p a))
            ||
        (
          let aux = of_cube (T.atom p)
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
          (not(T.belongs_to p (T.closure c'))) ||
            (
              try 
                (T.lub c' = p) 
              with 
                | _ -> false
            ) ||
            (not(T.in_touch c c'))
              )
          ) 
          a
            )
        )
      then
        Soc.add (T.atom p) accu
      else
        accu
          )
      with
        | _ -> accu
        )
        a
        (empty ~d:d ())
    )
      with
  | Not_found -> failwith "deadlocks : the empty set does not represent any area"

    let sources a =
      try
  let d = dimension a
  in
    (
      Soc.fold
        (
    fun c accu ->
      try
        let p = T.glb c
        in
          (
      if 
        (
          (
            (not(belongs_to p a))
            ||
        (
          let aux = of_cube (T.atom p)
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
          (not(T.belongs_to p (T.closure c'))) ||
            (
              try 
                (T.glb c' = p) 
              with 
                | _ -> false
            ) ||
            (not(T.in_touch c c'))
              )
          ) 
          a
            )
        )
      then
        Soc.add (T.atom p) accu
      else
        accu
          )
      with
        | _ -> accu
        )
        a
        (empty ~d:d ())
    )
      with
  | Not_found -> failwith "sources : the empty set does not represent any area"


    let might_go_infinity a =
      try
  let d = T.dimension (Soc.choose a)
  in
  let unbounded_sub_area =
    Soc.fold
      (
        fun c accu ->
    try
      let _ = T.lub c
      in
        accu
    with
      | T.Undefined -> (Soc.add c accu)
      )
      a
      (empty ~d:d ())
  in
    past_cone unbounded_sub_area a
      with
  | Not_found -> failwith "might_come_to_infinity : the empty set does not represent any area"

    let deadlock_attractor a = difference a (might_go_infinity a)

    let might_go_deadlock a = past_cone (deadlock_attractor a) a

    let infinity_attractor a = difference a (might_go_deadlock a)

    let common a =
      let c = Soc.choose a
      in
        Soc.fold (fun x accu -> (meet (of_cube x) accu)) (Soc.remove c a) (of_cube c)

    (* Il y a un problème dû au fait que l'intersection renvoie une liste plutôt qu'un cube *)

    let generalized_common ?d a = match d with
      | Some n -> (
          let coa = (Soc.cardinal a) - 1 in
          let () = assert(n <= coa) in
          let aux = Array.of_list (Soc.elements a) in
          let current = ref (Overall.List.initial_segment n) in
          let answer = ref (empty ~d:(dimension a) ()) in
          let () = 
            try
              while true do
                let () = 
                  answer := join
                  (List.fold_left (
                    fun accu i -> meet (of_cube aux.(i)) accu) 
                    (of_cube (T.full (dimension a))) 
                    !current)
                  !answer in
                current := Algebra.SequencesOfIntegers.next_list !current coa
              done
            with Not_found -> () in
          !answer)
      | None -> common a


   let in_touch a1 a2 = 
     (is_not_empty(meet(closure a1)a2)) || (is_not_empty(meet(closure a2)a1))

   module FGSL = Algebra.Free.Graded.SemiLattice
     (
       struct 
   type t = T.arc
   let compare = fun i -> T.compare_arc i 
   let string_of = T.string_of_arc
       end
     )


    let departure a =
      Soc.fold
  (
    fun c accu ->
      if Soc.for_all (fun c' -> T.is_included (T.in_the_past_of c c') c) a
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
      if Soc.for_all (fun c' -> T.is_included (T.in_the_future_of c c') c) a
      then
        Soc.add c accu
      else
        accu
  ) 
  a
  Soc.empty

    let connected_component c a =
      let rec aux accu has_increased =
  if has_increased
  then
    let augmented_accu =
      (
        Soc.fold
    (
      fun c local_accu ->
        (Soc.union local_accu (Soc.filter (fun x -> T.in_touch c x) a))
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

    let connected a = ((connected_component (Soc.choose a) a) = a)


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
      let answer = Array.make (Soc.cardinal a) (T.empty (dimension a)) in
      let i = ref (-1) in
  Soc.iter (fun c -> (incr i ; Array.set answer !i c)) a ;
  answer

    let factorize ?(unpacked=true) a = FGSL.factorize ~unpacked:true (Soc.fold (fun c accu -> FGSL.add (T.to_array c) accu) a FGSL.zero)
    let reachable a = failwith "reachable is not implemented yet"
    let min_elt = Soc.min_elt
    let mem = Soc.mem
    let local_deadlock_attractor a = failwith "local_deadlock_attractor is not implemented yet"
    let local_might_go_infinity a = failwith "local_might_go_infinity is not implemented yet"
    let lub a = failwith "lub is pointless in a torus"
    let iter = Soc.iter
    let initial rf rb = of_cube (T.initial rf rb)
    let glb a = failwith "glb is pointless in a torus"
    let ginzu a = failwith "ginzu is not implemented yet"
    let fold = Soc.fold
    let filter = Soc.filter
    let face b k a = failwith "face is not implemented yet"
    let exists = Soc.exists
    let downward a = failwith "downward is pointless in a torus"
    let cubical_order_convex a = failwith "cubical_order_convex is pointless in a torus"
    let cubic_hull a = failwith "cubic_hull is pointless in a torus"
    let complement_of_cube c = make (T.complement c)
    let compare = Soc.compare
      (* let common a = failwith "common is not implemented yet" *)
      (* let generalized_common ?d a = failwith "generalized_common is
   not implemented yet" *)
    let bounded lf rf lb rb = of_cube (T.bounded lf rf lb rb)
    let belongs_to_closure p a = failwith "belongs_to_closure is not implemented yet"
    let below n r a = failwith "below is pointless in a torus"
    let above n r a = failwith "above is pointless in a torus"
    let base a = failwith "base is not implemented yet"
    let cset2D_of_complement_of_area a = failwith "cset2D_of_complement_of_area is not implemented yet"

  end (* Make *)
