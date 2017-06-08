(***********************************************************************)
(*                                                                     *)
(*                               GMOD                                  *)
(*                                                                     *)
(*                           AreaOverCube.ml                           *)
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

module type S =
sig
  include Area.S
  val above: int -> regular_value -> t -> t
  val below: int -> regular_value -> t -> t
  val strictly_above: int -> regular_value -> t -> t
  val strictly_below: int -> regular_value -> t -> t
  val cubic_hull: t -> brick
  val departure: t -> t
  val arrival: t -> t
  val future_extension: brick -> t -> t
  val past_extension: brick -> t -> t
  val future_neighborhood: brick -> t -> t
  val past_neighborhood: brick -> t -> t
  val cubical_order_convex: t -> t
  val deadlock_attractor_weak: t -> t
  val might_go_infinity_in_all_dimensions: t -> t
  val cech_simplex: int -> brick array -> (int list * brick) list
  val cech_complex: t -> (int list * brick) list array
  val cech_complex_dimension: (int list * brick) list array -> int
end

module Make(Overall:In)(C:Cube.S with type ints = Overall.IntSet.t) =
  struct

    exception Undefined

    module Soc = Set.Make(C)

    module Points =
    struct
      type t = C.point
      let compare = C.compare_points
    end

    module Sop = Set.Make(Points)

    type regular_value = C.regular_value 
    type point = C.point 
    type generator = C.interval 
    type brick = C.t

    type ints = Overall.IntSet.t

    type t = Soc.t

    type underlying = C.underlying

    let exists   = Soc.exists
    let add      = Soc.add
    let cardinal = Soc.cardinal
    let mem      = Soc.mem

    let choose = Soc.choose
    let remove = Soc.remove

    let underlying a =
      try C.underlying (Soc.choose a)
      with Not_found -> failwith "Warning: the empty set does not represent any cubical area (Area.underlying)."

    let dimension a =
      try C.dimension (Soc.choose a)
      with Not_found -> failwith "Warning: the empty set does not represent any cubical area (Area.dimension)."

    let next c a =
      let rc = ref c in
      let f c' =
        if (C.compare c c') < 0 && (C.compare c' !rc) < 0
        then rc := c' in
      let () = Soc.iter f a in
      !rc

    let min_elt = Soc.min_elt

    let compare = Soc.compare

    let of_cube c = Soc.singleton c

    let initial f b = of_cube (C.initial f b)

    let terminal f b = of_cube (C.terminal f b)

    let bounded lf rf lb rb = of_cube (C.bounded lf rf lb rb)

    let empty ?u ?(d=0) () = of_cube (C.empty d)

    let zero = empty ()

    let full ?u ?(d=0) () = of_cube (C.full d)

    let one = full ()

    let atom ?u p = of_cube (C.atom p)

    let remove_useless_empty_brick a =
      let a' = Soc.remove (C.empty (dimension a)) a in
      if a' <> Soc.empty then a' else a

    let belongs_to p a =
      Soc.exists (fun c -> C.belongs_to p c) a

    let belongs_to_closure p a =
      Soc.exists (fun c -> C.belongs_to p (C.closure c)) a

    (* Warning: this function returns true if the argument is the
       empty set, it would be wiser to make it raise an exception. *)

    let is_empty a = Soc.for_all C.is_empty a

    let is_not_empty a = Soc.exists C.is_not_empty a

    (* Warning : for the following functions, the arguments must be
       pre-normal forms. *)

    let is_full a = Soc.exists C.is_full a

    let is_not_full a = Soc.for_all C.is_not_full a



    let make cl =
      let answer = List.fold_right (fun c accu -> (Soc.add c accu)) cl Soc.empty in
      if answer <> Soc.empty
      then answer
      else raise (Invalid_argument "make : the empty set does not represent a cubical area")

    let of_list cl =
      let answer = List.fold_right (fun c accu -> (Soc.add c accu)) cl Soc.empty in
      if answer <> Soc.empty
      then answer
      else raise (Invalid_argument "make : the empty set does not represent a cubical area")

    let to_list a = Soc.elements a

    (* let to_array a = Soc.fold (fun c accu -> Array.append accu [|c|]) a [||] *)

    let to_array a = 
      let answer = Array.make (Soc.cardinal a) (C.empty (dimension a)) in
      let i = ref (-1) in
      let () = Soc.iter (fun c -> incr i; Array.set answer !i c) a in
      answer

    let of_array ca = 
      let answer = Array.fold_right (fun c accu -> Soc.add c accu) ca Soc.empty in
      if answer <> Soc.empty
      then answer
      else raise (Invalid_argument "make : the empty set does not represent a cubical area")

    let belongs_to p a = Soc.exists (fun c -> C.belongs_to p c) a

    let cubic_hull a =
      if a <> Soc.empty
      then
        let d = C.dimension (Soc.choose a) in
        Soc.fold (C.join) a (C.empty d)
      else raise (Invalid_argument "cubic hull : the empty set does not represent a cubical area")

    (*-----------------------------------------------------------------

      Si (Ai)_i et (Bj)_j sont respectivement les familles des cubes
      maximaux des régions A et B, alors la famille (A_i^B_j)_{i,j}
      contient la famille des cubes maximaux de l'intersection A^B.

      ------------------------------------------------------------------*)

    (*

       La version 3 du calcul de l'intersection renvoie une forme normalisée
       pourvue que les formes a1 et a2 le soient. Cette fonction est
       optimisée pour préserver l'espace mais sa complexité est "à peu près
       cubique" au sens où elle "imbrique" trois boucles de récursion avec la
       fonction "iter". Au contraire, les versions 1 et 2 préservent le temps
       puisqu'elles ne sont "qu'à peu près quadratique".

    *)

    (* Il faut tout de même remarquer que la version 3 limite la croissance de "answer"     *)
    (* ce qui a aussi pour effet de limiter le temps. Dans un cas où il y aurait beaucoup   *)
    (* de redondance, il se pourrait que la version 3 soit plus rapide que les deux autres. *)

    (* Enfin, pour que la version 3 renvoie une forme normalisée, il suffit que a1 et a2    *)
    (* contiennent tous les sous-cubes maximaux des régions qu'ils recouvrent.              *)

    let meet a1 a2 =
      if a1 = Soc.empty || a2 = Soc.empty
      then raise (Invalid_argument "intersection : the empty set does not represent a cubical area")
      else
        let answer = ref (of_cube(C.empty (dimension a1))) in
        let must_be_added = ref true in
        let add_intersection_to_answer c1 c2 =
          let c = C.meet c1 c2 in (* and must_be_added = ref true *)
          let scanner elt_answer =
            if C.is_included elt_answer c
            then answer := Soc.remove elt_answer !answer
            else
              if C.is_included c elt_answer
              then must_be_added := false in
          let () = Soc.iter scanner !answer in
          let () = if !must_be_added
            then answer := Soc.add c !answer in
          must_be_added := true in
        let f c1 = Soc.iter (add_intersection_to_answer c1) a2 in
        let () = Soc.iter f a1 in
        !answer

    let rec meet_list l = match l with
      | [x] -> x
      | x :: l -> meet x (meet_list l)
      | [] -> raise Undefined
      
    let meet_array a = match a with
      | [||] -> raise Undefined
      | _ -> Array.fold_left (fun accu x -> meet accu x) a.(0) Array.(sub a 1 (pred (length a)))

    (* la forme proposée est "normalisée" *)

    let common a =
      let c = Soc.choose a in
      of_cube (Soc.fold C.meet (Soc.remove c a) c)

    (*

       La fonction generalized_common renvoie la réunion de toutes les
       intersection par paquets de n blocks, où n est la valeur passée
       en argument à l'étiquette d. Cette fonction est principalement
       utilisée par le calcul d'une région interdite produite par des
       barrières de synchronisation.

    *)

    (* Une autre version de la fonction common qui permet de faire des
       intersections "par paquets" *)

    (* Attention, la valeur retournée n'est pas forcément en forme
       normale *)

    (* Bug corrigé *)

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
                  answer := Soc.add
                  (List.fold_left (
                    fun accu i -> C.meet aux.(i) accu) 
                    (C.full (dimension a)) 
                    !current)
                  !answer in
                current := Algebra.SequencesOfIntegers.next_list !current coa
              done
            with Not_found -> () in
          !answer)
      | None -> common a




(*
    let print_seq s = List.iter (Printf.printf "%i ") s ; print_endline ""
*)




    (*Čech simplex of dimension n*)

    let cech_simplex n maximal_cubes_array =
      let d =
        if maximal_cubes_array <> [||]
        then C.dimension (maximal_cubes_array.(0))
        else invalid_arg "cech_simplex" in
      let full_brick = C.full d in
      let current = ref (Overall.List.initial_segment n) in
      let answer = ref [] in
      let coa = (Array.length maximal_cubes_array) - 1 in
       try
        let () =
          try
            while true do
              let entry =
                !current , 
                (List.fold_left (
                  fun accu i -> 
                    if i <= coa then C.meet maximal_cubes_array.(i) accu
                    else raise Exit)
                  full_brick 
                  !current) in
              let () = if C.is_not_empty (snd entry) then (answer := entry :: !answer) in
              current := Algebra.SequencesOfIntegers.next_list !current coa
            done
          with Not_found -> () in
        !answer
      with Exit -> []



    let cech_complex a =
      let maximal_cubes_array = Array.of_list (Soc.elements a) in
      let answer = ref [||] in
      let rec make n =
        let ncs = cech_simplex n maximal_cubes_array in
        if ncs <> []
        then (answer := (Array.append !answer [|ncs|]) ; make (n + 1))
        else raise Exit in
      try make 1 with Exit -> !answer
    
    let cech_complex_dimension cc =
      try C.dimension (snd (List.hd cc.(0)))
      with _ -> invalid_arg "cech_complex_dimension"

    module Concur98 = struct

      (* But : déterminer le bassin d'attraction des deadlocks sans
   calculer explicitement le complémentaire de la région
   interdite. C'est possible d'après l'algorithme de Fajstrup,
   Raussen et Goubault [Concur 98] mais il faut tenir compte de
   quelques subtilités avec les ouverts et les fermés ainsi que
   les situations "non génériques". *)

      (* calculation of the deadlock attractor by means of the
   Concur98 algorithm. The forbidden area is taken as an
   input. The algorithm is not valid in general. However I think
   it is when the covering of the forbidden area satisfies the
   following property:

   Two cubes which are connected actually meets.

   In particular this property is satisfied when all the cubes
   of the covering are closed, which implies that the forbidden
   area is closed

      *)


      (* 

   L'intersection de d cubes ne doit pas être vide.

   La borne inférieure de l'intersection doit appartenir à une unique
   face inférieure de chaque cube qui a engendré l'intersection.

   L'ensemble des indices des faces inférieurs auxquelles appartiennent
   la borne inférieure doit recouvrir toutes les dimensions.

   un peu compliqué tout ça et en plus ça ne donne pas l'attracteur le
   plus proche

   Une autre façon de faire : on calcule la borne inférieures des
   bornes inférieures des cubes qui provoque l'intersection...non ce
   n'est pas ça.

      *)

      (* d:int p:point c:cube *)

      (* Return the unique back face of the cube that contains p, if it exists
   and if it is the only one.  Otherwise it raises the exception
   Undefined. This function should be put in the file cube.ml *)

      let find_the_unique_casted_shadow_before d p c =
  let face_found = ref None in
  let i = ref 0 in
    while !i<d
    do
      let face = C.front !i c in
        (
    if C.belongs_to p face
    then 
      if !face_found = None (* we have not found a face that contains p yet. *)
      then face_found := Some (C.cast_shadow_before !i c)
      else raise Undefined (* there is at least two faces containing p. *)
        )
        ;
        incr i
    done
    ;
    (
      match !face_found with
        | Some face -> face (* We have found the unique face that contains p. *)
        | None -> raise Undefined (* There is no face containing p. *)
    )

      let candidate d aoc = 
  let intersection = Array.fold_left (fun accu c -> C.meet accu c) (C.full d) aoc 
  in 
    if C.is_not_empty intersection 
    then 
      let p = C.glb intersection in 
        try
    Array.fold_left 
      (fun accu c -> C.meet c accu)
      (C.full d) 
      (Array.map (fun c -> find_the_unique_casted_shadow_before d p c) aoc) 
        with
    | Undefined -> C.empty d
    else
      C.empty d

      let next_wave aoc = 
  let d = C.dimension aoc.(0) in
  let current = ref (Overall.List.initial_segment d) in
  let i_max = List.length !current - 1 in
  let answer = ref [||] (*empty ~d(*:(dimension a)*) ()*) in
  let coa = (Array.length aoc)-1 in
    try
      (
        (
    try
      while true
      do
        let aux_array = Array.of_list !current in
        let x = C.closure 
        (candidate d (Array.init d 
          (fun i -> if i <= i_max then aoc.(aux_array.(i)) else raise Exit))) in
          if (C.is_not_empty x) && (Overall.Array.for_all (fun c -> (x<>c)) aoc)
          then answer := Array.append [|x|] !answer
          else ()
          ;
          current := Algebra.SequencesOfIntegers.next_list !current coa
      done
    with Not_found -> ()
        )
        ; !answer
      )
    with
      | Exit -> [||]

      let algorithm aoc = 
  let current = ref [||] in
  let next = ref (next_wave aoc) in
    while !next <> [||]
    do
      current := Array.append !current !next ;
      next := next_wave !current
    done
    ;
    !current

    end (* Concur98 *)

    let deadlock_attractor_weak a = 
      let answer = Concur98.algorithm (to_array a) in
  if answer <> [||]
  then
    of_array answer
  else
    empty ~d:(dimension a) ()

    (* Return the normal form of the complement of the
       argument. *)

    let complement_of_cube c = make (C.complement c)

    let dig_cube_out_of_area c a = meet (complement_of_cube c) a 

    let dig_out_of a1 a2 = Soc.fold (dig_cube_out_of_area) a1 a2 

    (* Si la fonction meet sur laquelle complement est construite vérifie la propriété suivante : *)
    (* a1 a2 contiennent tous les sous-cubes maximaux des régions qu'ils recouvrent *)
    (* => meet a1 a2 est normalisée, alors complement a est normalisé même si a ne l'est pas. *)
      
    let complement a =
      try   
        let d = C.dimension (Soc.choose a) in
        let f c accu = meet (complement_of_cube c) accu in
        Soc.fold f a (of_cube (C.full d)) 
      with Not_found -> raise (Invalid_argument "complement : the empty set does not represent a cubical area")

    let difference a1 a2 = meet a1 (complement a2)

    (* 
       la fonction normalize est basée sur les propriétés de normalisation de
       la fonction complement.
    *)

    let normalize a = complement (complement a) 

    (*Provide the least representation. Very expensive operation. Should only be use for display*)
    let minimize a =
      let mandatory c = is_not_empty (difference (of_cube c) (Soc.remove c a)) in
      Soc.filter mandatory a

    
    (* Warning : the soundness of the result depends on the fact that
       the second argument a2 be a pre-normal form. *)

    let is_included a1 a2 = 
      let test c1 = Soc.exists (C.is_included c1) a2 in
      Soc.for_all test a1 

    let above k x a = Soc.fold (fun c accu -> Soc.add (C.above k x c) accu) a Soc.empty  

    let below k x a = Soc.fold (fun c accu -> Soc.add (C.below k x c) accu) a Soc.empty  

    let strictly_above k x a = Soc.fold (fun c accu -> Soc.add (C.strictly_above k x c) accu) a Soc.empty  

    let strictly_below k x a = Soc.fold (fun c accu -> Soc.add (C.strictly_below k x c) accu) a Soc.empty  

    (* Warning : the function join do not preserve the (pre-)normal forms. *)

    let join a1 a2 = Soc.union a1 a2

    let rec join_list l = match l with
      | [x] -> x
      | x :: l -> join x (join_list l)
      | [] -> raise Undefined
      
    let join_array a = match a with
      | [||] -> raise Undefined
      | _ -> Array.fold_left (fun accu x -> join accu x) a.(0) Array.(sub a 1 (pred (length a)))





    let string_of ?(gui=true) a = 
      try
    let a_cube = Soc.choose a in
    let a' = Soc.remove a_cube a in
      if gui 
      then
        let aux c s = ((C.string_of c)^" \n| "^s) in
      Soc.fold aux a' (C.string_of a_cube)
      else
        let aux c s = (s^" \n| "^(C.string_of c)) in
      Soc.fold aux a' (C.string_of a_cube)
      with Not_found -> "string_of : the empty set does not represent any area"

    (* alternative version for debugging, to be removed *)

    let string_of ?(gui=true) a = String.concat " " (List.map C.string_of (Soc.elements a))

    let print a =
      try
    let a_cube = Soc.choose a in
    let a' = Soc.remove a_cube a in
    let aux c = print_string ("| "^(C.string_of c)^"\n") in
    ((print_string ("  "^(C.string_of a_cube)^"\n")) ; (Soc.iter aux a'))
      with Not_found -> print_string "The empty set of cubes does not represent any area"

    let future_cone departure a =
      try
  let d = C.dimension (Soc.choose a) in
  let current_wave = ref departure in
  let global_answer = ref (empty ~d:d ()) in
  let next_wave w =
    let local_answer = ref (empty ~d:d ()) in
    (
      (
        Soc.iter
      (fun cw ->
        Soc.iter
         (fun ca ->
         if (C.in_touch cw ca) 
         then local_answer := Soc.add (C.in_the_future_of cw ca) !local_answer)
      a)
      w
        );
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
      with Not_found -> failwith "future_cone : the empty set does not represent any area"







    let downward a = Soc.fold (fun c accu -> Soc.add (C.downward c) accu) a Soc.empty

    let upward a = Soc.fold (fun c accu -> Soc.add (C.upward c) accu) a Soc.empty

    let closure a = Soc.fold (fun c accu -> Soc.add (C.closure c) accu) a Soc.empty

    let cubical_order_convex a = meet (upward a) (downward a)

    (* Warning : the function interior requires its argument be a
       pre-normal form. *)

    let interior a = Soc.fold (fun c accu -> Soc.add (C.interior c) accu) a Soc.empty

    let boundary a = difference (closure a) (interior a)

    let in_touch a1 a2 = 
      (is_not_empty(meet(closure a1)a2)) || (is_not_empty(meet(closure a2)a1))

    (* Une version plus efficace qui transforme une forme pré-normale
       en forme normale. *)

    let compress a = Soc.filter (fun c -> (Soc.for_all (fun c' -> (not(C.is_included c c'))||(c=c')) a)) a

    (* Doit-être utilisé avec a normalisé et topologiquement fermé ! *)
    (* Attention, ces fonctions sont fausses. *)

    let upper_corners_area a = 
      let f c accu =
        try Soc.add (C.atom (C.lub c)) accu
        with C.Undefined -> accu in
      Soc.fold f a Soc.empty

    let upper_corners_set a =
      let aux c accu =
        try Sop.add (C.lub c) accu
        with C.Undefined -> accu in
      Soc.fold aux a Sop.empty

    let upper_corners_list a = 
      let f c accu =
        try (C.glb c) :: accu
        with C.Undefined -> accu in
      Soc.fold f a []

    (* The argument of the function product should be (pre-)normal
       forms, if it is so the result is still a (pre-)normal form. *)

    let product a1 a2 =
      let g c1 c2 answer = Soc.add (C.product c1 c2) answer in
      let f c1 answer = Soc.fold (g c1) a2 answer in
      Soc.fold f a1 Soc.empty

    let rec product_list l = match l with
      | x :: l -> product x (product_list l)
      | [] -> one
      
    let product_array a = Array.fold_left (fun accu x -> product accu x) one a

    let rec product_fun f a b = if a > b then one else product (f a) (product_fun f (succ a) b)

    let rec product_fun_left f a b = if a > b then one else product (f a) (product_fun_left f (succ a) b)

    let rec product_fun_right f a b = if a > b then one else product (product_fun_right f a (pred b)) (f b)

    let rec exponent a n =
      if n <= 0 
      then of_cube (C.full 0)
      else product a (exponent a (n - 1))

    (*------------------------------------------------------------------------------------------------*)
    (*                                   generalized_product                                          *)
    (*------------------------------------------------------------------------------------------------*)

    (* The function generalized_product should be thought of as a kind
       of exterior derivate. *)
    
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

    module FGSL = Algebra.Free.Graded.SemiLattice
      (
        struct 
          type t = C.interval
          let compare = fun i -> C.compare_interval i 
          let string_of = C.string_of_interval
        end
      )

    let factorize ?(unpacked=true) a =
      FGSL.factorize ~unpacked:true (Soc.fold (fun c accu -> FGSL.add (C.to_array c) accu) a FGSL.zero)

    (* Attention, la fonction "levelwise_product" n'a pas encore été
       testée *)

    (* La fonction "levelwise_product" a été testée et semble bien
       fonctionner. *)

    (* L'argument "aaa" est un tableau de tableaux de régions linéaires
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

    (* En fait il est peut-être plus judicieux de considérer que la
       première case du tableau contient le lieu des points dont
       l'image est nulle. On peut alors travailler avec les fonctions
       partielles.*)


    (* Cette adaptation permet de gérer des fonctions dont la valeur est
       négative et des tableaux dont les cases contiennent des valeurs de
       dimension supérieure ou égale à 1. *)

    (* Il y a encore pas mal de boulot à faire sur cette fonction...  *)

    (* Pour la ligne d'indice n, la case d'indice k contient
       l'ensemble des points x tets que f_k(x)=(k+(lvl_min_col
       n)). Ainsi les valeurs négatives sont permises. *)



    (* L'argument "aaa" est un tableau de tableaux de régions dont
       chaque ligne est homogène en dimension. A chaque ligne n est
       attribuée une fonction en escalier f_n définie sur la réunion
       des régions contenues dans les cases de cette ligne. Etant
       donnée une ligne d'indice n, la k-ème case contient le lieu des
       points de la région pour lesquels la valeur de la fonction est
       k+(bottom_value n) (ou k+(bottom_value n)-1 je ne sais plus
       trop). En particulier, les régions contenues dans les cases de
       la ligne forment une partition de l'ensemble de définition de
       f_n.

       L'indexation des tableaux en OCaml fait que la première case
       porte l'indice 0. En supposant que la matrice possède n lignes
       et que l'on note f_0,...,f_{n-1} les fonctions qui
       correspondent aux différents lignes de la matrice, on associe à
       chaque point x du produit des domaines de définition des
       fonction f_0,...,f_{n-1} la somme des valeurs de ces fonctions
       au point x, c'est-à-dire f_0(x)+...+f_{n-1}(x). L'appel à la
       fonction avec l'argument "level" calcul alors le lieu des
       points où cette somme est supérieure ou égale à la valeur
       "level".

       La réalisation de cette fonction nécessite un algorithme
       d'énumération des décompostions d'un entier positif en somme
       d'entiers positifs. 

       (Array.length aaa) représente le nombre de fonctions
       c'est-à-dire n avec les notations précédentes.

       (initial_sequence level) renvoie un tableau d'entiers positifs
       de même longueur que (aaa) dont la somme vaut level et tel que
       
       1) si la k-ème case est "pleine" c'est-à-dire contient un
       entier égal à la longueur de la k-ème ligne de (aaa), alors il
       en va de même pour toutes les cases d'incide k' avec k'<k

       2) si la k-ème case est "vide" c'est-à-dire contient l'entier
       0, alors il en va de même pour toutes les cases d'incide k'
       avec k<k'

       Ces deux conditions ne laissent au plus qu'un seul tableau
       possible. Dans le cas où ce un tel tableau n'existe pas, c'est
       celui dont la somme des éléments est la plus proche de level,
       tout en étant alors nécessairement inférieure.

    *)

    (* tests 6, 7, 8 et 9 passés avec succès *)

    (* Il y a peut-être un problème avec les cas "limites" *)

    (* Cette fonction passe en revue *)

    let generalized_product = AC.generalized_product

    let levelwise_product ?u ?g level aaa = AC.levelwise_product ?g level aaa

    let locus_higher_than = AC.locus_higher_than
    
    let locus_lower_than = AC.locus_lower_than

    let future_cone_point p a = future_cone (atom p) a


    let past_cone arrival a =
      try
    let d = C.dimension (Soc.choose a) in
    let current_wave = ref arrival in
    let global_answer = ref (empty ~d:d ()) in
    let next_wave w =
      let local_answer = ref (empty ~d:d ()) in
        (
          (
        Soc.iter
          (fun cw -> Soc.iter
          (fun ca -> if C.in_touch cw ca
             then local_answer := Soc.add (C.in_the_past_of cw ca) !local_answer)
          a)
        w
          );
          !local_answer
        )
    in
      (
        while not(is_included !current_wave !global_answer)
        do
          global_answer := join !global_answer !current_wave ;
          current_wave := next_wave !current_wave
        done;
        !global_answer
      )
      with Not_found -> failwith "past_cone : the empty set does not represent any area"

    let past_cone_point p a = past_cone (atom p) a

  let coinitial a1 a2 = let b = meet a1 a2 in
  (is_included a1 (future_cone b a1)) && (is_included a2 (future_cone b a2))
    
    let cofinal a1 a2 = let b = meet a1 a2 in
    (is_included a1 (past_cone b a1)) && (is_included a2 (past_cone b a2))

    (*

      Je n'ai actuellement pas de contre-exemple à la fonction
      deadlocks, cependant, je n'ai pas de preuve mathématique de
      l'algorithme que j'ai implanté non plus...à suivre donc...ben
      justement il y a peut-être un (petit) problème... cf. test10

    *)

    let deadlocks a =
      try
  let d = dimension a
  in
    (
      Soc.fold
        (
    fun c accu ->
      try
        let p = C.lub c
        in
          (
      if
        (
          (
            (not(belongs_to p a))
            ||
        (
          let aux = of_cube (C.atom p)
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
          (not(C.belongs_to p (C.closure c'))) ||
            (
              try 
                (C.lub c' = p) 
              with 
                | _ -> false
            ) ||
            (not(C.in_touch c c'))
              )
          ) 
          a
            )
        )
      then
        Soc.add (C.atom p) accu
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
        let p = C.glb c
        in
          (
      if 
        (
          (
            (not(belongs_to p a))
            ||
        (
          let aux = of_cube (C.atom p)
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
          (not(C.belongs_to p (C.closure c'))) ||
            (
              try 
                (C.glb c' = p) 
              with 
                | _ -> false
            ) ||
            (not(C.in_touch c c'))
              )
          ) 
          a
            )
        )
      then
        Soc.add (C.atom p) accu
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

    (* The argument should be a normal form. *)

    (* Les fonctions departure et arrival ne donnent pas exactement ce
       que l'on attend. Elles sont en effet directement liées à la
       fonction dihomotopy_classes_version_3. Des corrections ont été
       apportées et ça semble fonctionner maintenant. *)

    let departure a = Soc.filter 
      (
  fun c -> Soc.for_all (fun c' -> (C.is_included (C.in_the_past_of c c') c) || (not ((C.in_the_future_of c' c) = c))) a
      ) a

    let arrival a = Soc.filter 
      (
  fun c -> Soc.for_all (fun c' -> (C.is_included (C.in_the_future_of c c') c) || (not ((C.in_the_past_of c' c) = c))) a
      ) a

    let future_extension c a = Soc.filter (fun c' -> not (C.is_included (C.in_the_future_of c c') c)) a

    let past_extension c a = Soc.filter (fun c' -> not (C.is_included (C.in_the_past_of c c') c)) a

    let future_neighborhood c a = Soc.fold
      (
  fun c' accu -> 
    let e = C.in_the_future_of c c' 
    in 
      if not (C.is_included e c) then Soc.add e accu else accu
      ) a (empty ~d:(dimension a) ())

    let past_neighborhood c a = Soc.fold
      (
  fun c' accu ->
    let e = C.in_the_past_of c c'
    in
      if not (C.is_included e c) then Soc.add e accu else accu
      ) a (empty ~d:(dimension a) ())

    let reachable a =
      try
  let d = dimension a
  in
    future_cone_point (C.origin d) a
      with
  | Not_found -> failwith "reachable : the empty set does not represent any area"

    (*
      La fonction past_cone prend deux régions cubiques a1 et a2 en arguments
      et elle retourne l'ensemble des points de a2 à partir desquels on peut
      atteindre a1 en suivant un chemin continue et croissant.

      La fonction past_cone est actuellement buggée,
      sur le test suivant

      a = {1}*[0,-[ | [0,-[*{1}
      b = a / {1}*{1}
      d = no.(b -> ({1}*]2,-[ | ]2,-[*{1}))

      on obtient la réponse suivante :

      a = [0,-[x{1} | {1}x[0,-[
      b = [0,1[x{1} | ]1,-[x{1} | {1}x[0,1[ | {1}x]1,-[
      c = [0,1[x{1} | ]1,-[x{1} | {1}x[0,1[ | {1}x]1,-[
      d = [0,1[x{1} | ]1,-[x{1} | {1}x[0,1[ | {1}x]1,-[

      alors qu'elle devrait être pour c et d :

      c = ]1,-[x{1} | {1}x]1,-[
      d = ]1,-[x{1} | {1}x]1,-[

      NB : la fonction might_go_to_infinity est basée sur la fonction
      past_cone.

      NB : la fonction past_cone est basée sur la fonction in_touch du
      module Cube qui vient d'être débuggée.

      Mais il semble que cette fonction ait encore un problème puisqu'elle
      renvoie : prendre arrival = {0} et a = {1} (ou l'inverse).

      ATTENTION : il semble qu'il faille que arrival soit incluse dans a
      pour que la fonction soit correcte.

    *)

    (*

      Sous l'hypothèse que la région cubique "arrival" soit incluse dans
      la région cubique "a", la région cubique "past_cone arrival a" est
      l'ensemble des points de la région cubique de "a" à partir desquels
      on peut atteindre (l'un des points de) la région cubique "arrival".

      Le nom que porte cette fonction est dû au fait que si l'on note C la
      categorie fondamentale de la région cubique "a", "past_cone arrival
      a" est aussi l'ensemble des objets x de C tels qu'il existe un objet
      y de "arrival" tel que C[x,y] ne soit pas vide. En particulier, la
      détermination du cone passé (ainsi que celle du cone futur)
      constitue la première étape de la détermination de la catégorie des
      composantes de C.

    *)

    (* Cette fonction n'apparaît pas dans la signature, elle est surtout mise
      ici comme un "pense-bête".*)

    let might_go_infinity a =
      try
  let d = C.dimension (Soc.choose a)
  in
  let unbounded_sub_area =
    Soc.fold
      (
        fun c accu ->
    try
      let _ = C.lub c
      in
        accu
    with
      | C.Undefined -> (Soc.add c accu)
      )
      a
      (empty ~d:d ())
  in
    past_cone unbounded_sub_area a
      with
  | Not_found -> failwith "might_come_to_infinity : the empty set does not represent any area"


    let might_go_infinity_in_all_dimensions a = 
      try
  let d = C.dimension (Soc.choose a)
  in
  let unbounded_sub_area =
    Soc.fold
      (
        fun c accu ->
    if (C.downward c) = (C.full d)
    then 
      Soc.add c accu
    else
      accu
      )
      a
      (empty ~d:d ())
  in
    past_cone unbounded_sub_area a
      with
  | Not_found -> failwith "might_come_to_infinity_in_all_dimensions : the empty set does not represent any area"


    let deadlock_attractor a = difference a (might_go_infinity a)

    let might_go_deadlock a = past_cone (deadlock_attractor a) a

    let local_might_go_infinity soi a =
      try
  let unbounded_sub_area =
    Soc.fold
      (
        fun c accu ->
    try
      let _ = C.lub (C.projection soi c)
      in
        accu
    with
      | C.Undefined -> (Soc.add c accu)
      )
      a
      (empty ~d:(dimension a) ())
  in
    past_cone (remove_useless_empty_brick unbounded_sub_area) a
      with
  | Not_found -> failwith "might_come_to_infinity : the empty set does not represent any area"


    let local_deadlock_attractor soi a = difference a (local_might_go_infinity soi a)
 
    let infinity_attractor a = difference a (might_go_deadlock a)

    let cubical_set a = ()

    let iter f a = Soc.iter f a

    let fold f a x = Soc.fold f a x

    let filter f a = Soc.filter f a

    (* Dans la fonction qui suit, le dimension du cube doit être commune à
       celle de tous les autres cubes de a *)

let cube_ginzu_area c a =
  let dim = C.dimension c in
  let rec aux pos a =
  if pos < 0
  then a
  else
    (
      aux
        (pos-1)
        (
      let left = C.before pos c in
      let middle = C.during pos c in
      let right = C.after pos c in
        (
          Soc.fold
            (
          fun x accu ->
            (
              Soc.add (C.meet left x)
                (
              Soc.add (C.meet middle x)
                (Soc.add (C.meet right x) (Soc.remove x accu))
                )
            )
            )
            a
            (empty ~d:dim ())
        )
        )
    )
      in
  aux (dim-1) a

    let area_ginzu_area a1 a2 = Soc.fold cube_ginzu_area a1 a2

    let ginzu a = area_ginzu_area a a

    let cube_ginzu_area_imperative c a =
      let dim = C.dimension (Soc.choose !a) in
      let answer = ref (empty ~d:dim ()) in
      for pos = 0 to dim - 1
      do
        let left = C.before pos c in
        let middle = C.during pos c in
        let right = C.after pos c in
        let aux () =
            if !a = Soc.empty
            then a := !answer
            else
              let x = Soc.choose !a in
              a := Soc.remove x !a ;
              answer := Soc.add (C.meet left x) (Soc.add (C.meet middle x) (Soc.add (C.meet right x) !answer)) in
        aux () ; answer := (empty ~d:dim ())
      done

    let ginzu_imperative a =
      let dim = C.dimension (Soc.choose !a)
      in
      let rec aux ginzuter ginzuted =
  if (!ginzuter = Soc.empty)
  then
    a := !ginzuted
  else
    (
      let c = Soc.choose !ginzuter
      in
        (
    (ginzuter := Soc.remove c !ginzuter) ;
    (cube_ginzu_area_imperative c ginzuter) ;
    (cube_ginzu_area_imperative c ginzuted) ;
    (ginzuted := Soc.add c !ginzuted) ;
    (aux ginzuter ginzuted)
        )
    )
      in
  aux a (ref (empty ~d:dim ()))

    (* Return the connected component (in the topological sense)
       containing a given cube. If the argument is a normal form, then so
       is the result.*)

    let connected_component c a =
      let f c accu = Soc.union accu (Soc.filter (C.in_touch c) a) in
      let rec aux accu has_increased =
      if has_increased
      then
        let augmented_accu =
          Soc.fold f accu Soc.empty in
        aux augmented_accu (not (augmented_accu = accu))
      else accu in
      aux (of_cube c) true
    
    (*new version 14/12/2012, argument should be a normal form or satisfy the following property: if c and c' are in 
    touch, then there exists c'' which meets both c and c'*)
    
    let connected_component c a =
      let keep_going = ref true in
      let accumulator = ref Soc.empty in
      let current_wave = ref (of_cube c) in
      let condition c' c'' = 
        if C.not_disjoint c' c'' && not (Soc.mem c'' !accumulator) 
        then (keep_going := true; true) 
        else false in
      let sfx () = 
        let active_wave = !current_wave in
        current_wave := Soc.empty;
        accumulator := Soc.union !accumulator active_wave;
        Soc.iter (fun c' -> current_wave := Soc.union !current_wave (Soc.filter (condition c') a)) active_wave in              
      let () = while !keep_going do keep_going := false; sfx () done in
      !accumulator

    let connected a = is_included a (connected_component (Soc.choose a) a)

    let face b k a =
      let dim = dimension a
      in
  (
    Soc.fold
      (
        fun c accu ->
    Soc.add (C.face b k c) accu
      )
      a
      (Soc.singleton (C.empty dim))
  )

    let lub a =
      let candidate = C.lub (cubic_hull a)
      in
  if belongs_to_closure candidate a
  then
    candidate
  else
    raise Undefined

    let glb a =
      let candidate = C.glb (cubic_hull a)
      in
  if belongs_to_closure candidate a
  then
    candidate
  else
    raise Undefined

    let base a =
      let aux = ref Overall.IntSet.empty in
      let () =
      for i = ((dimension a)-1) downto 0 do
        if (Soc.exists (fun c -> ((C.of_list [C.slice i c]) <> (C.full 1))) a)
        then aux := (Overall.IntSet.add i !aux)
      done in
      Soc.fold (fun c accu -> (Soc.add (C.projection !aux c) accu)) a (Soc.empty)

    let proj0 (x0,x1,x2) = x0
    and proj1 (x0,x1,x2) = x1
    and proj2 (x0,x1,x2) = x2


    let cset2D_of_complement_of_cube c =
      let dim = C.dimension c
      and base = C.base_coordinates c
      in
  try
    (
      let codim0 = make (C.cset2D_complement c)
      in
      let answer = Soc.fold
        (
    fun c' accu ->
      (
        let local_accu1 = ref (fst accu)
        and local_accu2 = ref (snd accu)
        in
          (
      Overall.IntSet.iter
        (
          fun i ->
            let newbie = (C.back i c')
            in
        (
          if not (C.is_included newbie (C.closure c))
          then
            local_accu1 := Soc.add newbie !local_accu1
          else
            ()
          ;
          Overall.IntSet.iter
            (
              fun j ->
          (
            let newbie2 = (C.back j newbie)
            in
              if not (C.is_included newbie2 (C.closure c))
              then
                local_accu2 := Soc.add newbie2 !local_accu2
              else
                ()
          )
            ) (Overall.IntSet.filter (fun j -> (j > i)) base)
        )
        ) base
      ;
      local_accu1 := remove_useless_empty_brick !local_accu1
      ;
      local_accu2 := remove_useless_empty_brick !local_accu2
      ;
      (!local_accu1,!local_accu2)
          )
      )
        ) codim0 ((empty ~d:dim ()),(empty ~d:dim ()))
      in
        (
    (remove_useless_empty_brick codim0),
    (remove_useless_empty_brick(fst answer)),
    (remove_useless_empty_brick(snd answer))
        )
    )
  with
    | C.Base2D(codim0,codim1) -> (make codim0,make codim1,empty ~d:dim ())

    (* L'ensemble des intersections de la forme c1 inter c2 où c1 et c2
       appartiennent respectivement à a1 et a2 *)

    let distributed_intersection a1 a2 =
      let dim = dimension a1
      in
  (
    remove_useless_empty_brick
      (
        Soc.fold
    (
      fun c1 accu ->
        (
          Soc.fold
      (fun c2 accu -> Soc.add (C.meet c1 c2) accu)
        ) a2 accu
    ) a1 (empty ~d:dim ())
      )
  )

    let cset2D_of_complement_of_area a =
      let answer =
  (
    let dim = dimension a
    in
      Soc.fold
        (
    fun c accu ->
      (
        let current = cset2D_of_complement_of_cube c
        in
        let codim0 = (distributed_intersection (proj0 accu) (proj0 current))
        and codim1 =
          Soc.union
      (distributed_intersection (proj0 accu) (proj1 current))
      (distributed_intersection (proj1 accu) (proj0 current))
        and codim2 =
          Soc.union
      (distributed_intersection (proj0 accu) (proj2 current))
      (
        Soc.union
          (distributed_intersection (proj1 accu) (proj1 current))
          (distributed_intersection (proj2 accu) (proj0 current))
      )
        in
          (codim0,codim1,codim2)
      )
        ) a ((of_cube(C.full dim)),(empty ~d:dim ()),(empty ~d:dim ()))
  )
      in
  (
    (remove_useless_empty_brick (proj0 answer)),
    (remove_useless_empty_brick (proj1 answer)),
    (remove_useless_empty_brick (proj2 answer))
  )

  end
