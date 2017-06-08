let verbosity = ref 0


module type S =
sig
  type brick
  type area
  type t
  val yccc                         : brick -> t
  (** Partition of the complement of a cube following Yoneda components *)
  val connected                    : area -> t
  (** Partition following connected components *)
  val lattice                      : area -> t
  (** Covering following maximal sub-lattices *)
  val yoneda                       : area -> t
  (** Partition following Yoneda components *)
  val ginzu                        : area -> t
  (** The coarsest parition whose elements are cubes *)
  val mutex_program_model_yoneda   : (area ref) -> unit
  (** partition of a mutex program model following Yoneda components *)
  val choose                       : t -> area
  val add                          :  area -> t -> t
  val fold                         : (area -> 'a -> 'a) -> t -> 'a -> 'a
  val iter                         : (area -> unit) -> t -> unit
  val cardinal                     : t -> int (* Renvoie le nombre d'élément de la partition. *)
  val remove                       : area -> t -> t
  val empty                        : t
  val dimension                    : t -> int (* renvoie la dimension de l'espace ambiant. *)
  val string_of                    : t -> string (* pour affichage *)
  val string_list_of               : t -> string list (* pour affichage *)
  val find                         : (area -> bool) -> t -> area
  val print                        : t -> unit
  val clean                        : t -> t
  val dihomotopy_classes           : area -> t
  (* val dihomotopy_classes_version_3 : area -> t (*brick list list*) *)
  val exact_dihomotopy_classes     : ?a:area -> t -> t
  val dihomotopy_classes_based_on_ginzu : ?norm:unit -> area -> t
  val print_dihomotopy_classes     : area -> unit
  val dihomotopy_classes_iter_path : (brick list -> unit) -> area -> unit
  val dihomotopy_classes_iter_wave : (area       -> unit) -> area -> unit
    val dihomotopy_classes_fold_wave : (area -> 'a -> 'a  ) -> area -> 'a -> 'a
end

module type Requirements =
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


module Make(Overall:Requirements) = struct


  module OverCube
    (C:Cube.S with type ints = Overall.IntSet.t) 
    (A:AreaOverCube.S 
      with type regular_value = C.regular_value
      and type point = C.point
      and type generator = C.interval
      and type brick = C.t
      and type ints = Overall.IntSet.t)
    = 
  struct
    type regular_value = C.regular_value
    type point = C.point
    type generator = C.interval
    type brick = C.t
    type area = A.t
    type ints = Overall.IntSet.t

    module Soa = Set.Make(A)

    type t = Soa.t

    let string_list_of co =
      let answer = ref [] in
      let first = ref true in
      let add_line s = answer := s::!answer in
      (
	if co <> Soa.empty
	then
	  (
	    Soa.iter
	      (fun a ->
		(if not !first then add_line ((String.make !Common.Terminal.width '_')^"\n\n"));
		add_line "\n";
		add_line (A.string_of a);
		first := false
	      )
	      co
	  )
	else
	  add_line "string_list_of_covering : the empty set does not represent any covering"
      );
      !answer

    let fold = Soa.fold
    and iter = Soa.iter
    and cardinal = Soa.cardinal
    and remove = Soa.remove
    and empty = Soa.empty
    and add = Soa.add
    and choose = Soa.choose

    let find_version_1 statement co = Soa.choose (Soa.filter statement co)

    let find_version_2 statement co =
      let rec aux co =
	let x = Soa.choose co
	in
	  if (statement x) then x else (aux (Soa.remove x co))
      in
	aux co

    let find = find_version_2

    let dimension co = A.dimension (Soa.choose co)

    (* 

       Attention : pour que le résultat de la fonction qui suit soit
       correct, il faut que que la région cubique passé en argument soit
       sous forme normalisée.

    *)

    let connected a = A.fold (fun c accu -> (Soa.add (A.connected_component c a) accu)) a (Soa.empty)

    let ginzu a = A.fold (fun c accu -> (Soa.add (A.of_cube c) accu)) (A.ginzu a) (Soa.empty)

    (* 

       yccc_version_1 doit calculer un raffinement de la partition en
       Yoneda composantes, autrement dit cette partition n'est pas
       optimale. Cependant, on souhaite que dans le cas où le cube soit
       un cylindre dont la base est de dimension inférieure ou égale à
       2, le résultat soit exact.

    *)

    let yccc_version_1 c = 
      let dim = C.dimension c
      in
	(
	  try
	    let bd = C.base_dimension c
	    in
	      (
		if bd <= 1
		then
      A.fold (fun x accu -> (Soa.add (A.of_cube x) accu)) (A.complement_of_cube c) (Soa.empty)
		else
		  (
		    if bd = 2
		    then
		      (
			let a = A.complement_of_cube c
			in
			let downward_cubes = A.filter (fun x -> (x = C.downward x)) a
			and   upward_cubes = A.filter (fun x -> (x = C.upward   x)) a
			in
			let card_up   = A.cardinal   upward_cubes
			and card_down = A.cardinal downward_cubes
			in
			  (
			    if card_down < 2
			    then
			      let a_up = A.common upward_cubes
			      in
				Soa.add a_up (connected (A.difference a a_up))
			    else
			      (
				let a_do = A.common downward_cubes
				in
				  (
				    if card_up = 0
				    then
				      (
					Soa.add a_do (connected (A.difference a a_do))
				      )
				    else
				      (
					let a_up = A.common upward_cubes
					in
					  Soa.add a_do (Soa.add a_up (connected (A.difference a (A.join a_do a_up)))) (* Cas générique *)
				      )
				  )
			      )
			  )
		      )
		    else
		      (
			ginzu (A.complement_of_cube c) (* In higher dimension, we work the rough way *)
		      )
		  )
	      )
	  with
	    | C.Undefined -> Soa.singleton (A.of_cube (C.full dim)) (* pour avoir une forme normalisée *)
	)

    let yccc = yccc_version_1

    (* 

       Provide a covering with possible overlaps whose elements are the
       maximal sub-lattices of the fundamental category of a.

    *)

    let lattice a = failwith "Covering.Make.OverCube.lattice is not implemented yet." (* to be done *)

    (* 

       Provide a partition whose elements are the Yoneda components of
       the complement of a

       CONJECTURE : (yoneda a) is the partition generated by (lattice a)

    *)

    let yoneda_version_1 a = 
      let whole_space =  A.of_cube (C.full (A.dimension a))
      in
      let dig_a_cubic_hole c answer =
	let cube_complementary = yccc c
	in      
	  Soa.fold 
	    (
	      fun x local_answer -> 
		(
		  Soa.union local_answer 
		    (
		      Soa.fold 
			(
			  fun y aux -> Soa.add (A.meet x y) aux
			) 
			cube_complementary 
			Soa.empty 
		    )
		)
	    ) 
	    answer 
	    Soa.empty
      in
	A.fold dig_a_cubic_hole a (Soa.singleton whole_space)

    let yoneda_version_2 a = 
      let whole_space = A.of_cube (C.full (A.dimension a))
      in
      let dig_a_cubic_hole c answer =
	let cube_complementary = yccc c
	in      
	  Soa.fold 
	    (
	      fun x local_answer -> 
		(
		  Soa.union local_answer 
		    (
		      Soa.fold 
			(
			  fun y aux -> Soa.add (A.meet x y) aux
			) 
			cube_complementary 
			Soa.empty 
		    )
		)
	    ) 
	    answer 
	    Soa.empty
      in
	A.fold dig_a_cubic_hole a (Soa.singleton whole_space)

    let yoneda_version_3 a = 
      let whole_space = A.of_cube (C.full (A.dimension a))
      in
      let dig_a_cubic_hole c answer =
	let cube_complementary = yccc c
	in      
	  Soa.fold 
	    (
	      fun x local_answer -> 
		(
		  Soa.union local_answer 
		    (
		      Soa.fold 
			(
			  fun y aux -> Soa.add (A.meet x y) aux
			) 
			cube_complementary 
			Soa.empty 
		    )
		)
	    ) 
	    answer 
	    Soa.empty
      in
	A.fold dig_a_cubic_hole a (Soa.singleton whole_space)

    let yoneda = yoneda_version_2

    (* 

       Attention : pour que le résultat de la fonction qui suit soit
       correcte, il faut que que la région cubique passé en argument soit
       sous forme normalisée.

       Cette fonction fait un usage massif de la fonction difference,
       cette dernière garantit un retour sous forme normalisée, c'ets ce
       qui permet de le "réinjecter" récursivement...

       La sous-fonction find_branching_component renvoie, s'il y en a,
       l'une des composantes de a qui rencontre c et constitue un
       branchement dans la catégorie de composantes.

    *)

    (* 

       La région suivante fournie un contre-exemple qui montre que
       l'algorithme ne donne pas un résultat exact. 

       {0}x[0,1]x[0,1] | [0,1]x{0}x[0,1] | [0,1]x[0,1]x{1}

       Cependant, si la région est de dimension 2 ou représente un
       programme PV ne comportant que des mutex (éventuellement des
       sémaphores d'arité supérieure) il est possible que cet algorithme
       soit exact ... mais il faudrait encore le prouver.

    *)  

    (* Attention, il faut tester cette fonction... *)

    (*

      Il faudrait déterminer le domaine sur lequel cette fonction renvoie
      un résulat exact. Il semble par ailleurs que le résultat qu'elle
      fournit dépende de l'ordre total que l'on utilise sur les régions
      cubiques. Il suffit de considérer la région cubique suivante

      [0,5]x[0,2] | [1,2]x[0,6] | [0,2]x[0,3]

      ou même

      [0,2]x[0,2] | [0,3]x[0,1] | [1,2]x[0,3]

      ou encore "graphiquement"

      OOOO
      OXOO
      XXOO
      XXXO

      où la région prise en compte est décrite par les "X".

    *)

    let mutex_program_model_yoneda ra (* ra : reference to an area *) =
      let future_branching c1 c2 = 
	(
	  not ((C.is_included c1 (C.downward c2)) || (C.is_included c2 (C.downward c1)))
	)
      and past_branching c1 c2 = 
	(
	  not ((C.is_included c1 (C.upward c2)) || (C.is_included c2 (C.upward c1)))
	)
      in
      let branching (* donne un critère nécessaire pour que deux cubes ne soient pas contenus dans la même composante *) c1 c2 = 
	((past_branching c1 c2) || (future_branching c1 c2)) 
      in
      let answer = ref (Soa.empty) and flag = ref true
      in
      let find_branching_component c = 
	let rec aux c current has_forked =
	  let next_current = (A.next current !ra) 
	    (* Par rapport à l'ordre total sur l'ensemble !ra, on détermine l'élément qui suit current *)
            (* Dans le cas où current est déjà le plus grand élément de !ra, c'est current qui est renvoyé *)
	  in  
	    if (current <> next_current) 
	    then (* il y aura une prochaine étape *)
	      (
		let c' = C.meet c current
		in
		  if ((C.is_not_empty c') && (branching c current))
		  then
		    aux c' next_current true
		  else 
		    aux c  next_current has_forked 
	      )
	    else (* c'est la dernière étape *)
	      (
		let c' = C.meet c current
		in
		  if ((C.is_not_empty c') && (branching c current)) 
                    (* j'ai trouvé un branchement à la dernière étape *)
		  then
		    (
		      answer := Soa.add (A.of_cube c') !answer ; 
		      ra     := A.meet !ra (A.complement_of_cube c') ;
		      flag   := true
		    )		  
		  else 
		    (
		      if has_forked 
			(* je n'ai pas trouvé de branchement à la dernière étape mais il y en a eu avant *)
		      then
			(
			  answer := Soa.add (A.of_cube c) !answer ; 
			  ra     := A.meet !ra (A.complement_of_cube c) ;
			  flag   := true
			)
		      else 
			(* je n'ai pas trouvé de branchement à la dernière étape et il n'y en a pas eu avant... *)
			() (* ...autrement dit le cube c ne fait de branchement avec aucun autre *)
		    )
	      )
	in
	  aux c (A.min_elt !ra) false 
      in (* fin de la fonction find_branching_component *)
	(
	  while !flag 
            (* chaque fois que find_branching_component trouve une composante, il modifie ra et signale sa découverte au moyen de flag  *)
	  do 
	    flag := false 
	    ;
	    (
	      let current = ref (A.min_elt !ra) 
	      in 
	      let next_current = ref (A.next !current !ra) 
	      in
		(
		  (
                    while (!current <> !next_current)
		    do
		      find_branching_component !current ;
		      if !flag (* ra a été modifiée, il faut reprendre depuis le début *)
		      then
			(
			  current := (A.min_elt !ra) ;
			  next_current := A.next !current !ra
			)
		      else (* on a pas trouvé de composante, ra est inchangée, on continue *)
			(
			  current := !next_current ;
			  next_current := A.next !next_current !ra
			)
		    done
		  )
		  ;
		  (
		    find_branching_component !current ;
		    if !flag (* ra a été modifiée, il faut reprendre depuis le début *)
		    then
		      (
			current := (A.min_elt !ra) ;
			next_current := A.next !current !ra
		      )
		    else (* on a pas trouvé de composante, ra est inchangée, on continue *)
		      () (* il n'y a plus de composante à trouver *)
		  )
		)
	    )
	  done  
	) (* ne reste plus qu'à séparer les composantes connexes qui dans ce qui reste, forment chacune une composante *)
	  

    let string_of_version_1 co = 
      try
	let co_block = Soa.choose co 
	in
	let co' = Soa.remove co_block co
	in    
	let aux c s = (s^"----------\n "^(A.string_of co_block)^"\n")
	in
          Soa.fold aux co' (A.string_of co_block)
      with
	| Not_found -> "string_of : the empty set does not represent any covering"  
	    

    let string_of_version_2 co = 
      if co <> Soa.empty 
      then
	Soa.fold
	  (
	    fun a accu -> 
	      accu^(A.string_of a)^"\n--------------------------------------------------------------------------------\n"
	  )
	  co
	  "\n--------------------------------------------------------------------------------\n"
      else
	"string_of : the empty set does not represent any covering"

    module V = 
    struct
      type t = A.brick
      let compare = C.compare
      let hash c = 0
      let equal c c' = (c = c')
    end
      
    module DC = (Graph.Imperative.Digraph.Concrete(V))

    module DA = (Graph.Imperative.Digraph.Abstract(V))

    (* Question : Is this graph loop-free ? I think it should be. *)
    (* Actually it is not, think of the four squares. *)

    (* Write a function which tests loop-freeness. *)

    let relevant_dipaths_version_5 a =
      let answer = DC.create ()
      and a' = A.compress a
      and departure = A.departure a
      and arrival = A.arrival a
      in
	(
          A.iter
	    (
	      fun c ->
		(
		  A.iter
		    (
		      fun c' ->
			let it = C.in_touch c c'
			in
			  if it
			  then
			    let s = C.in_the_past_of c' c and t = C.in_the_future_of c c'
			    in
			      (
				if
				  (
				    try
				      (not(C.is_included s c') && not(C.is_included t c )) ||
					((A.is_included (A.of_cube c) departure) && (A.is_included (A.of_cube c') arrival))
				    with
				      | _ -> false
				  )
				then
				  DC.add_edge answer c c'
				else
				  ()
			      )
			  else
			    ()
		    )
		    a'
		)
	    )
	    a'
	  ;
	  (
	    let sources = A.sources a
	    in
	      A.iter 
		(
		  fun c -> 
		    (
		      if 
			(
			  try 
			    (
			      A.belongs_to (C.glb c) sources
			    ) 
			  with | _ -> false
			)
		      then DC.add_vertex answer c 
		      else ()
		    )
		) 
		a
	  )
  	  ;
	  answer
	)
	  

    let print_all_vertices g =
      DC.iter_vertex (fun c -> (print_endline (C.string_of c))) g
	

    let print_all_arrows g =
      DC.iter_edges_e (fun arrow -> (print_endline ((C.string_of (DC.E.src arrow))^" -> "^(C.string_of (DC.E.dst arrow))))) g
	


    let relevant_dipaths a = 
      let g = relevant_dipaths_version_5 a
      in
	if !verbosity >= 2 (*!Flag.full_verbose *)
	then
	  Common.Terminal.(print_endline (color Yellow ~bold:true "Vertices") ; 
    print_all_vertices g;
    print_endline (color Yellow ~bold:true "\nArrows"); 
    print_all_arrows g ; print_endline ((color Yellow ~bold:true "number of vertices : ")^(string_of_int (DC.nb_vertex g)));
    print_endline ((color Yellow ~bold:true "number of arrows : ")^(string_of_int (DC.nb_edges g))) ; g)
	else
	  (
	    if !verbosity >= 1 (*!Flag.verbose *)
	    then
	      Common.Terminal.(print_endline ((color Yellow ~bold:true "number of vertices : ")^(string_of_int (DC.nb_vertex g)));
        print_endline ((color Yellow ~bold:true "number of arrows : ")^(string_of_int (DC.nb_edges g))) ; g)
	    else
	      g
	  )


    (*

      Une idée fausse :

      (C.meet (C.upward a) b) is empty <=> (C.meet a (C.downward b)) is empty

      Pour autant ces deux ensembles sont généralement différents :
      prendre par exemple a = [2,3]x[1,4] et b = [1,4]x[2,3]

    *)


    (* Les fonctions smoothen_up et smoothen_down ne commutent pas. Le
       fait que l'on mette (ans) ou [next] dans leur code ne change
       rien. C'est normal puisqu'elle dépile puis empile les éléments
       d'une liste.*)

	  
    (*

      On représente un chemin de la façon suivante : une liste de
      sous-cubes maximaux sans répétition. On exige en outre que pour
      chaque [...c;c'...]

      (C.in_the_future_of c c') ne soit pas inclus dans c

      et

      (C.in_the_past_of c' c) ne soit pas inclus dans c'

      La tête de la liste représente alors le point le plus avancé.

      On a trois références, accu, growing et to_be_grown. 

      1) Initialisation : accu et to_be_grown sont vide tandisque growing
      contient les listes de longueur 1 dont le seul élément est dans
      departure.

      2) Itération : tant que growing n'est pas vide

      {

      2.0) On vide to_be_grown

      pour chaque chemin gamma de growing

      {

      2.1) Si la tête de gamma est dans arrival on ajoute gamma à accu.

      2.2) Pour chaque cube c qui prolonge gamma on ajoute c::gamma à
      to_be_grown 

      }

      2.3) growing <- to_be_grown

      2.4) retour à 2)

      }

      3) Lissage et convertion de accu au type "set of arrays".

    *)

    let counter = ref 1

    let execution_tracing = false (* true *)

    let weaken_is_an_extension = false



    let rec smoothen_down path ans = 
      match path with
	| a::b::path' ->
	    smoothen_down ((C.in_the_past_of ~it:true a b)::path') (a::ans)
	| a::[] -> a::ans
	| _ -> ans

    (* Les fonctions smoothen_up et smoothen sont vraissemblablement
       devenues inutiles. (?) *)

    let rec smoothen_up path ans =  
      match path with
	| a::b::path' ->
	    smoothen_up ((C.in_the_future_of ~it:true a b)::path') (a::ans)
	| a::[] -> a::ans
	| _ -> ans

    let smoothen path = 
      let answer = smoothen_up (smoothen_down path []) [] (* smoothen_up et smoothen_down ne commutent pas *)
      in
	(
	  if execution_tracing
	  then
	    (
	      List.iter (fun c -> print_endline (C.string_of c)) answer
	      ;
	      print_endline "="
	    )
	  else
	    ()
	)
	;
	answer

    (* Il y a des ensembles vides ... *)

    let printwave s w = 
      if execution_tracing
      then
	(
	  if s <> "" then print_endline s else () ; 
	  List.iter 
	    (
	      fun path -> print_endline 
		(
		  String.concat " -> " (List.map C.string_of (List.rev path))
		)
	    ) 
	    w
	)
      else
	()

(*

  Dans l'exemple "helix.pv", la première trace de la 6ème vague est la
  suivante :

  [0,1[*[0,-[*[0,-[ -> [0,-[*[0,-[*[6,7[ -> [0,3[*[6,-[*[0,-[ -> [0,-[*[0,-[*[4,5[ -> [0,3[*[4,-[*[2,-[ -> [0,-[*[0,-[*[8,-[

  ou même dès la 4ème vague (seconde trace)

  [0,1[*[0,-[*[0,-[ -> [0,-[*[0,-[*[6,7[ -> [0,3[*[6,-[*[0,-[ -> [0,-[*[0,-[*[4,5[

  On constate que cette trace passe par le cube [0,-[*[0,-[*[6,7[ puis
  par le cube [0,-[*[0,-[*[4,5[, la trace donc a "reculé" ce qui est
  interdit par la relation d'ordre présente sur chaque région cubique.

  Le problème a été réglé en modifiant le critère de stockage d'un
  chemin dans accu.

*)

    (* 

       Quelques tests de performance et de validation

       5philosophers.pv : ~9m38s 26132 classes puis 82 après nettoyage puis 32 après raffinement (résultat exact)

       4philosophers.pv : <1s 416 classes puis 22 après nettoyage puis 16 après raffinement (résultat exact)

       3philosophers.pv : <1s 14 classes puis 8 après nettoyage (résultat exact)

       2philosophers.pv : <1s 4 classes puis 4 après nettoyage (résultat exact)

       helix.pv         : ~3m24s 18854 classes puis 168 après nettoyage puis (après ~1h48min51s) 1 après raffinement (résultat exact)

    *)

    let dihomotopy_classes_version_4 a = print_endline "dihomotopy_classes_version_4";
      let arrival = A.arrival a (* critère de découverte d'un chemin maximal *) in
      let accu = ref [] in
      let to_be_grown = ref [] in
      let growing = ref (A.fold (fun c accu -> [c]::accu) (A.departure a) []) in 
      let is_a_maximal_dipath head_of_the_path path = A.exists (fun c -> C.are_cofinal c head_of_the_path) arrival in
      let _ = if execution_tracing then
	(
	  print_endline ("Arrival\n"^(A.string_of arrival)^"\n") ;
	  printwave ("Wave number "^(string_of_int !counter)) !growing ;
	  print_endline "_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _"
	)
      in
	while !growing <> [] (* Il reste des chemins en croissance *)
	do
	  incr counter (* Pourra être supprimé ultérieurement. *)
	  ;
	  to_be_grown := [] (* On réinitialise la prochaine vague *)
	  ;
	  List.iter 
	    (
	      fun path -> (* Etant donné un chemin en croissance ... *)
		let h = List.hd path in
		  if is_a_maximal_dipath h path (* ...si on a trouvé un chemin maximal... *)
		    (* A.is_included (A.of_cube h) arrival *)
		    (* A.mem (List.hd path) arrival *) (* mauvais critère *) 
		  then (* ...on l'ajoute à ceux que l'on a déjà et on sait qu'il ne grandira plus... *)
		    accu := path::!accu 
		  else (* ...sinon on va essayer de le faire grandir. *)
		    (
		      A.iter 
			(
			  fun c -> (* Pour chaque cube de la région sur laquelle on travaille... *)
			    let extension = C.in_the_future_of h c in
			    let anti_extension = C.in_the_past_of c h in
			      if (not (C.is_included extension h)) && (not(C.is_included anti_extension c)) 
				(* ...a-t-on une extension du chemin ? *)
			      then (* Si oui on ajoute le chemin étendu aux chemins à faire croître à l'itération suivante *)
				to_be_grown := (extension::path)::!to_be_grown 
			      else
				()
			)
			a
		    )
	    )
	    !growing
	  ;
	  growing := !to_be_grown
	  ;
	  (
	    if execution_tracing 
	    then 
	      (
		printwave ("Wave number "^(string_of_int !counter)) !growing 	
		;
		print_endline "---"
	      )
	    else ()
	  )
	done
	;
	(
	  if execution_tracing 
	  then 
	    (
	      print_endline "\nLes chemins de référence\n"
	      ;
	      List.iter (fun path -> print_endline (String.concat " -> " (List.rev (List.map C.string_of path)))) !accu 
	    )
	  else ()
	)
	;
	if execution_tracing then print_endline "\nLes classes de dihomotopies\n" else ()
	;
	List.fold_left
	  (
	    fun accu path ->
	      let aux = 
		(
		  A.compress (* Utile *) 
		    (
		      List.fold_left (fun accu c -> A.add c accu) 
			(A.empty ~d:(A.dimension a) ()) 
			(smoothen_down path [])
		    )
		)
	      in
		(
		  if execution_tracing
		  then
		    (
		      print_endline (A.string_of aux)
		      ;
		      print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
		    )
		  else
		    ()
		)
		;
		Soa.add
		  aux
		  accu
	  )
	  Soa.empty
	  !accu


    (* Un autre version basée sur les propriétés de la fonction ginzu. *)


    let dihomotopy_classes a = 
      let answer = dihomotopy_classes_version_4 a in 
      let card = (Soa.cardinal answer) in
	    let () = Printf.printf "There %s %i dihomotopy %s.\n%!" 
      (if card > 1 then "are at most" else "is")
      card
      (if card > 1 then "classes" else "class") in
      answer

    (*

      Itération sur l'ensemble des classes d'équivalence de dihomotopy de
      chemins de la région cubique a. Attention : dans sa version
      actuelle, certaines classes peuvent être "répétées". 

    *)

    let dihomotopy_classes_iter_path effect a =
      let g = relevant_dipaths a
      in
      let initial =
	let sources = A.sources a
	in
	  (
	    DC.fold_vertex
	      (
		fun c li ->
		  (
		    if 
		      (
			try 
			  A.belongs_to (C.glb c) sources
			with 
			  | _ -> false
		      ) 
		    then 
		      c::li 
		    else
		      li
		  )
	      )
	      g
	      []
	  )
      in
      let available_bifurcation path = 
	try
	  DC.succ g (List.hd (List.tl path))
	with
	  | Failure "hd" -> initial
      in
      let rec backtrack path =
	(
	  match path with
	    | x::path' -> 
		if path'=[]
		then
		  (
		    try
		      [Common.List.next_in_list x initial] 
		    with
		      | _ -> []
		  )
		else
		  (
		    try
		      let y = (Common.List.next_in_list x (available_bifurcation path))
		      in
			(
			  y::path'
			)
		    with
		      | _ -> backtrack path'
		  )
	    | [] -> [] (* ce cas ne devrait pas se produire...je pense *)
	)
      in
      let rec explore path =
	(
	  if path <> []
	  then      
	    (
	      let next_wave = DC.succ g (List.hd path)
	      in
		if next_wave <> []
		then
		  (
		    explore ((List.hd next_wave)::path)
		  )
		else
		  (
		    (effect path) ;
		    (explore (backtrack path))
		  )
	    )
	  else
	    () (* Terminaison *)
	)
      in
	(
	  try
	    (explore [List.hd initial]) 
	  with
	    | _ -> failwith "No maximal paths : the model is empty" 
	)

    let clean co = 
      let answer = Soa.filter (fun a -> Soa.for_all (fun a' -> ((not(A.is_included a a')) || (a=a'))) co) co in
      let card = (Soa.cardinal answer) in
	Printf.printf "There %s %i dihomotopy %s after clean up.\n%!" 
	  (if card > 1 then "are at most" else "is")
	  card
	  (if card > 1 then "classes" else "class")
	;answer

    (* La fonction suivante peut-être améliorée en supprimant au fur
       et à mesure les chemins que l'on a amalgamé. *)

    let exact_dihomotopy_classes ?a co =
      let a = match a with
	| Some a -> a
	| None -> A.normalize (Soa.fold (fun a accu -> A.join a accu) co (A.empty ~d:(A.dimension(Soa.choose co)) ()))
      in
      let related a0 a1 = 
	(A.connected (A.meet a (A.meet (A.closure a0) (A.closure a1))))
	&& (A.coinitial a0 a1)
	&& (A.cofinal a0 a1)
      in
      let equivalence_class a =  
	let answer = ref a in
	let has_been_increased = ref true in
	let current_covering = ref co in
	  while !has_been_increased
	  do
	    has_been_increased := false ;
	    answer :=
	      Soa.fold
		(
		  fun a' accu -> if related a' accu 
		  then 
		    (
		      has_been_increased := true ; 
		      current_covering := Soa.remove a' !current_covering ; 
		      A.join a' accu
		    ) 
		  else accu
		)
		!current_covering
		!answer (*A.empty ()*)
	  done
	  ;
	  !answer
      in 
      let answer = 
	Soa.fold (fun a accu -> Soa.add (equivalence_class a) accu)
	  co 
	  Soa.empty
      in 
      let card = (Soa.cardinal answer) in
	Printf.printf "There %s %i dihomotopy %s after thorough clean up.\n%!" 
	  (if card > 1 then "are at most" else "is")
	  card
	  (if card > 1 then "classes" else "class")
	;answer

    (* In progress: for now we just providing a list of traces so that
       any dihomotopy class is represented. *)


    let dihomotopy_classes_based_on_ginzu ?norm a = 
      let a = match norm with 
	| Some () -> let a = print_string "Normalizing..." ; A.normalize a in print_endline "done." ; a
	| None -> print_endline "No need to normalize." ; a
      in
      let ga = A.ginzu a in
      let answer = empty in
      let current_wave = ref (A.departure a) in
      let next_wave = ref empty in (* the "ignore" instruction have been put to prevent the compiler from detecting unused variable *)
	ignore ga ; ignore answer ; ignore current_wave ; ignore next_wave ; empty

    let dihomotopy_classes_iter_wave f a = Soa.iter f (clean (dihomotopy_classes a))

    let dihomotopy_classes_fold_wave f a initial_value = Soa.fold f (clean (dihomotopy_classes a)) initial_value

    (* Il peut y avoir des répétitions *)

    let print_dihomotopy_classes a =
      let print_path path = 
	(
	  A.print (A.normalize (A.make path)) ; 
	  print_string ((String.make !Common.Terminal.width '-')^"\n")
	)
      in
      let g = relevant_dipaths a
      in
      let initial =
	A.fold
	  (
	    fun c li ->
	      (
		if DC.pred g c = [] 
		then 
		  c::li 
		else
                  li
	      )
	  )
	  a
	  []
      in
      let available_bifurcation path = 
	try
	  DC.succ g (List.hd (List.tl path))
	with
	  | Failure "hd" -> initial
      in
      let rec backtrack path =
	(
	  match path with
	    | x::path' -> 
		if path'=[]
		then
		  (
		    try
		      [Common.List.next_in_list x initial] 
		    with
		      | _ -> []
		  )
		else
		  (
		    try
		      (Common.List.next_in_list x (available_bifurcation path))::path'
		    with
		      | _ -> backtrack path'
		  )
	    | [] -> [] (* ce cas ne devrait pas se produire...je pense *)
	)
      in
      let rec explore path =
	(
	  if path <> []
	  then      
	    (
	      let next_wave = DC.succ g (List.hd path)
	      in
		if next_wave <> []
		then
                  explore ((List.hd next_wave)::path) (* parcourt en profondeur *)
		else
		  (
		    (print_path path) ;
		    explore (backtrack path)
		  )
	    )
	  else
	    ()
	)
      in
	try
	  (
	    print_string ((String.make !Common.Terminal.width '-')^"\n") ;
	    explore [List.hd initial]
	  )
	with
	  | _ -> 
	      print_string "No maximal paths : the model is empty"
	  

    let string_of = string_of_version_2
      

    let print co = 
      if co <> Soa.empty 
      then
	(
	  print_string ((String.make !Common.Terminal.width '_')^"\n\n")
	  ;
	  Soa.iter
	    (
	      fun a -> 
		A.print a ; print_string ((String.make !Common.Terminal.width '_')^"\n\n")
	    )
	    co
	)
      else
	print_string "string_of : the empty set does not represent any covering"

    let atomize a = A.fold (fun c accu -> (Soa.add (A.of_cube c) accu)) a Soa.empty 

  end
  
  type ints = Overall.IntSet.t

  module OverTorus(T:Torus.S) = 
  struct
    (* To be done *)
  end

  module OverCylinder(X:Cylinder.S) =
  struct
    (* To be done *)
  end

end
