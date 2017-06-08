(*open Type*)
open Semantics

module type S =
sig
  type regular_value
  type area
  val recursive_mutex: string -> Semantics.process -> area
  val write: string -> Semantics.process -> regular_value array
  val read: string -> Semantics.process -> regular_value array
  val quantitative_semaphore_levelwise: string -> int -> int -> Semantics.process -> area
  val ifo_levelwise: string -> int -> process -> area
  val switch_mutex: ?initial:bool -> string -> process -> area
  val normal_mutex: string -> process -> area
  val switch_semaphore: string -> process -> area
end (*S*)

module Make
  (RV:sig type t
          val step: t -> t
          val start: t
  end)
  (AC:AreaOverCube.S with type regular_value = RV.t):
  (S with type regular_value = RV.t and type area = AC.t)

  =

struct
type regular_value = RV.t
type area = AC.t

(* L'argument mtx_rec est le mutex dont on cherche la partie occuppée : string.
L'argument il est le processus dont on cherche la partie occuppée par le mutex mtx_rec. *)

(* La position courante : int.
La liste d'instructions à analyser : instruction list.
Le nombre de "locks" déjà posé sur le sémaphore.
La région de dimension 1 qui représente la zone où le mutex est pris : A.t. *)

let recursive_mutex mtx_rec il =
  let rec aux p il nbo accu =
    if nbo < 0
    then failwith (Printf.sprintf "Warning : some process attempts to unlock recursive mutex %s though it does not hold it." mtx_rec)
    else
      match il with
      | (P(a),_)::il' ->
        if a = mtx_rec
        then
          if nbo = 0
          then aux (RV.step p) il' 1 (AC.join accu (AC.terminal true p))
          else aux (RV.step p) il' (nbo+1) accu
        else aux (RV.step p) il' nbo accu
      | (V(a),_)::il' ->
        if a = mtx_rec
        then
          if nbo = 1
          then aux (RV.step p) il' 0 (AC.difference accu (AC.terminal true p))
          else aux (RV.step p) il' (nbo-1) accu
        else aux (RV.step p) il' nbo accu
      | ins::il' -> aux (RV.step p) il' nbo accu
      | [] -> AC.normalize accu in
  AC.normalize (aux RV.((*step*) start) (Array.to_list il.code) 0 (AC.empty ~d:1 ()))

(* Mutex dont on cherche la partie occuppée : string. *)
(* Processus dont on cherche la partie occuppée par le mutex mtx_rec. *)
(*Bounded or not*)
(*Current position: int*)
(*Process to be analyzed: instruction list*)
(*locus of points where the resource is held: A.t*)

let switch_mutex ?(initial=false) mtx_rec il =
  let accu =
    if initial
    then (AC.full ~d:1 ())
    else (AC.empty ~d:1 ()) in
  let rec aux bnd p il accu =
    match il with
    | (P(a),_)::il' ->
      if a = mtx_rec
      then aux true (RV.step p) il' (AC.join accu (AC.terminal true p))
      else aux bnd (RV.step p) il' accu
    | (V(a),_)::il' ->
      if a = mtx_rec
      then aux false (RV.step p) il' (AC.difference accu (AC.terminal true p))
      else aux bnd (RV.step p) il' accu
    | ins::il' -> aux bnd (RV.step p) il' accu
    | [] -> AC.normalize accu in
  AC.normalize (aux initial RV.((*step*) start) (Array.to_list il.code) accu) (*bugfix 11/03/2013*)

let normal_mutex mtx_rec il =
  let rec aux p il lckd accu =
    match il with
    | (P(a),_)::il' ->
      if a = mtx_rec
      then
        if lckd
        then AC.normalize accu
        else aux (RV.step p) il' true (AC.join accu (AC.terminal true p))
      else aux (RV.step p) il' lckd accu
    | (V(a),_)::il' ->
      if a = mtx_rec
      then aux (RV.step p) il' false (AC.difference accu (AC.terminal true p))
      else aux (RV.step p) il' lckd accu
    | ins::il' -> aux (RV.step p) il' lckd accu
    | [] -> AC.normalize accu in
  AC.normalize (aux RV.((*step*) start) (Array.to_list il.code) false (AC.empty ~d:1 ()))

let switch_semaphore sem il =
  let rec aux p il accu =
    match il with
      | (P(a),_)::il' ->
        if a = sem
        then aux (RV.step p) il' (AC.join accu (AC.terminal true p))
        else aux (RV.step p) il' accu
      | (V(a),_)::il' ->
        if a = sem
        then aux (RV.step p) il' (AC.difference accu (AC.terminal true p))
        else aux (RV.step p) il' accu
      | ins::il' -> aux (RV.step p) il' accu
      | [] -> AC.normalize accu in
  AC.normalize (aux RV.((*step*) start) (Array.to_list il.code) (AC.empty ~d:1 ()))

    (* Pour des raisons combinatoires, l'arité d'un sémaphore à compteurs
       ne devrait pas dépasser 50. *)

    (*-----------------------------------------------------------------------*)
    (* Calcul de la région interdite générée par les sémaphores quantitatifs *)
    (*-----------------------------------------------------------------------*)

    (** Dans le processus séquentiel *il*, on recherche le lieu des points où le processus détient n occurrences du sémaphore 
    *sem* d'arité *arity_sem* (c'est donc une région de dimension 1). Lève une erreur dans le cas où le processus tente de 
    libérer une occurrence de *sem* alors qu'il n'en détient aucune. Plus précisément, la fonction renvoie le lieu des 
    points où le processus détient au moins n occurrences du sémaphore, avec un phénomène de blocage au cas où il prenne 
    toutes les occurrences. En particulier on obtient une filtration (chaîne décroissante) de région en faisant croître le 
    paramètre n*)

let quantitative_semaphore_levelwise sem arity_sem n il =
  let rec aux p il nbo accu =
    if nbo < 0
    then failwith
      (Printf.sprintf "Warning : some process attempts to unlock the counting semaphore %s though it does not hold it." sem)
    else
      if arity_sem <= nbo
      then AC.normalize (AC.join accu (AC.terminal true p))
      else
        match il with
          | (P(a),_)::il' ->
              if a = sem
              then
                if nbo = n - 1
                then aux (RV.step p) il' (nbo + 1) (AC.join accu (AC.terminal true p))
                else aux (RV.step p) il' (nbo + 1) accu
              else aux (RV.step p) il' nbo accu
          | (V(a),_)::il' ->
              if a = sem
              then
                if nbo = n
                then aux (RV.step p) il' (nbo - 1) (AC.difference accu (AC.terminal true p))
                else aux (RV.step p) il' (nbo - 1) accu
              else aux (RV.step p) il' nbo accu
          | ins::il' -> aux (RV.step p) il' nbo accu
          | []       -> AC.normalize accu in
  AC.normalize (aux RV.start (Array.to_list il.code) 0 (if n > 0 then AC.empty ~d:1 () else AC.full ~d:1 ()))

(* L'arité d'une file/pile est le nombre maximum de messages que
   peut contenir la file/pile. *)

(* Par défaut la réception de message sur une pile/file vide est
   blocante alors que le débordement ne l'est pas *)

(* Lieu des points où la hauteur/longueur de la pile/file est au
   moins n. Pas de saturation si on atteint la capacité de la
   pile. *)

(* il y a sans doute un bug ici : on veut le lieu des points dont
   la charge est supérieure ou égale à n *)

(* Il y a un problème, notamment dans la partie négative: je crois
   qu'il est réglé *)

let ifo_levelwise ifo n il =
  let rec aux p il nbo accu =
    match il with
      | (S(a,_),_)::il' ->
            if a = ifo
            then
              if n - 1 = nbo
              then aux (RV.step p) il' (nbo + 1) (AC.join accu (AC.terminal true p))
              else aux (RV.step p) il' (nbo + 1) accu
            else aux (RV.step p) il' nbo accu
      | (R(a,_),_)::il' ->
            if a = ifo
            then
              if nbo = n
              then aux (RV.step p) il' (nbo - 1) (AC.difference accu (AC.terminal true p))
              else aux (RV.step p) il' (nbo - 1) accu
            else aux (RV.step p) il' nbo accu
      | ins::il' -> aux (RV.step p) il' nbo accu
      | []       -> AC.normalize accu in
    AC.normalize (aux RV.((*step*) start) (Array.to_list il.code) 0 (if n>0 then AC.empty ~d:1 () else AC.full ~d:1 ()))

let write varname il =
  let il = Array.to_list il.code in (* 2012-06-06 *)
  let pos = ref RV.start in
  let f accu x =
  let () = pos := RV.step !pos in
    match x with
	    | (T(id,_),_) -> if id = varname then Array.append accu [|!pos|] else accu
	    | _       -> accu in
  List.fold_left f [||] il

(* Attention les "if then else's" ne sont pas bien pris en compte,
   pas plus que les "call's" *)

let read varname il =
  let il = Array.to_list il.code in (* 2012-06-06 *)
  let pos = ref RV.start in
  let f accu x = let () = pos := RV.step !pos in
   match x with
    | (T(_,e),_) ->
      if variable_occurs_in_expression varname e
      then Array.append accu [|!pos|]
      else accu
    | (ITE ilbl,_) -> (* pas trop sûr que cette condition soit à retenir *)
      if List.exists (* pas de recherche en profondeur dans un premier temps *)
        (fun (il,e) -> variable_occurs_in_expression varname e) (* || (variable_occurs_in_process varname il) *)
         ilbl
      then Array.append accu [|!pos|]
      else accu
    | _ -> accu
  in List.fold_left f [||] il

end (*Make*)
