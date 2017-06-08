open Type


(* Ce module doit permettre de gérer les petites catégories sans boucle finies. *)

(* Iadlg : Imperative Concrete Directed Labeled Graphs whose vertices and arrows are strings *)

module Icdlgves = (Graph.Imperative.Digraph.ConcreteLabeled(Compare))(Compare)


(* Iadlg : Imperative Abstract Directed Labeled Graphs whose vertices and arrows are strings *)

module Iadlgves = (Graph.Imperative.Digraph.AbstractLabeled(Compare))(Compare)


(* Etant donné un graph sans boucle et un ensemble de paire de chemins parallèles, on veut *) 
(* déterminer la petite catégorie sans boucle qui en résulte. La complexité du problème est *)
(* très importante aussi ces algorithmes ne fonctionneront que dans des cas "petits". *)

module Cs (* Composable Sequences *) =
struct 
  type t = ((Icdlgves.E.t) array)
  let compare c1 c2 = (* Ordre lexicographique sur les tableaux *)
    let l1 = Array.length(c1) and l2 = Array.length(c2) 
    in 
    let common_len = (min l2 l1)
    in
    let rec aux a1 a2 n =
      (	  
	if (n<common_len) 
	then 	    
	  let diff = (Icdlgves.E.compare c1.(n) c2.(n))
	  in
	    if (diff != 0) then diff else (aux a1 a2 (n+1))
	else
	  (l2 - l1)
      )
    in
      aux c1 c2 0

  let is_composable c = 
    let l = ((Array.length c)-1)
    in
    let rec aux a n =
      if n<l then 
	(
	  if ((Icdlgves.E.src c.(n+1)) = (Icdlgves.E.dst) c.(n)) 
	  then
	    aux a (n+1)
          else
	    false
	)
      else 
	true
    in
      aux c 0
end



module Scs = Set.Make(Cs) (* Set of composable sequences *)


module Spcs = Set.Make (* Set of Pairs of Composable Sequences *)
  (
    struct (* Pair of Composable Sequences *)
      type t = (Cs.t)*(Cs.t)
      let compare p1 p2 = 
	let diff_firsts = Cs.compare (fst p1) (fst p2) 
	in
	  if (diff_firsts != 0) then diff_firsts else Cs.compare (snd p1) (snd p2) 
    end
  )


type flfc = 
    {
      mutable grph:Icdlgves.t (* type "graphe dirigé étiqueté" *) ; 
      mutable rltn:Spcs.t (* type "ensemble de paires de suites composables" *)
    }


(* Beyond this line, we have the function that manipulates "paths" on finite loop-free categories *)


(* La fonction suivante renvoie l'ensemble des suites composables réécrites à partir de c dans la *)
(* catégorie sans boucle finie cat, si le système de réécriture que l'on considère provient d'une *)
(* ctaégorie sans boucle finie, cette fonction termine *)

let match_at2 a c n = (* cette fonction n'a pas encore été testée! *)
  let la = Array.length a and lc = Array.length c
  in
    if (la > (lc-n)) (* on vérifie que la longueur de a est inférieure ou égale à celle du segment final à partir de n *)
    then 
      false (* si ce n'est pas le cas, a ne peut pas être une sous-suite de c à partir de la position n *)
    else 
      (
	let rec aux a1 c1 k =
	  if k<la then (* si on a pas épuisé a...*)
	    (
	      if (a1.(k)=c1.(n+k)) (*... on vérifie que les termes correspondent... *)
	      then 
		aux a1 c1 (k+1) (*...si oui on passe au suivant... *)
	      else 
		false (*...sinon a n'est pas un sous-terme à partir de la position n. *)
	    )
	  else true (* si on a épuisé a alors c'est un sous terme de c à la position n *)
	in
	  aux a c 0
      )


(* une deuxième version qui ne prend pas en compte les "sorties de tableau" *)

let match_at a c n = (* cette fonction n'a pas encore été testée! *)
  let rec aux k =
    try 
      let x = a.(k) (* on essaie d'extraire le k-ième terme *)
      in
	(
	  try
	    if x=c.(n+k) (* on compare les termes courants de a et c...*) 
	    then 
	      (aux (k+1)) (*...s'ils sont égaux on passe au suivant... *)
	    else 
	      false (*...sinon a n'est pas un sous-terme de c à la position n *)
	  with 
	    | _ -> false (* si on arrive pas à extraire le terme de c correspondant, a n'est pas un sous-terme de c à la position n *)
	)
    with 
      | _ -> true (* si on n'y arrive pas, c'est que a est un sous-terme à la position n *)


  in
    aux 0


(* la fonction qui suit n'est pas blindée *)

let replace (a,b) c n = 
  let la = (Array.length a) and lc = (Array.length c) and debut = (Array.sub c 0 n)  
  in 
  let  fin = (Array.sub c (n+la) (lc-(n+la)))
  in
    Array.concat [debut;b;fin]


(* la fonction qui suit recherche toutes les occurrences de a dans c et, chaque fois qu'elle en trouve *)
(* une, la remplace par b et ajoute le résultat obtenu a l'ensemble "accumulator" *)

let sons_from_a_rewritting (a,b) c = 
  let accumulator = ref (Scs.empty) and la = (Array.length a) and lc = (Array.length c) 
  in 
  let n = (lc-la)
  in
    if n<0 
    then 
      !accumulator 
    else
      (
	(
	  for i=0 to n
	  do 
	    if (match_at a c i) 
	    then (accumulator := Scs.add (replace (a,b) c i) (!accumulator)) 
	    else ()
	  done
	)
	;
	!accumulator
      )


let sons_from_sym_rewrittings rew c =
  let accumulator = ref (Scs.empty)
  in
  let aux c (a,b) = (accumulator := (Scs.union (!accumulator) (Scs.union (sons_from_a_rewritting (a,b) c) (sons_from_a_rewritting (b,a) c) ))) 
  in
    Spcs.iter (aux c) rew ; !accumulator


let sons_from_rewrittings rew c =
  let accumulator = ref (Scs.empty)
  in
  let aux c p = (accumulator := (Scs.union (!accumulator) (sons_from_a_rewritting p c)))
  in
    Spcs.iter (aux c) rew ; !accumulator


let all_equivalents rew c =
  let accumulator = ref (Scs.empty) and current_wave = ref (Scs.singleton c) and next_wave = ref (Scs.empty)
  in
  let aux c = (next_wave := (Scs.union !next_wave (sons_from_sym_rewrittings rew c)))
  in  
    (
      while (not (Scs.subset !current_wave !accumulator))
      do 
        next_wave := Scs.empty 
        ;
        Scs.iter aux (!current_wave)
        ;
        accumulator := Scs.union !accumulator !current_wave
        ;
        current_wave := !next_wave      
      done
      ;
      !accumulator 
    )



(* Cette fonction est très inefficace puisqu'elle calcule entièrement la classe de c1 avant de décider *)
(*    *)
let are_equivalent2 rew c1 c2 = 
  Scs.mem c2 (all_equivalents rew c1)


let are_equivalent rew c1 c2 =
  let accumulator1 = ref (Scs.empty) and current_wave1 = ref (Scs.singleton c1) and equivalent = ref false and
      accumulator2 = ref (Scs.empty) and current_wave2 = ref (Scs.singleton c2) and next_wave = ref (Scs.empty) 
  in
  let aux c = (next_wave := (Scs.union !next_wave (sons_from_sym_rewrittings rew c)))
  in  
    (
      while ((not !equivalent) && ((not (Scs.subset !current_wave1 !accumulator1))||(not (Scs.subset !current_wave2 !accumulator2))))
      do 
        next_wave := Scs.empty 
        ;
        Scs.iter aux (!current_wave1)
        ;
        accumulator1 := Scs.union !accumulator1 !current_wave1
        ;
        current_wave1 := !next_wave
	;     
	next_wave := Scs.empty 
        ;
        Scs.iter aux (!current_wave2)
        ;
        accumulator2 := Scs.union !accumulator2 !current_wave2
        ;
        current_wave2 := !next_wave
	;
	equivalent := not (Scs.is_empty(Scs.inter !accumulator1 !accumulator2 ))
      done
      ;
      !equivalent
    )


(*
let a = ("","a","") and b = ("","b","")
in 
let c1 = [|a;b;a;b;a;b;a;b;a;b;a;b;a;b|] and c2 = [|a;a;a;a;a;a;a;b;b;b;b;b;b;b|]
in
let rew = ref (Spcs.empty)
in
let _ = (rew := Spcs.add ([|a;b|],[|b;a|]) !rew)
in
  if (are_equivalent !rew c1 c2) 
  then (print_string "True\n") 
  else (print_string "False\n")

*)

(*
let a = ("","a","") and b = ("","b","")
in 
let c1 = [|a;b;a;a;b|] and c2 = [|a;a;b;b|]
in
let rew = ref (Spcs.empty)
in
let _ = (rew := Spcs.add ([|a;b|],[|b;a|]) !rew)
in
  if (are_equivalent !rew c1 c2) 
  then (print_string "True\n") 
  else (print_string "False\n")

*)

(*
if Scs.is_empty(Scs.empty) then print_string "True\n" else print_string "False\n"

*)

(*
let a = ("","a","") and b = ("","b","") and c = ("","c","")
in
let c1 = [|a;a;a;a|] and c2 = [|b;b;b;b|]
in
let rew = ref (Spcs.empty)
in
let _ = 
  (
    rew := Spcs.add ([|a;b|],[|c|]) !rew
    ;
    rew := Spcs.add ([|b;c|],[|a|]) !rew
    ;
    rew := Spcs.add ([|c;a|],[|b|]) !rew
  )
in
  if (are_equivalent !rew c1 c2) 
  then (print_string "True\n") 
  else (print_string "False\n")

*)

(*
let a = ("","a","") and b = ("","b","") and c = ("","c","")
in 
let c1 = [|c|] and c2 = [|c;c|]
in
let rew = ref (Spcs.empty)
in
let _ = 
(
rew := Spcs.add ([|a;b|],[|c|]) !rew
;
rew := Spcs.add ([|b;c|],[|a|]) !rew
;
rew := Spcs.add ([|c;a|],[|b|]) !rew
)
in
  if (are_equivalent !rew c1 c2) 
  then (print_string "True\n") 
  else (print_string "False\n")

*)

(*
let a = ("","a","") and b = ("","b","") and c = ("","c","")
in 
let c1 = [|a;a|] and c2 = [|b;b;b|]
in
let rew = ref (Spcs.empty)
in
let _ = 
(
rew := Spcs.add ([|a;b|],[|c|]) !rew
;
rew := Spcs.add ([|b;c|],[|a|]) !rew
;
rew := Spcs.add ([|c;a|],[|b|]) !rew
)
in
  if (are_equivalent !rew c1 c2) 
  then (print_string "True\n") 
  else (print_string "False\n")

*)


(* Il faut maintenant faire des tests assez fins, c'est sur ces algorithmes que *)
(* repose tout le reste de la construction de la catégorie des composantes! *)

(* La fonction qui suit prend deux suites composables de la catégorie cat et renvoie "true" lorsque *)
(* ces deux suites sont équivalentes dans cat, "false" dans le cas contraire *)
(* cat est de type flfc, c1 et c2 sont des suites composables *)

(*
let are_equivalent cat c1 c2 = 
  let rec aux accumulated current_wave =
  in
aux Cs.empty (Cs.singleton c1)

*)

(*----------------------------------------------------------------------------*)
