(* Ce fichier n'a pas encore été testé ! *)
(* Les fonctions sont écrites en considérant que b est l'ensemble des points du modèle géométrique *)
(* mais on peut aussi effectuer des modifications de façon à obtenir quelque chose qui permet de *)
(* travailler avec b qui représente le complémentaire de b *)

(*
open Type

*)

module Soi = Set.Make
  (
    struct 
      type t = int 
      let compare = Pervasives.compare 
    end
  )


exception Do_not_compose


(* L'idée est qu'un ensemble de point de R^n est un modèle géométrique dans lequel *)
(* l'avancement d'un processeur est marqué par l'incrémentation d'une seule dimension. *)

module Block = Set.Make
(
struct
  type t = int array
  let compare t1 t2 = Pervasives.compare t1 t2
end
)


(* Le modèle précédent peut être "recouvert" de façon à faciliter les calculs d'homotopies dirigées. *)

module Covering = Set.Make(Block)


(* Un (di)chemin dirigé est décrit par un point de départ et un tableau dont le i-ème terme indique *)
(* quel est le (numéro du) processus qui effectue la i-ème action *)
(* Le type Glob.elt représente donc des (di)chemins et Glob.t des ensembles de (di)chemins qui peuvent *)
(* ainsi typiquement être des classes de dihomotopie. *)

module Glob = Set.Make
(
struct
  type t = (int array) * (int array)
  let compare t1 t2 = Pervasives.compare t1 t2
end
)


let print_int_array t = 
  print_string "[ " ;
  Array.iter (fun x -> print_string((string_of_int x)^" ")) t ;
  print_string "]"


(* le n-ème point du chemin *)
(* attention, le point d'indice 0 est le premier, autrement dit celui qui est donné par *)
(* orgn tandisque pth définit un chemin dirigé qui part de l'origine (orgn). *)

let dipath_nth_pnt n orgn pth =
  if (n = 0) 
  then 
    orgn 
  else
    (
      let p = (Array.copy orgn)
      in
	(
          for i = 0 to (n - 1) 
          do
	    let to_increase = pth.(i)
	    in
	      p.(to_increase) <- (p.(to_increase))+1 
	  done
	  ;
	  p
	)
    )


(* L'ensemble des points par lesquels passent un chemin dirigé *)
(* cp:current point *)

let trace orgn pth =
  let accu = ref Block.empty and cp = Array.copy orgn
  in
    accu := Block.add cp !accu ;
    for i = 0 to ((Array.length pth) - 1)
    do
      (
	let to_increase = pth.(i)
	in
	  cp.(to_increase) <- (cp.(to_increase)+1) 
      )
      ;
      accu := Block.add cp !accu
    done
    ;
    accu


(* renvoie le point où s'arrête le dichemin pth *)

let dipath_end orgn pth = dipath_nth_pnt (Array.length(pth)) orgn pth


(* p and b stand for point and block *)
(* cette fonction renvoie l'ensemble des points successeurs d'un point de b *)
(* où b représente un modèle géométrique *)

let successor_points p b = 
  let accu = ref Block.empty
  in
    for i=0 to ((Array.length p)-1)
    do 
      (
	p.(i) <- p.(i)+1 ;
	if (Block.mem p b) 
	then 
	  accu := Block.add p (!accu)
	else 
	  () 
	;
	p.(i) <- p.(i)-1 (* pour laisser p inchangé *)
      )
    done
    ;
    accu


(* Cette fonction est une reprise de la précédente à cela près qu'elle ne donne pas *)
(* l'ensemble des points successeurs de p mais l'ensemble des (numéros des) processus *)
(* qui peuvent avancer à partir de p. *)

let possible_next_moves p b = 
  let accu = ref Soi.empty
  in
    for i=0 to ((Array.length p)-1)
    do 
      (
	p.(i) <- p.(i)+1 
        ;
	if (Block.mem p b)
	then 
          accu := Soi.add i (!accu)
	else 
	  () 
	;
	p.(i) <- p.(i)-1 (* pour laisser p inchangé *)
      )
    done
    ;
    accu


(* Cette fonction renvoie toutes les extensions élémentaires d'un dichemin donné *)
(* Une extension est dite élémentaire lorsqu'elle allonge le chemin d'une "unité" *)

let elementary_extensions orgn pth b =
  let pnm = possible_next_moves (dipath_end orgn pth) b and answer = ref Block.empty
  in
  let extend n = (answer := (Block.add (Array.append pth [|n|]) !answer))
  in
    (
      Soi.iter extend !pnm
      ;
      answer
    )


(* Cette fonction est un effet de bord qui permute les termes i et i+1 d'un tableau *)

let commute a i =
  let n = a.(i)
  in
    (
      a.(i) <- a.(i+1) ;
      a.(i+1) <- n
    )


(* i:int, orgn:origine, pth:(di)path, b:block, bdh:block of dihomotopy *)
(* le point de départ étant fixé, on ne s'intéresse qu'au "parcourt effectué" *)
(* Attention, la fonction qui suit est dans doute boguée *)
(* Cette fonction est un effet de bord...elle modifie bdh *)

let add_elementary_dihomotopy i orgn pth b bdh = 
  let pth' = pth 
  in
  let _ = commute pth' i
  in 
    (
      if Block.mem (dipath_nth_pnt (i+1) orgn pth') b
      then 
	bdh := Block.add pth' !bdh
      else 
	()
    )


(* La même en travaillant sur les "nappes" *)

let add_elementary_dihomotopy_wave i orgn pth b wdh =
  let pth' = Array.sub pth 0 i
  in
  let _ = pth'.(i) <- pth.(i+1)
  in
  let pnt = (dipath_nth_pnt (i+1) orgn pth')
  in
    (
      if Block.mem pnt b
      then
	wdh := Block.add pnt !wdh
      else
	()
    )


(* 
   Cette fonction est basée sur la conjecture suivante : tout chemin
   dirigé pth dont l'image est incluse dans celle d'une dihomotopie H et
   tel que H(0,0)=pth(0) et H(0,1)=pth(1) est lui même dihomotope à H(0,)
   et H(1,). Cette conjecture généralise le fait que deux dichemins sur
   un pospace ayant la même image sont nécessairement dihomotopes.
*)

let in_the_wave w orgn pth =
  ()
 

(* b:allowed block: l'ensemble des points autorisés *)
(* g:glob:c'est-à-dire un ensemble de chemin qui ont le même point de départ et d'arrivée *)
(* ag:accumulator glob: ... *)
(* ng:next glob: ... *)
(* dw:dihomotopy wave:la "nappe" qui contient tous les points par lesquels passent un dichemin *)
(* dihomotope à l'un des dichemins appartenant au glob *)
(* elle est construite au fur et à mesure *)
(* Cette fonction doit donc renvoyer un block. *)

let dihomotopy_wave b g = 
  let dw = ref Block.empty and ag = ref Block.empty and ng = ref g
  in
    (

    )


(* Ce qui suit est en chantier...

(* Rend l'ensemble des chemins dihomotopes à un chemin pth donné *)

let dihomotopy_class p b =
  let current_glob = ref Glob.singleton p
  in
  let next_glob = ref Glob.empty
  in
    (
      if (Glob.subset next_glob current_glob) 
      then () 
      else
	(

	)	
    )


let dihomotopic p1 p2 b =
  let current_glob = ref Glob.singleton p1
  in
  let next_glob = ref Glob.empty
  in
    (
      next_glob := (*---*) ;
      current_glob := Glob.union (!next_glob) (!current_glob)
    )


*)


