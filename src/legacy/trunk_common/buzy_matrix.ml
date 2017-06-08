open AbstractSyntax

module type S =
sig
  type regular_value
  type area
  val ifo: AbstractSyntax.process array -> string -> area array array * (int -> int)
  val quantitative: AbstractSyntax.process array -> string -> int -> area array array
  val write: AbstractSyntax.process array -> string -> regular_value array array
  val read: AbstractSyntax.process array -> string -> regular_value array array
end

module Make
  (AC:AreaOverCube.S)
  (BuzySection:Buzy_section.S with type regular_value = AC.regular_value and type area = AC.t):
  (S with type regular_value = AC.regular_value and type area = AC.t) =

struct
type regular_value = AC.regular_value
type area = AC.t

(* Chaque ligne du tableau contient les numéros d'instruction un booléen qui prend la valeur (true) ssi 
l'instruction qui correspond écrit sur la variable (varname) *)

let write runpro varname = 
  let dimension = Array.length runpro in
  Array.init dimension (fun k -> BuzySection.write varname (runpro.(k)))

(* Chaque ligne du tableau contient les numéros d'instruction un booléen qui prend la valeur
   (true) ssi l'instruction qui correspond écrit sur la variable
   (varname) *)

let read runpro varname = 
  let dimension = Array.length runpro in 
  Array.init dimension (fun k -> BuzySection.read varname (runpro.(k)))

(* Pour chaque ligne i, l'entrée d'indice j-1 (j allant de 1 à arity) contient la région qui occuppe au moins j occurrences du 
smaphore sem *)

let quantitative runpro sem arity =
  let dimension = Array.length runpro in
  let answer = Array.make_matrix dimension arity (* 0 *) (AC.empty ~d:1 ())
  in
    for i = 0 to dimension - 1
    do 
      for j = 1 to arity
      do
        answer.(i).(j - 1) <- BuzySection.quantitative_semaphore_levelwise 
				  sem arity j runpro.(i)
      done
    done;
    answer

let print_all_entries m g = 
  for i=0 to (Array.length m) - 1
  do
    Printf.printf "Ground level of line %i is %i\n& " i (g i);
    for j=0 to (Array.length (m.(i))) - 1
    do Printf.printf " M(%i)(%i) = %s &" i (j + (g i) + 1) (AC.string_of ~gui:false m.(i).(j))
    done;
    print_endline ""
  done

let ifo runpro ifo =
  let dimension = Array.length runpro in
  let ground = ref [||] in
  let answer = ref [||] in
  (
    let a_line = ref [||] in
    let j = ref 1 in
    for i = 0 to dimension-1
    do 
      (
	a_line := [||];
	j := 1 (*0*);		
	let aux = ref (BuzySection.ifo_levelwise ifo !j runpro.(i))
	in
	while AC.is_not_empty !aux
	do
	  a_line := Array.append !a_line [|!aux|];
	  incr j;
	  aux := (BuzySection.ifo_levelwise ifo !j runpro.(i))
	done;
	j := 0 (*-1*);
	let aux = ref (BuzySection.ifo_levelwise ifo !j runpro.(i))
	in
	while (AC.is_not_full !aux)
	do
	  a_line := Array.append [|!aux|] !a_line;
	  decr j;
	  aux := (BuzySection.ifo_levelwise ifo !j runpro.(i))
	done;
	if !a_line <> [||] (* c'est quand même un hack pour générer une exception *)
	then
	  (
	    ground := Array.append (!ground) [|!j|]; (* fonction du signe de n *)
	    answer := Array.append !answer [|!a_line|]
	  )
	else (* ssi ifo n'apparaît pas dans le processus d'indice i *)
	  (
	    ground := Array.append (!ground) [|-1|];
	    answer := Array.append !answer [|[|AC.full ~d:1 ()|]|]
	  )
      )
    done;
    (
      !answer,
      (
	fun i -> 
	  (try (!ground).(i)
	   with _ -> 0)
      )
    )
  )

end(*Make*)
