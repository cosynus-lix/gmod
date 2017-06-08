open AbstractSyntax

module type S =
sig
  type area
  val recursive_mutex        : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val normal_mutex           : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val switch_mutex           : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val switch_semaphore       : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val quantitative_semaphore : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val synchronization        : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val monitor                : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val pile_starvation        : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val pile_overflow          : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val file_starvation        : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val file_overflow          : ?runpro:AbstractSyntax.process array -> AbstractSyntax.t -> area
  val conflict               : ?runpro:AbstractSyntax.process array -> ?out:bool -> ?forbidden:area -> AbstractSyntax.t -> (area option * string list)
end

(* Attention, il faut utiliser locus_higher_than au lieu de
   levelwise_product et penser à donner le niveau de départ *)

module Make
  (I:sig include Interval.S
    val start:regular_value
    val step: regular_value -> regular_value
    val regular_value_to_string: regular_value -> string end)
  (C:Cube.S with type regular_value = I.regular_value and type interval = I.t)
  (AC:AreaOverCube.S with type regular_value = I.regular_value and type brick = C.t)
  (BuzySection:Buzy_section.S with type regular_value = I.regular_value and type area = AC.t)
  (Buzy_matrix:Buzy_matrix.S with type regular_value = I.regular_value and type area = AC.t) =
struct

type area = AC.t

let pile_starvation runpro lifo = (* Coming from starvation *)
  let aux = (Buzy_matrix.ifo runpro lifo) in
  AC.complement (AC.locus_higher_than ~cochain:true ~ground:(snd aux) 0 (fst aux))

let pile_starvation ?runpro syntactical_semantics =
  let runpro =
    match runpro with
      | None -> Interpreter.init_runpro syntactical_semantics
      | Some x -> x
  in
  let lifo = AbstractSyntax.lifo syntactical_semantics in
  AC.compress
    (List.fold_left
       (fun accu (lifo,n) ->
	 Printf.printf "  - treating %s of length %i\n%!" lifo n;
         AC.join (pile_starvation runpro lifo) accu
       )
       (AC.empty ~d:(Array.length runpro) ())
       lifo
    )

let pile_overflow runpro lifo arity = (* Coming from overflow *)
  let aux = Buzy_matrix.ifo runpro lifo in
  AC.locus_higher_than ~cochain:true ~ground:(snd aux) (arity+1) (fst aux)

let pile_overflow ?runpro syntactical_semantics =
  let runpro =
    match runpro with
      | None -> Interpreter.init_runpro syntactical_semantics
      | Some x -> x
  in
  let lifo = AbstractSyntax.lifo syntactical_semantics in
  AC.compress
    (
      List.fold_left
	(fun accu (l, n) ->
	  Printf.printf "Attempt to send a message to a full pile: treating %s of length %i\n%!" l n;
	  AC.join (pile_overflow runpro l (List.assoc l lifo)) accu
	)
	(AC.empty ~d:(Array.length runpro) ())
        lifo
    )

let file_starvation runpro fifo = (* Coming from starvation *)
  let aux = Buzy_matrix.ifo runpro fifo in
  AC.complement
     (AC.locus_higher_than ~cochain:true ~ground:(snd aux) 0 (fst aux))

let file_starvation ?runpro syntactical_semantics =
  let runpro = match runpro with
    | None -> Interpreter.init_runpro syntactical_semantics
    | Some x -> x
  in
  let fifo = AbstractSyntax.fifo syntactical_semantics in
  AC.compress
    (List.fold_left
       (fun accu (f,n) ->
              (*Printf.printf "Treating the file %s of length %i\n%!" fifo n ; *)
	 AC.join
	   (
	     Printf.printf "  - treating %s of length %i\n%!" f n;
	     file_starvation runpro f
	   )
	   accu
       )
       (AC.empty ~d:(Array.length runpro) ())
       fifo)

let file_overflow runpro fifo arity = (* Coming from overflow *)
  let aux = Buzy_matrix.ifo runpro fifo
  in
    AC.locus_higher_than ~cochain:true ~ground:(snd aux) (arity+1) (fst aux)

let file_overflow ?runpro syntactical_semantics =
  let runpro = match runpro with
    | None -> Interpreter.init_runpro syntactical_semantics
    | Some x -> x
  in
  let fifo = AbstractSyntax.fifo syntactical_semantics in
  AC.compress
    (List.fold_left
       (fun accu (f,n) ->
         AC.join
	   (
	     Printf.printf "Attempt to send a message to a full file: treating %s of length %i\n%!" f n ;
	     file_overflow runpro f (List.assoc f fifo)
	   )
	   accu
       )
       (AC.empty ~d:(Array.length runpro) ())
       fifo
    )

let quantitative_semaphore runpro sem arity =
  AC.levelwise_product ~level:arity (Buzy_matrix.quantitative runpro sem arity)

let quantitative_semaphore ?runpro syntactical_semantics =
  let runpro = match runpro with
    | None -> Interpreter.init_runpro syntactical_semantics
    | Some x -> x
  in
  let semq = AbstractSyntax.semaphore `Quantitative syntactical_semantics in
  AC.compress
    (List.fold_left
       (fun accu (s,n) ->
         Printf.printf "  - treating %s\n%!" s;
	 AC.join
	   (quantitative_semaphore runpro s (List.assoc s semq))
	   accu
       )
       (AC.empty ~d:(Array.length runpro) ())
       semq)

let mutex runpro buzy_part mtx =
  AC.generalized_product 2 (Array.map (fun il -> buzy_part mtx il) runpro)

(*-----------------------------------------*)
(* Forbidden area from the recursive mutex *)
(*-----------------------------------------*)

let recursive_mutex ?runpro syntactical_semantics =
  let runpro = match runpro with
    | None -> Interpreter.init_runpro syntactical_semantics
    | Some x -> x
  in
  let mtx = AbstractSyntax.mutex `Recursive syntactical_semantics in
  AC.compress
    (List.fold_left
	(fun accu m ->
          Printf.printf "  - treating %s\n%!" m;
          AC.join (mutex runpro BuzySection.recursive_mutex m) accu)
	(AC.empty ~d:(Array.length runpro) ())
        mtx)

(*--------------------------------------*)
(* Forbidden area from the switch mutex *)
(*--------------------------------------*)

let switch_mutex ?runpro syntactical_semantics =
  let runpro = match runpro with
    | None -> Interpreter.init_runpro syntactical_semantics
    | Some x -> x
  in
  let mtx = AbstractSyntax.mutex `Default syntactical_semantics in
  AC.compress
    (List.fold_left
       (fun accu m ->
         Printf.printf "  - treating %s\n%!" m;
         AC.join (mutex runpro BuzySection.switch_mutex m) accu)
       (AC.empty ~d:(Array.length runpro) ())
       mtx
    )

(*--------------------------------------*)
(* Forbidden area from the normal mutex *)
(*--------------------------------------*)

let normal_mutex ?runpro syntactical_semantics =
  let runpro = match runpro with
    | None -> Interpreter.init_runpro syntactical_semantics
    | Some x -> x
  in
  let mtx = AbstractSyntax.mutex `Normal syntactical_semantics in
  AC.compress
    (List.fold_left
       (fun accu m ->
         Printf.printf "  - treating %s\n%!" m;
         AC.join (mutex runpro BuzySection.normal_mutex m) accu)
       (AC.empty ~d:(Array.length runpro) ())
       mtx
    )

(*-----------------------------------------------------------------------------------*)
(* Calcul de la région interdite générée par les sémaphores à interrupteurs (défaut) *)
(*-----------------------------------------------------------------------------------*)

let switch_semaphore runpro buzy_part sem arity =
    AC.generalized_product arity (Array.map (fun il -> buzy_part sem il) runpro)


let switch_semaphore ?runpro syntactical_semantics =
  let runpro = match runpro with
    | None -> Interpreter.init_runpro syntactical_semantics
    | Some x -> x
  in
  let sem = AbstractSyntax.semaphore `Default syntactical_semantics in
  AC.compress
    (List.fold_left
       (fun accu (s,n) ->
         Printf.printf "  - treating %s of arity %i\n%!" s n;
	 AC.join
	   (switch_semaphore runpro BuzySection.switch_semaphore s (List.assoc s sem))
	   accu
       )
       (AC.empty ~d:(Array.length runpro) ())
       sem
    )

let mutex ?runpro syntactical_semantics =
  let runpro =
    match runpro with
      | None -> Interpreter.init_runpro syntactical_semantics
      | Some x -> x
  in
  let recursive_mutex        = recursive_mutex        ~runpro syntactical_semantics
  and normal_mutex           = normal_mutex           ~runpro syntactical_semantics
  and switch_mutex           = switch_mutex           ~runpro syntactical_semantics
  and switch_semaphore       = switch_semaphore       ~runpro syntactical_semantics
  and quantitative_semaphore = quantitative_semaphore ~runpro syntactical_semantics
  in
  AC.compress
    (
      List.fold_left
	(fun accu a -> AC.join accu a)
	(AC.empty ~d:(Array.length runpro) ())
	[switch_mutex;recursive_mutex;normal_mutex;switch_semaphore;quantitative_semaphore]
    )

(* La fonction ci-dessous n'a pas été testée *)

(* pour avoir la "porte" qui permet de passer le frontière, il
   suffit d'ajouter la partie commune aux barrières, ce qui
   signifie exactement que tous les processus sont en attente
   derrière une même barrière *)

(* On recense tous les hyperplans issus des instructions W(sync). La
   réunion de ces hyperplans est notées F. Si l'arité de la barrière
   est n, on effectue les intersections par paquets de n, la réunion
   de ces intersections est notée A. La région interdite produite
   est la différence ensembliste F\A.*)

let synchronization runpro sync arity =
  let dimension = Array.length runpro in
  let hyperplanes =
    let answer = ref (AC.empty ~d:dimension ()) in
    let () = for pid = 0 to dimension - 1 do
      let aux = ref (AC.empty ~d:dimension ()) in
      let k = ref I.start in
      let () = Array.iter(*iteri*)
        (
          fun (*k*) i ->
            let () = k := I.step !k in
            if fst i = W(sync)
            then aux := AC.add 
              (C.of_array (Array.init dimension (fun n -> if n = pid then I.atom !k else I.full)))
              !aux
        ) runpro.(pid).code in
        answer := AC.join !answer !aux
    done in
    !answer in
  let synchronizations = AC.generalized_common ~d:arity hyperplanes in
  AC.difference hyperplanes synchronizations

let synchronization ?runpro syntactical_semantics =
  let runpro = match runpro with
    | None -> Interpreter.init_runpro syntactical_semantics
    | Some x -> x in
  let sync = AbstractSyntax.synchronization syntactical_semantics in
  let f accu (s,n) = Printf.printf "  - treating %s of arity %i\n%!" s n;
    AC.join accu (synchronization runpro s n) in
  List.fold_left f (AC.empty ~d:(Array.length runpro) ()) sync

(* Pour les moniteurs, chaque instruction M supprime un hyperplan et
   chaque instruction N en ajoute un. *)

let monitor runpro mon =
  let dimension = Array.length runpro in
  let forbidden_hyperplanes =
    let k = ref I.start in
    let answer = ref (AC.empty ~d:dimension ()) in
    (
      for pid = 0 to dimension - 1 do
	let aux = ref (AC.empty ~d:dimension ()) in
	(
	  Array.iter(*iteri*)
	    (fun (*k*) i ->
      let () = k := I.step !k in
		(
		  match i with
		    | (M(j,_),_) ->
		      if j = mon 
          then
            aux :=
              AC.add
              (
                C.of_array
                  (
                    Array.init dimension
                      (
                        fun n ->
                          if n = pid
                          then I.atom (*(k/2)+1*) !k
                          else I.full
                      )
                  )
              )
              !aux
		    | _ -> ()
		)
	    )
	    runpro.(pid).code
	  ;
	  answer := AC.join !answer !aux
	)
      done;
      !answer
    )
  and allowed_hyperplanes =
    let k = ref I.start in
    let answer = ref (AC.empty ~d:dimension ())
    in
    (
      for pid = 0 to dimension-1
      do
	let aux = ref (AC.empty ~d:dimension ()) in
	(
	  Array.iter(*iteri*)
	    (
	      fun (*k*) i ->
		    (* The instructions "notify All" and "Notify"
		       are not distinguished. Both are interreted
		       as "notify All". This limitation is due to
		       combinatorial explosion that happens if one
		       has to take into account all possible cases
		       of individual notification. *)
		if ((fst i) = A(mon)) || ((fst i) = N(mon))
		then
		  aux :=
		    AC.add
		    (
		      C.of_array
			(
			  Array.init dimension
			    (
			      fun n ->
				if n=pid
				then I.atom !k (*(k/2)+1*)
				else I.full
			    )
			)
		    )
		    !aux
		else ()
	    )
	    runpro.(pid).code
	  ;
	  answer := AC.join !answer !aux
	)
      done
      ;
      !answer
    )
  in
  AC.difference forbidden_hyperplanes allowed_hyperplanes

let monitor ?runpro syntactical_semantics =
  let runpro = match runpro with
    | None -> Interpreter.init_runpro syntactical_semantics
    | Some x -> x
  in
  let mon = AbstractSyntax.monitor syntactical_semantics in
  List.fold_left
    (fun accu m ->
      Printf.printf "  - treating %s\n%!" m;
      AC.join accu (monitor runpro m))
    (AC.empty ~d:(Array.length runpro) ())
    mon

(* write_write returns the area of states where two processes are trying to write on the same global variable. *)

let write_write ?(out=false) ?forbidden runpro varname =
  let aux = Buzy_matrix.write runpro varname in
  let dimension = Array.length aux in
  let critical_section = ref (AC.empty ~d:dimension ()) in
  let details = ref [] in
  let forbidden = match forbidden with
    | Some a -> a
    | None -> !critical_section in
  let () = (*Look for situation where two processes try to write on the same common variable*)
    for i = 0 to dimension - 1 do
      for j = i + 1 to dimension - 1 do (*Get the corresponding instruction number*)
        let auxi = aux.(i) in
        let auxj = aux.(j) in
        let ni = Array.length auxi in
        let nj = Array.length auxj in
        for x = 0 to ni - 1 do
          for y = 0 to nj - 1 do
            let cube =
              let cube = Array.make dimension I.full in
              let () = cube.(i) <- I.atom (auxi.(x)) in
              let () = cube.(j) <- I.atom (auxj.(y)) in
              (AC.of_cube (C.of_array cube)) in
            if not (AC.is_included cube forbidden)
            then
              let () = details := Common.Terminal.(Printf.sprintf
                      ("The processes %s %s and %s %s write on the variable %s at the same time.\n")
                      (color White ~bold:true (string_of_int i))
                      (color Cyan (Printf.sprintf "[%s]" (I.regular_value_to_string auxi.(x))))
                      (color White ~bold:true (string_of_int j))
                      (color Cyan (Printf.sprintf "[%s]" (I.regular_value_to_string auxj.(y))))
                      (color Blue ~bold:true varname)
              ) :: !details in
              if out then critical_section := AC.join !critical_section cube
          done
        done
      done
    done in
    (!critical_section,!details)
  

(* read_write returns the area of states where a process is trying to
   read a global variable while another process is trying to write on
   it. *)

let read_write ?(out=false) ?forbidden runpro varname =
  let auxr = Buzy_matrix.read runpro varname in
  let auxw = Buzy_matrix.write runpro varname in
  let dimension = Array.length auxr in (* doit être égal à (Array.length auxw) *)
  let critical_section = ref (AC.empty ~d:dimension ()) in
  let details = ref [] in
  let forbidden = match forbidden with
    | Some a -> a
    | None -> AC.empty ~d:dimension () in
  (
  let () =
    for i = 0 to dimension - 1 do
      for j = 0 to i - 1 do
        let auxi = auxr.(i) in
        let auxj = auxw.(j) in
        let ni = Array.length auxi in
        let nj = Array.length auxj in
        for x = 0 to ni - 1 do
          for y = 0 to nj - 1 do
            let cube =
              let cube = Array.make dimension I.full in
              let () = cube.(i) <- I.atom (auxi.(x)) in
              let () = cube.(j) <- I.atom (auxj.(y)) in
              (AC.of_cube (C.of_array cube)) in
            if not (AC.is_included cube forbidden) then
              let () = details := Common.Terminal.(Printf.sprintf
                   ("The process %s %s reads the variable %s while the process %s %s write on it.\n")
                   (color White ~bold:true (string_of_int i))
                   (color Cyan (Printf.sprintf "[%s]" (I.regular_value_to_string auxi.(x))))
                   (color Blue ~bold:true varname)
                   (color White ~bold:true (string_of_int j))
                   (color Cyan (Printf.sprintf "[%s]" (I.regular_value_to_string auxj.(y))))
                ) :: !details in
              if out then critical_section := AC.join !critical_section cube
          done
        done
      done;
      for j = i + 1 to dimension - 1 do
        let auxi = auxr.(i) in
        let auxj = auxw.(j) in
        let ni = Array.length auxi in
        let nj = Array.length auxj in
        for x = 0 to ni - 1 do
          for y = 0 to nj - 1 do
            let cube =
              let cube = Array.make dimension I.full in
              let () = cube.(i) <- I.atom (auxi.(x)) in
              let () = cube.(j) <- I.atom (auxj.(y)) in
              (AC.of_cube (C.of_array cube)) in
            if not (AC.is_included cube forbidden) then
              let () = details := Common.Terminal.(Printf.sprintf
                 ("The process %s %s reads the variable %s while the process %s %s write on it.\n")
                 (color White ~bold:true (string_of_int i))
                 (color Cyan (Printf.sprintf "[%s]" (I.regular_value_to_string auxi.(x))))
                 (color Blue ~bold:true varname)
                 (color White ~bold:true (string_of_int j))
                 (color Cyan (Printf.sprintf "[%s]" (I.regular_value_to_string auxj.(y))))
                ) :: !details in
              if out then critical_section := AC.join !critical_section cube
          done
        done
      done
    done in
    !critical_section, !details
  )

let conflict ?runpro ?(out=false) ?forbidden syntactical_semantics =
  let runpro = match runpro with
    | None -> Interpreter.init_runpro syntactical_semantics
    | Some x -> x
  in
  let forbidden = match forbidden with
    | Some a -> a
    | None -> (AC.empty ~d:(Array.length runpro) ())
  in
  let printable_list_of_conflicts = ref [] in
  let conflict_area =
    AC.join
      (List.fold_left
	 (fun accu var ->
	   let aux = write_write ~out:out ~forbidden:forbidden runpro var in
	   printable_list_of_conflicts := (!printable_list_of_conflicts)@(snd aux);
	   AC.join (fst aux) accu
	 )
	 (AC.empty ~d:(Array.length runpro) ())
	 syntactical_semantics.AbstractSyntax.variables
      )
      (List.fold_left
	 (fun accu var ->
	   let aux = read_write ~out:out ~forbidden:forbidden runpro var in
	   printable_list_of_conflicts:= (!printable_list_of_conflicts)@(snd aux);
	   AC.join (fst aux) accu
         )
         (AC.empty ~d:(Array.length runpro) ())
         syntactical_semantics.AbstractSyntax.variables
      )
  in
  (if out then Some conflict_area else None),
  !printable_list_of_conflicts

end
