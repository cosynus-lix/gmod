module type CubicalSet =
sig
  type area
  type cube
  type t
  val make : area -> t
  val empty : int -> t
  val face : bool -> int -> int -> cube -> t -> cube
  val degeneracy : int -> int -> t -> t
  val print : t -> unit
  val to_string : t -> string
end

(*
module Overall =
struct
  module IntSet = Common.IntSet
  let array_for_all = Common.Array.for_all
  let initial_segment n = Common.List.initial_segment n
end
*)

(*
module AreaMake = (Area.Make(Common) : 
  (sig 
  
    module OverCube: functor (C:Cube.S with type ints = Common.IntSet.t) -> Sig.CubicalArea 
    with type regular_value = C.regular_value 
    and type generator = C.interval 
    and type point = C.point 
    and type brick = C.t 
    and type ints = C.ints
    
    module OverTore: functor (T:Torus.S) -> Sig.Area
    with type regular_value = T.regular_value 
    and type generator = T.arc 
    and type point = T.point 
    and type brick = T.t
    
    module OverCyle: functor (X:Cylinder.S) -> Sig.Area
    with type regular_value = X.regular_value 
    and type generator = X.generator 
    and type point = X.point 
    and type brick = X.t
  
  end))
*)

(*
module A = (AreaMake.OverCube(Type.C): 
  (Sig.CubicalArea
  with type regular_value = Type.C.regular_value 
  and type generator = Type.C.interval 
  and type point = Type.C.point 
  and type brick = Type.C.t 
  and type ints = Type.C.ints))
*)

(*
module AreaMake = Area.Make(Common)
*)

module CS 
  (C:Cube.S with type ints = Common.IntSet.t) 
  (A:AreaOverCube.S 
  with type regular_value = C.regular_value 
  and type generator = C.interval 
  and type point = C.point 
  and type brick = C.t 
  and type ints = C.ints) 
  :
  (CubicalSet
   with type cube = C.t
   and type area = A.t)
  =
struct

  type cube = C.t

  type area = A.t

  module M = Map.Make(C)

  type t =
      {
        vertices : A.t;
        arrows : A.t;
        relations : A.t;
        source : C.t M.t;
        target : C.t M.t;
        up : C.t M.t;
        down : C.t M.t;
        left : C.t M.t;
        right : C.t M.t
       }

  exception Found
  exception Not_found

  (* Cette fonction lève l'exception Invalid_argument "Index out of bounds" dans
     le cas où elle n'a pas trouvé d'élément répondant au critère demandé par le
     prédicat. *)
  let find_in_array pre arr =
    let found = ref false in
    let i = ref 0 in
      while not !found && !i < Array.length arr do
	if pre arr.(!i) then
	  found := true
	else
          incr i
      done;
      arr.(!i)

  let find_from_set pre set =
    let answer = ref (C.empty 0) in
    try
      A.iter (fun c -> if pre c then (answer := c; raise Found)) set;
      raise Not_found
    with
      | Found -> !answer

  let empty d =
    {
      vertices = A.empty ~d:d () ;
      arrows = A.empty ~d:d () ;
      relations = A.empty ~d:d () ;
      source = M.empty;
      target = M.empty;
      up     = M.empty;
      down   = M.empty;
      left   = M.empty;
      right  = M.empty
     }


  let make a =
    let dim = A.dimension a in
    let aux = A.cset2D_of_complement_of_area a in
    let source_aux = ref M.empty in
    let target_aux = ref M.empty in
    let up_aux     = ref M.empty in
    let down_aux   = ref M.empty in
    let left_aux   = ref M.empty in
    let right_aux  = ref M.empty in
    A.iter
      (fun c0 ->
	for i = 0 to dim-1 do
	  let bi = C.back i c0 in
	  (
	    try
	      let c1 = find_from_set (fun c -> C.is_included c bi) (Common.snd3 aux) in
	      source_aux := M.add c1 c0 !source_aux
	    with
	      | Not_found -> ()
	  );
	  let fi = C.front i c0 in
	  (
	    try
	      let c1 = find_from_set (fun c -> C.is_included c fi) (Common.snd3 aux) in
	      target_aux := M.add c1 c0 !target_aux
	    with
	      | Not_found -> ()
	  )
	done)
      (Common.fst3 aux);
    A.iter
      (fun c1 ->
	for i = 0 to dim - 1 do
	  for j = i + 1 to dim - 1 do
	    let bi = C.back i c1 in
            (
	      try
		let c2 = find_from_set (fun c -> C.is_included c bi) (Common.thd3 aux) in
		left_aux := M.add c2 c1 !left_aux
	      with
		| Not_found -> ()
	    );
	    let fi = C.front i c1 in
            (
	      try
		let c2 = find_from_set (fun c -> C.is_included c fi) (Common.thd3 aux) in
		right_aux := M.add c2 c1 !right_aux
	      with
		| Not_found -> ()
	    );
	    let bj = C.back j c1 in
            (
	      try
		let c2 = find_from_set (fun c -> C.is_included c bj) (Common.thd3 aux) in
		down_aux := M.add c2 c1 !down_aux
	      with
		| Not_found -> ()
	    );
	    let fj = C.front j c1 in
            (
	      try
		let c2 = find_from_set (fun c -> C.is_included c fj) (Common.thd3 aux) in
		up_aux := M.add c2 c1 !up_aux
	      with
		| Not_found -> ()
	    )
	  done
	done)
      (Common.snd3 aux);
    source_aux := M.remove (C.empty dim) !source_aux;
    target_aux := M.remove (C.empty dim) !target_aux;
    up_aux := M.remove (C.empty dim) !up_aux;
    down_aux := M.remove (C.empty dim) !down_aux;
    left_aux := M.remove (C.empty dim) !left_aux;
    right_aux := M.remove (C.empty dim) !right_aux;
    {
      vertices  = Common.fst3 aux ;
      arrows    = Common.snd3 aux ;
      relations = Common.thd3 aux ;
      source    = !source_aux       ;
      target    = !target_aux       ;
      up        = !up_aux           ;
      down      = !down_aux         ;
      left      = !left_aux         ;
      right     = !right_aux
    }

  let face e i n x t = (* failwith "concur2005.CS.face is not implemented yet"  *)
    match (e,i,n) with
      | (false,0,0) -> M.find x t.source
      | (true ,0,0) -> M.find x t.target
      | (false,0,1) -> M.find x t.left
      | (true ,0,1) -> M.find x t.right
      | (false,1,1) -> M.find x t.down
      | (true ,1,1) -> M.find x t.up
      | _  -> raise Not_found

  let degeneracy i j t = failwith "concur2005.CS.degeneracy is not implemented yet"

  let print t =
    let print_set prefixe a =
      print_string (prefixe ^ "\n");
      A.iter (fun c -> print_string ((C.string_of c)^"\n")) a (*t.vertices*)
    in
    let print_map prefixe m =
      print_string (prefixe ^ "\n");
      M.iter (fun k c -> print_string ((C.string_of k)^"-->"^(C.string_of c)^"\n")) m (* t.source *)
    in
    print_set "\nCodimension 0" t.vertices;
    print_set "\nCodimension 1" t.arrows;
    print_set "\nCodimension 2" t.relations;
    print_map "\nSource" t.source;
    print_map "\nTarget" t.target;
    print_map "\nLeft" t.left;
    print_map "\nRight" t.right;
    print_map "\nDown" t.down;
    print_map "\nUp" t.up

  let to_string t =
    let answer = ref "" in
    let add_line s = answer := !answer ^ s in
    let add_set prefixe a =
      add_line (prefixe^"\n");
      A.iter (fun c -> add_line (C.string_of c ^ "\n")) a
    in
    let add_map prefixe m =
      add_line (prefixe ^ "\n");
      M.iter (fun k c -> add_line (C.string_of k ^ " ⟼ " ^ C.string_of c ^ "\n")) m
    in
    add_set "\nCodimension 0" t.vertices;
    add_set "\nCodimension 1" t.arrows;
    add_set "\nCodimension 2" t.relations;
    add_map "\nSource" t.source;
    add_map "\nTarget" t.target;
    add_map "\nLeft" t.left;
    add_map "\nRight" t.right;
    add_map "\nDown" t.down;
    add_map "\nUp" t.up;
    !answer
end
