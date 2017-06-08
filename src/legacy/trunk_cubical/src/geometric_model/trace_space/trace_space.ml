open Common
open Type

module IntPair = struct
  type t = int * int
  let compare = compare
  let to_string (x,y) = Printf.sprintf "(%d, %d)" x y
end

module IPHG = Hypergraph.Make(IntPair)

(** Boolean vector. *)
module BVec = Boolean.Vector
(** Map over boolean vectors. *)
module BVecMap = Map.Make(BVec)
module BMat = Boolean.Matrix
module BMatSet = Set.Make(BMat)

(** Ouput an n-fold list product with itself. *)
let rec list_exponent list n =
  let _product list list_of_lists =
    List.concat
      (List.map
   (fun y -> List.map (fun z -> z::y) list)
   list_of_lists) in
  if n == 0 then
    [[]]
  else
    _product list (list_exponent list (n-1))

(** Given a cube and a coordinate dimension, output the extended cube in all
    other dimensions. *)
let extend_brick cube dimension =
  let intervalList = Type.C.to_list cube in
  let intervalList2 = List.mapi
    (fun d x ->
      if d != dimension then
  Type.I.downward x
      else
  x)
    intervalList
  in
  Type.C.of_list intervalList2

(** Given a list of cubes and dimensions for each, return the list of cubes with
    their extensions. *)
let extend_cube_list_in_multiple_dimensions brickList dirs =
  let newBrickList = ref brickList in
  let brickList = Array.of_list brickList in
  BMat.iter
    (fun int_pair ->
      newBrickList := (extend_brick (brickList.(fst int_pair)) (snd int_pair)) :: !newBrickList
    )
    dirs;
  !newBrickList

(** Predicate to check if a given cubeList, with extension directions, generates
    a dead area. *)
let isDead cubeList matrix =
  let newCubeList = extend_cube_list_in_multiple_dimensions cubeList matrix in
  let area = Type.AC.complement (Type.AC.make newCubeList) in
  (* The try with is because sometimes the area is empty. *)
  try
    not (
      Type.AC.belongs_to
        (Type.AC.glb area)
        (Type.AC.might_go_infinity_in_all_dimensions area)
    )
  with _ -> true

(** Print a string representation of a set of boolean matrices. *)
let string_of_bmats ms =
  (*
  let string_of_bmat m =
    "[" ^ String.concat ", " (List.map (fun (x,y) -> Printf.sprintf "(%d, %d)" x y) (Enum.to_list (BMat.enum m))) ^ "]"
  in
  *)
  let string_of_bmat m = BMat.to_string m in
  String.concat "\n" (List.map string_of_bmat (BMatSet.elements ms))

(** true if a matrix represents an empty simploid, false otherwise. This checks
    if there is a row filled with zeros. *)
let empty_simploid matrix =
  try
    BMat.iter_rows (fun v -> if BVec.is_empty v then raise Exit) matrix;
    false
  with
    | Exit -> true

let connected m1 m2 = not (empty_simploid (BMat.inter m1 m2))

let bmat_components ms l =
  let is_in_component m c = List.exists (connected m) c in
  let components = ref [] in
  BMatSet.iter
    (fun m ->
      let cur = ref [m] in
      components :=
        List.filter
        (fun c ->
          if is_in_component m c then
            (
              cur := c @ !cur;
              false
            )
          else
            true
        ) !components;
      components := !cur :: !components
    ) ms;
  List.map (fun c -> List.hd c) !components

(** Find the matrix coordinates in which a forbidden cube touches the
    boundary. *)
let boundary_coordinates n cubes =
  let l = Array.length cubes in
  let ans = ref (BMat.create l n) in
  let add xy = ans := BMat.add xy !ans in
  for i = 0 to l - 1 do
    let cube = cubes.(i) in
    for j = 0 to n - 1 do
      if Type.I.belongs_to 0 cube.(j) then add (i,j)
    done
  done;
  !ans

(** This is a slow and simple algorithm which uses the isDead function. *)
let analyze1_part1_version1 cubeList inter_dims n =
  let l = List.length cubeList in
  let column_matrices' = list_exponent (List.init l (fun n -> n)) n in
  let column_matrices =
    BMatSet.of_list
      (List.map
         (fun ilist ->
           BMat.of_list l n (List.mapi (fun i j -> (j, i)) ilist)
         )
         column_matrices')
  in
  let boundary_column_matrices =
    BMatSet.map (fun ips -> BMat.diff ips inter_dims) column_matrices
  in
  let deadMatrices =
    BMatSet.filter
      (fun x -> isDead cubeList x)
      boundary_column_matrices
  in
  deadMatrices

(* Version of part 1 described in Raussen's paper. *)
let analyze1_part1_version2
    cubes (* forbidden cubes *)
    inter_dims (* coordinates in the matrix where the cube touches the boundary *)
    n (* number of processes *)
    =
  let l = Array.length cubes in

  (** Upper bound for unbounded intervals. *)
  let infty = max_int in

  let get_b_ij i j =
    try
      Type.I.lub cubes.(i).(j)
    with _ -> infty
  in

  (** The upper bound of a cube. *)
  let get_b i =
    Array.map
      (fun interval -> try (Type.I.lub interval) with _ -> infty)
      cubes.(i)
  in

  (** The lower bound of a cube. *)
  let get_a k j =
    Type.I.glb cubes.(k).(j)
  in

  (** Componentwise min on integer vectors. *)
  let vector_min v1 v2 =
    Array.map2 min v1 v2
  in

  (*
  (** The cube indices such that the j-th glb is below a given b_value. *)
  let c j b_value =
    BVec.of_list
      (List.may_mapi
         (fun i cube ->
           if Type.I.glb cube.(j) < b_value then
             Some i
           else
             None
         )
         (* TODO: remove this conversion *)
         (Array.to_list cubes)
      )
  in
  *)

  (** Returns a map wich to every boolean vector associates [init i] if the vector
      is a "singleton" with i as only non-null value, and [update key value i] to
      (key ⊎ i) if the cardinal of key is < n. *)
  let increment_over_powerset update init =
    let ans = ref BVecMap.empty in
    for i = 0 to l - 1 do
      let tmp =
        BVecMap.fold
    (fun key value accu ->
      if BVec.cardinal key < n then
        BVecMap.add (BVec.add i key) (update key value i) accu
      else
              accu
    )
    !ans
    !ans
      in
      ans := BVecMap.add (BVec.singleton l i) (init i) tmp
    (* TODO: why is this slower than above? *)
    (* TODO: whould this be faster with an in place data structure? *)
    (*
      BVecMap.iter
      (fun key value ->
      if BVec.cardinal key < n then
      ans := BVecMap.add (BVec.add i key) (f key value i) !ans
      ) !ans;
      ans := BVecMap.add (BVec.singleton l i) (g i) !ans
    *)
    done;
    !ans
  in

  let bold_b =
    let update key old_value i = vector_min old_value (get_b i) in
    let init = get_b in
    increment_over_powerset update init
  in

  let bold_b ii =
    BVecMap.find ii bold_b
  in

  (*
  let lazy_increment_over_powerset update init =
    let bvm = ref BVecMap.empty in
    let add ii v = bvm := BVecMap.add ii v !bvm in
    let rec ans ii =
      try
        BVecMap.find ii !bvm
      with
        | Not_found ->
          let i = BVec.pick ii in
          let ii' = BVec.rm i ii in
          let v =
            if BVec.is_null ii' then
              init i
            else
              update ii' (ans ii') i
          in
          add ii v;
          v
    in
    ans
  in
  *)

  (*
  let bold_b =
    let update key old_value i = vector_min old_value (get_b i) in
    let init = get_b in
    lazy_increment_over_powerset update init
  in
  *)

  let r =
    let update j old_key old_value k =
      if (bold_b old_key).(j) < get_a k j then
        old_value
      else if get_b_ij k j<= (bold_b old_key).(j) then
        let b_kj = get_b_ij k j in
        BVec.add k
          (BVec.filter (fun i -> get_a i j < b_kj) old_value)
          (*
          (BVec.inter
             (c j (get_b_ij k j))
             old_value)
          *)
      else
        BVec.add k old_value
    in
    let init = BVec.singleton l in
    Array.init n (fun j -> increment_over_powerset (update j) init)
  in

  let r j ii =
    BVecMap.find ii r.(j)
  in

  (*
  let r j ii =
    let update j old_key old_value k =
      if (bold_b old_key).(j) < get_a k j then
        old_value
      else if (get_b_ij k j) <= (bold_b old_key).(j) then
        BVec.add k
          (BVec.inter
             (c j (get_b_ij k j))
             old_value)
      else
        BVec.add k old_value
    in
    let init = BVec.singleton in
    let r = Array.init n (fun j -> lazy_increment_over_powerset (update j) init) in
    r.(j) ii
  in
  *)

  (** Set of non-null rows. *)
  (* TODO: use BVec.is_null *)
  let row_set matrix =
    BMat.fold (fun (a,b) v -> BVec.add a v) matrix (BVec.empty l)
  in

  (** Set of non-null columns. *)
  let column_set matrix =
    BMat.fold (fun (a,b) v -> BVec.add b v) matrix (BVec.empty n)
  in

  (** Set of null columns. *)
  let zero_columns matrix =
    BVec.diff
      (BVec.full n)
      (column_set matrix)
  in

  (** Is a matrix dead? *)
  let is_dead matrix =
    let zero = zero_columns matrix in
    let rows = row_set matrix in
    let condition2 =
      BMat.for_all
        (fun (i,j) -> BVec.mem i (r j rows))
        matrix
    in
    let condition1 =
      BVec.for_all
        (fun j ->
          try
            (bold_b rows).(j) = infty
          with _ -> false)
        zero
    in
    condition1 & condition2
  in

  (** Integer exponentiation a^b. *)
  let rec pow_int a b =
    let ans = ref 1 in
    for i = 0 to b - 1 do
      ans := a * !ans
    done;
    !ans
  in

  (** Enumeration of the column matrices. *)
  let column_matrix_enum =
    Enum.map
      (fun number ->
        let m = ref (BMat.create l n) in
        for i = 0 to n - 1 do
          m := BMat.add ((number / pow_int l i) mod l, i) !m
        done;
        !m
      )
      (Enum.indexes (pow_int l n))
  in

  (** Enumeration of the column matrices excepting the ones touching the boundary. *)
  let boundary_column_matrix_enum =
    Enum.map (fun m -> BMat.diff m inter_dims) column_matrix_enum
  in

  (** Dead matrices. *)
  let dead_matrices =
    Enum.filter
      is_dead
      boundary_column_matrix_enum
  in

  BMatSet.of_enum dead_matrices




(*
let analyze1_part1_version3
    cubes (* forbidden cubes *)
    inter_dims (* coordinates in the matrix where the cube touches the boundary *)
    n (* number of processes *)
    =
  let l = Array.length cubes in

  (** Upper bound for unbounded intervals. *)
  let infty = max_int in

  let y =
    let lub int = try Type.I.lub int with _ -> infty in
    let lub = Array.map (fun ints -> Array.map lub ints) cubes in
    fun i j -> lub.(i).(j)
  in

  let yy i = Array.init n (y i) in

  (** The lower bound of a cube. *)
  let x =
    let glb = Array.map (fun ints -> Array.map Type.I.glb ints) cubes in
    fun i j -> glb.(i).(j)
  in

  let dead = ref BMatSet.empty in

  (*
    for i = 0 to l - 1 do
    for j = 0 to n - 1 do
    Printf.printf "(%d,%d) : %d - %d\n%!" i j (x i j) (y i j)
    done
    done;
  *)

  let increment_over_powerset update init =
    let ans = ref BVecMap.empty in
    for i = 0 to l - 1 do
      let tmp =
        BVecMap.fold
    (fun key value accu ->
      if BVec.cardinal key < n then
        BVecMap.add (BVec.add i key) (update key value i) accu
      else
              accu
    )
    !ans
    !ans
      in
      ans := BVecMap.add (BVec.singleton l i) (init i) tmp
    done;
    !ans
  in

  let vector_min v1 v2 =
    Array.map2 min v1 v2
  in

  let bold_b =
    let update key old_value i = vector_min old_value (yy i) in
    let init = yy in
    increment_over_powerset update init
  in

  let bold_b ii =
    BVecMap.find ii bold_b
  in

  let r =
    let update j old_key old_value k =
      if (bold_b old_key).(j) < x k j then
        old_value
      else if y k j <= (bold_b old_key).(j) then
        let b_kj = y k j in
        BVec.add k (BVec.filter (fun i -> x i j < b_kj) old_value)
      else
        BVec.add k old_value
    in
    let init = BVec.singleton l in
    Array.init n (fun j -> increment_over_powerset (update j) init)
  in

  let r j ii =
    BVecMap.find ii r.(j)
  in

  let rec indexes
      j (* column to fill *)
      m (* matrix with begining already filled up to column j *)
      rows (* I = R(M) *)
      y_rows (* y_rows.(j) is the min of y i j for i in rows *)
      bounds (* dimensions in which the boundary is touched *)
      =
    if j = n then
      (
        if not (BMat.is_empty m) then
          dead := BMatSet.add m !dead
      )
    else
      for i = 0 to l - 1 do
        (* Printf.printf "index: %d %d\n%!" i j; *)
        try
          if BMat.mem (i,j) inter_dims then
            let bounds = BVec.add j bounds in
            BVec.iter (fun i -> if y i j <> infty then raise Exit) rows;
            indexes (j+1) m rows y_rows bounds
          else
            let m' = BMat.add (i,j) m in
            let changed_rows = not (BVec.mem i rows) in
            let rows = BVec.add i rows in
            if not (BVec.mem i (r j rows)) then raise Exit;
            if changed_rows then
              (
                BVec.iter (fun j -> if y i j <> infty then raise Exit) bounds;
                if y_rows.(j) < x i j then raise Exit;
                BMat.iter
                  (fun (i',j) ->
                    let yij = y i j in
                    (* assert (yij <> y_rows.(j)); *)
                    if yij < y_rows.(j) && not (x i' j < yij) then raise Exit
                  ) m
              );
            let y_rows =
              if changed_rows then
                Array.mapi (fun j yrj -> min yrj (y i j)) y_rows
              else
                y_rows
            in
            indexes (j+1) m' rows y_rows bounds
        with
          | Exit -> ()
      done
  in
  indexes 0 (BMat.create l n) (BVec.create l) (Array.create n infty) (BVec.create n);
  !dead
*)


let analyze1_part1_version4
    cubes (* forbidden cubes *)
    inter_dims (* coordinates in the matrix where the cube touches the boundary *)
    n (* number of processes *)
    =
  let l = Array.length cubes in

  (** Upper bound for unbounded intervals. *)
  let infty = max_int in

  let y =
    let lub int = try Type.I.lub int with _ -> infty in
    let lub = Array.map (fun ints -> Array.map lub ints) cubes in
    fun i j -> lub.(i).(j)
  in

  (** The lower bound of a cube. *)
  let x =
    let glb = Array.map (fun ints -> Array.map Type.I.glb ints) cubes in
    fun i j -> glb.(i).(j)
  in

  let dead = ref BMatSet.empty in

  (*
    Printf.printf "\n%!";
    for i = 0 to l - 1 do
    for j = 0 to n - 1 do
    Printf.printf "(%d,%d) : %d - %d\n%!" i j (x i j) (y i j)
    done
    done;
  *)

  let rec indexes
      j (* column to fill *)
      m (* m.(j) = chosen row i (or None) *)
      rows (* non-null rows (up to now) *)
      y_rows (* y_rows.(j) is the min of y i j for i in rows *)
      =
    if j = n then
      (
        let mat = ref (BMat.create l n) in
        Array.iteri (fun j -> function Some i -> mat := BMat.add (i,j) !mat | None -> ()) m;
        let m = !mat in
        if not (BMat.is_empty m) then dead := BMatSet.add m !dead
      )
    else
      for i = 0 to l - 1 do
        (* Printf.printf "index: %d %d\n%!" i j; *)
        try
          let changed_rows = not (BVec.mem i rows) in
          let rows = BVec.add i rows in
          let m = Array.copy m in
          if BMat.mem (i,j) inter_dims then
            m.(j) <- None
          else
            m.(j) <- Some i;
          (
            match m.(j) with
              | Some i -> if x i j >= y_rows.(j) then raise Exit
              | None -> if y_rows.(j) <> infty then raise Exit
          );
          let j' = j in
          let y_rows =
            if changed_rows then
              Array.mapi
                (fun j yrj ->
                    (* min yrj (y i j) *)
                  let yij = y i j in
                  if yrj <= yij then
                    yrj
                  else
                    match m.(j) with
                      | None ->
                          (* Boundary *)
                        if j <= j' && yij <> infty then raise Exit;
                        yij
                      | Some i ->
                        if x i j >= yij then raise Exit;
                        yij
                ) y_rows
            else
              y_rows
          in
          indexes (j+1) m rows y_rows
        with
          | Exit -> ()
      done
  in
  indexes 0 (Array.make n None) (BVec.create l) (Array.create n infty);
  !dead



(** Naive algorithm for computing dead matrices. *)
let analyze1_part2_version1
    deads (* dead matrices *)
    inter_dims (* coordinates in the matrix where the cube touches the boundary *)
    l (* number of holes *)
    n (* number of processes *)
    =
  let live = ref (BMatSet.singleton (BMat.full l n)) in
  BMatSet.iter
    (fun d ->
      BMatSet.iter
        (fun m ->
          if BMat.included d m then
            (
              live := BMatSet.remove m !live;
              BMat.iter
                (fun ij ->
                  let m = BMat.remove ij m in
                  try
                    (* Remove this to get maximal matrices only. *)
                    if false then
                    live :=
                      BMatSet.filter
                      (fun n ->
                        if BMat.included m n then raise Exit;
                        not (BMat.included n m))
                      !live;
                    live := BMatSet.add m !live
                  with
                    | Exit -> ()
                )
                d
            )
        ) !live
    ) deads;
  let ans = ref BMatSet.empty in
  BMatSet.iter
    (fun m ->
      let m = BMat.diff m inter_dims in
      if not (empty_simploid m) then ans := BMatSet.add m !ans
    )
    !live;
  !ans

(** Hypergraph algorithm, taking as input a list of dead column matrices. *)
let analyze1_part2_version2
    deads (* dead matrices *)
    inter_dims (* coordinates in the matrix where the cube touches the boundary *)
    l (* number of holes *)
    n (* number of processes *)
    =
  let all_ones = BMat.full l n in
  let hypergraph =
    BMatSet.fold
      (fun dead hypergraph ->
        IPHG.add_edge
          (IPHG.E.of_enum (BMat.enum dead))
          hypergraph
      )
      deads
      IPHG.empty
  in
  let trans = IPHG.transversal hypergraph in
  let liveMatrices_without_boundaries =
    BMatSet.of_enum
      (Enum.map
         (fun edge -> BMat.diff all_ones (BMat.of_enum l n (IPHG.E.enum edge)))
         (IPHG.ES.enum trans)
      )
  in
  let liveMatrices =
    BMatSet.map
      (fun m -> BMat.diff m inter_dims)
      liveMatrices_without_boundaries
  in
  liveMatrices

(*
(** Direct implementation of Berger algorithm. *)
let analyze1_part2_version3
    deads (* dead matrices *)
    inter_dims (* coordinates in the matrix where the cube touches the boundary *)
    l (* number of holes *)
    n (* number of processes *)
    =
  let ans = ref (BMatSet.singleton (BMat.full l n)) in
  for i = 0 to Array.length deads - 1 do
    let old = !ans in
    ans := BMatSet.empty;
    BMatSet.iter
      (fun m ->
        BMat.iter
          (fun ij ->
            let m = BMat.remove ij m in
          ) deads.(i)
      ) old
  done
*)

(** From the maximal live matrices, produce a simploidal set structure. *)
let analyze_part3 liveMatrices brickno dimensions =
  let l = brickno in
  let n = dimensions in
  let take_boundaries simplex ips =
    BMat.fold
      (fun direction s -> Simploid.Simploidal.d (fst direction) (snd direction) s)
      ips
      simplex
  in
  let all_ones =
    Enum.fold
      (fun a j ->
        BMat.union a (BMat.of_enum l n (Enum.map (fun i -> (i, j)) (Enum.indexes brickno))))
      (BMat.create l n)
      (Enum.indexes dimensions)
  in
  let super =
    ref
      (Simploid.Simploidal.of_simplicial
   (snd (Simploid.Simplicial.standard (dimensions - 1))))
  in
  for i = 0 to brickno - 2 do
    super := Simploid.Simploidal.product
      !super
      (Simploid.Simploidal.of_simplicial
   (snd (Simploid.Simplicial.standard (dimensions - 1))))
  done;

  let top = Simploid.Simploidal.SS.choose (Simploid.Simploidal.maximal !super) in
  let complements = BMatSet.map (fun live -> BMat.diff all_ones live) liveMatrices in
  let local_max = Enum.map (take_boundaries top) (BMatSet.enum complements) in
  let below_local_max = Enum.map (fun x -> Simploid.Simploidal.S.get_below x) local_max in
  let live_sset = Enum.reduce Simploid.Simploidal.union below_local_max in
  live_sset

exception Ex of string


let euler_characteristic_version1 matrices =
  let positive = ref [] in
  let negative = ref [] in
  (* let cnt = ref 0 in *)
  BMatSet.iter
    (fun matrix ->
      (* Printf.printf "%d: +%d -%d\n%!" !cnt (List.length !positive) (List.length !negative); incr cnt; *)
      let add l =
        List.may_rev_map
          (fun n ->
            let i = BMat.inter matrix n in
            if empty_simploid i then None else Some i
          ) l
      in
      let new_positives = add !negative in
      let new_negatives = add !positive in
      positive := matrix :: !positive;
      positive := List.rev_append new_positives !positive;
      negative := List.rev_append new_negatives !negative
    )
    matrices;
  List.length !positive - List.length !negative

let euler_characteristic_version2 matrices =
  let l, n = BMat.dim (BMatSet.choose matrices) in
  let ln = l * n in
  let dim m = ln - BMat.cardinal m in
  let ans = ref 0 in
  BMatSet.iter
    (fun m ->
      if (dim m) mod 2 = 0 then
        incr ans
      else
        decr ans
    )
    matrices;
  !ans

(** From geometrical semantics, compute the trace space. The output is a
    covering of the trace space from which we can extract representatives for
    each connected component. *)
let analyze ?(components=true) ?(euler=true) ?(covering=false) semantics =
  (* let _ = Interpreter.init_fun semantics in *)
  let forbidden_area = Geometric_model.forbidden semantics in
  let cubes = Type.AC.to_array forbidden_area in
  let cube_list = Array.to_list cubes in
  let cubes = Array.map Type.C.to_array cubes in
  (*
  Array.sort
    (fun c1 c2 ->
      let m1 = Array.fold_left (fun m i -> m + Type.I.glb i) 0 c1 in
      let m2 = Array.fold_left (fun m i -> m + Type.I.glb i) 0 c2 in
      m1 - m2
    ) cubes;
  *)
  (* TODO: remove conversion *)
  let brickno = Array.length cubes in
  let l = brickno in
  let dimensions = Type.AC.dimension forbidden_area in
(*
  let n = dimensions in
*)
  let intersection_coordinates = boundary_coordinates dimensions cubes in

  (* Printf.printf "Boundary:\n%s\n%!" (BMat.to_string intersection_coordinates); *)

  let dead =
    print_stats
      "dead matrices"
      (fun () ->
        analyze1_part1_version4
          cubes
          intersection_coordinates
          dimensions)
  in
  Printf.printf "Dead matrices: %d\n%!" (BMatSet.cardinal dead);
  (* Printf.printf "%s\n%!" (string_of_bmats dead); *)

  (* TODO: ensure that we don't have empty rows? *)
  let alive =
    print_stats
      "live matrices"
      (fun () ->
        analyze1_part2_version1
          dead
          intersection_coordinates
          brickno
          dimensions)
  in
  Printf.printf "Maximal live matrices: %d\n%!" (BMatSet.cardinal alive);
  (* Printf.printf "%s\n%!" (string_of_bmats alive); *)

  let comps = ref [] in
  if components then
    (
      comps := print_stats "connectected components" (fun () -> bmat_components alive l);
      Printf.printf "Connected Components: %d\n%!" (List.length !comps);
      (* Printf.printf "%s\n%!" (string_of_bmats (BMatSet.of_list !comps)) *)
    );

  if euler then
    (
      let eul = print_stats "Euler characteristic" (fun () -> euler_characteristic_version2 alive) in
      Printf.printf "Euler Characteristic: %d\n%!" eul
    );

  let co = ref Co.empty in
  if covering then
    List.iter
      (fun ips ->
        co :=
          Co.add
    ((*Co.A*)AC.complement
       ((*Co.A*)AC.of_list
          (extend_cube_list_in_multiple_dimensions cube_list ips)))
    !co
      )
      !comps;
  (*
    let (sset, t) = duration (fun () -> analyze_part3 alive brickno dimensions) () in
    Printf.printf "Part 3 time: %F\n%!" t;
    let live_matrices =
    List.map (* unused variable  *)
    (fun x -> List.of_enum (IntPairSet.enum x))
    (List.of_enum (IntPairSetSet.enum alive))
    in
  *)
  !co
