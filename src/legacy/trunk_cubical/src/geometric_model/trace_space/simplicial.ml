open Common

(** Presimplicial sets. *)
module Pre = struct
  module Set = struct
    (** A simplex in a given dimension. *)
    type simplex_id = int

    (** A simplex name together with its dimension. These should be used to
        identify uniquely the simplices. *)
    type simplex = int * simplex_id

    (** A simplicial set. a.(d) is the set of simplices in dimension d,
        a.(d).(n) is the set (array) of faces of n-th d-dimensional array. *)
    type t = simplex_id array array array

    (** Dimension of the simplex (the maximal dimension of a possibly non empty
        set of simplexes plus one). *)
    let dimension s =
      Array.length s

    let simplices_faces s d =
      if d < 0 then
        [|[|0|]|]
      else if d < Array.length s then
        s.(d)
      else
        [||]

    let face ss (d,s) i = d-1, ss.(d).(s).(i)

    let simplices s d =
      Array.length (simplices_faces s d)

    let empty = [||]

    let standard d =
      (* Enumerate ordered sets with k elements amongst d. *)
      let enum_binomial n k =
        let rec aux len start =
          if len = 0 then
            [[]]
          else
            let ans =
              List.init
                (n-start)
                (fun n ->
                  let n = n+start in
                  List.map (fun l -> n::l) (aux (len-1) (n+1))
                )
            in
            List.concat ans
        in
        aux k 0
      in
      (* The simplices given as lists of integers. *)
      let simplices =
        Array.init
          d
          (fun n -> Array.of_list (enum_binomial d (n+1)))
      in
      (* Index of an n dimensional simplex given as a list of integers. *)
      let index n l =
        if n-1 < 0 then 0 else Array.findi (fun m -> m = l) simplices.(n-1)
      in
      (* Faces of a simplex given as a list of integers. *)
      let faces l =
        List.map_ctxt (fun b _ t -> b@t) l
      in
      let faces n l =
        Array.of_list (List.map (index n) (faces l))
      in
      Array.init d (fun n -> Array.map (faces n) simplices.(n))

    (** Disjoint union of two sets. *)
    let union s1 s2 =
      let shift n a = Array.map (fun x -> x + n) a in
      let l = max (Array.length s1) (Array.length s2)  in
      let ans = Array.make l [||] in
      for d = 0 to l - 1 do
        ans.(d) <-
          Array.append
          (simplices_faces s1 d)
          (Array.map
             (shift (if d = 0 then 0 else (simplices s1 (d-1))))
             (simplices_faces s2 d))
      done;
      ans

    let to_string s =
      let ans = ref "" in
      let faces l =
        let l = Array.to_list l in
        let l = List.map string_of_int l in
        "(" ^ String.concat ", " l ^ ")"
      in
      for n = 0 to Array.length s - 1 do
        ans := !ans ^ Printf.sprintf " * dim %d: %d simplices\n" n (Array.length s.(n));
        Array.iter (fun l -> ans := !ans ^ Printf.sprintf "    - %s\n" (faces l)) s.(n)
      done;
      !ans

    (** Number of connected components. *)
    let components s =
      let color = Array.init (simplices s 0) (fun i -> i) in
      let same x y =
        for i = 0 to Array.length color - 1 do
          if color.(i) = max x y then color.(i) <- min x y
        done
      in
      Array.iter (fun x -> same x.(0) x.(1)) s.(1);
      let colors = ref [] in
      Array.iter (fun c -> if not (List.mem c !colors) then colors := c :: !colors) color;
      List.length !colors
  end

  module Matrix = struct
    module MI = Matrix.Int

    (** Matrices of faces operators. *)
    type t = (int Matrix.t) array

    let dimension s = Array.length s

    let of_set s =
      let ans =
        Array.init
          (Set.dimension s)
          (fun n -> Matrix.make (Set.simplices s n) (Set.simplices s (n-1)) 0)
      in
      for n = 0 to Array.length s - 1 do
        for k = 0 to Array.length s.(n) - 1 do
          let sn = s.(n) in
          for i = 0 to Array.length sn.(k) - 1 do
            if i mod 2 = 0 then
              MI.incr ans.(n) k sn.(k).(i)
            else
              MI.decr ans.(n) k sn.(k).(i)
          done
        done
      done;
      ans

    let check s =
      for i = 1 to dimension s - 1 do
        assert (MI.is_zero (MI.mult s.(i) s.(i-1)))
      done
  end
end

(*
let () =
  let s = Pre.Set.standard 3 in
  Printf.printf "%s\ncomponents: %d\n%!" (Pre.Set.to_string s) (Pre.Set.components s);
  let s = Pre.Set.union s s in
  Printf.printf "%s\ncomponents: %d\n%!" (Pre.Set.to_string s) (Pre.Set.components s);
  let m = Pre.Matrix.of_set s in
  Pre.Matrix.check m
*)

(** Prodsimplicial or simploidal sets. *)
module Prod = struct
  module Set = struct
    type simploid = Pre.Set.simplex array

    (** A prodsimplicial set consists of a simplicial set together with a list
        of simploids in the set. *)
    type t = Pre.Set.t * simploid array

    let of_simplicial s =
      s,
      Array.concat
        (Array.to_list
           (Array.mapi
              (fun d sd ->
                Array.init
                  (Array.length sd)
                  (fun i -> d,i))
              s
           )
        )

    (* TODO: check that the simploid is actually in the pss? *)
    let face (s,_) sid i j =
      let d,_ = sid.(i) in
      if d = 0 then
        Array.remove sid i
      else
        Array.init
          (Array.length sid)
          (fun k ->
            if k = i then
              Pre.Set.face s sid.(i) j
            else
              sid.(k))

    let to_string (s,p) =
      let simploids =
        let e = Array.enum p in
        let e = Enum.map (fun (d,s) -> Printf.sprintf "(%d,%d)" d s) e in
        String.concat_enum " " e
      in
      Printf.sprintf "Underlying simplicial set:\n%s\nSimploids:\n%s\n%!" (Pre.Set.to_string s) simploids
  end
end

(*
  let () =
  let s = Pre.Set.standard 3 in
  let s = Prod.Set.of_simplicial s in
  Printf.printf "%s\n%!" (Prod.Set.to_string s)
*)
