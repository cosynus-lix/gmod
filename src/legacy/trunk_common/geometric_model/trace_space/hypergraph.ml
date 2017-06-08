open Common

module Make
  (V:sig
    include Set.OrderedType
    val to_string : t -> string
  end) =
struct
  type vertex = V.t

  module E = Set.Make(V)

  type edge = E.t

  module ES = Set.Make(E)

  type t =  ES.t

  let empty = ES.empty

  let add_edge e g =
    ES.add e g

  let union g1 g2 =
    ES.union g1 g2

  let join g1 g2 =
    let ans = ref ES.empty in
    ES.iter
      (fun x ->
        ES.iter
          (fun y ->
            ans := ES.add (E.union x y) !ans
          ) g2
      ) g1;
    !ans

  let minimal g =
    ES.filter
      (fun e ->
        ES.for_all
          (fun x -> x = e || not (E.subset x e))
          g
      )
      g

  (*
  let maximal g =
    ES.filter
      (fun e ->
        ES.for_all
	 (fun x ->
           if x = e then
             true
	   else
             not (E.subset e x)
         )
        g
      )
      g
  *)

  let is_transversal g t =
    ES.for_all
      (fun e -> not (E.is_empty (E.inter e t)))
      g

  (** Hypergraph containing all the elements of an edge as edges. *)
  let singletons e =
    E.fold (fun v g -> ES.add (E.singleton v) g) e empty

  (* The first hypergraph transversal algorithm given by Kavvadias and
     Stavropoulos. *)
  let transversal_version1 g =
    ES.fold (fun e g -> minimal (join g (singletons e))) g (ES.singleton E.empty)

    (*
    ES.fold
      (fun e a ->
        minimal (join a (ES.of_enum (Enum.map E.singleton (E.enum e))))
      )
      g
      (ES.singleton E.empty)
    *)

  let transversal = transversal_version1

  let to_string g =
    let line l =
      let e = List.map V.to_string (E.elements l) in
      "[" ^ String.concat "; " e  ^ "]"
    in
    let e = List.map line (ES.elements g) in
    "[" ^ String.concat "; " e ^ "]"
end
