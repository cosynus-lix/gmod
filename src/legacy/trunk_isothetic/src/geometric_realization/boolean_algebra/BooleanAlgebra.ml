(*Tensor product of boolean algebras in the category of commutative idempotent monoids*)

open BooleanAlgebraSig

module RawTensorProduct(BA:BooleanAlgebra) = struct

  type generator = BA.t

  type brick = Cp of generator array | Em of generator array

  module Cover = Set.Make(struct type t = brick let compare = Pervasives.compare end)

  type t = Cover.t

  module Brick = struct

  (*administrativia*)

  let dimension c = match c with
    | Cp arg
    | Em arg -> Array.length arg

  let test_dimension p1 p2 str =
    let d1 = dimension p1 in
    let d2 = dimension p2 in
    if d1 = d2
    then d1
    else failwith (Printf.sprintf "The arguments of %s are heterogeneous" str)

  let argument c = match c with
    | Cp arg
    | Em arg -> arg

  let is_empty p = match p with
    | Cp _ -> false
    | Em _ -> true

  let is_included p1 p2 =
    match p1,p2 with
      | Cp arg1,Cp arg2 ->
        let d = Array.length arg1 in
        let pos = ref 0 in (
        try
          while !pos < d do
            if not (BA.is_included arg1.(!pos) arg2.(!pos)) then raise Exit ;
            incr pos done ;
          true
        with Exit -> false)
      | Em _, _ -> true
      | _ -> false

  let is_included p a = Cover.exists (fun p2 -> is_included p p2) a

  let intersection p1 p2 =
      let d = test_dimension p1 p2 "(intersection)" in
      match p1,p2 with
        | Cp arg1 , Cp arg2  -> (
            try Cp (Array.init d (fun k ->
              let aux = BA.intersection arg1.(k) arg2.(k) in
              if not (BA.is_empty aux)
              then aux
              else raise Exit))
            with Exit -> Em arg1)
        | Em arg , _ -> Em arg
        | _ , Em arg -> Em arg

  let complement p =
    let d = dimension p in
    match p with
      | Cp [||] -> Cover.singleton (Em [||])
      | Cp arg -> (
          let answer = ref Cover.empty in
          let f k g =
            let aux = BA.complement g in
              if not (BA.is_empty aux)
              then answer :=
                Cover.add
                (Cp(Array.init d (fun x -> if x = k then aux else BA.full arg.(x))))
                !answer in
          Array.iteri f arg ;
          if Cover.is_empty !answer
          then Cover.singleton (Em arg)
          else !answer)
      | Em arg -> Cover.singleton (Cp (Array.map BA.full arg))

  let product p1 p2 = match p1 , p2 with
    | Cp arg1 , Cp arg2 -> Cp (Array.append arg1 arg2)
    | _ -> Em (Array.append (argument p1) (argument p2))

end(*Brick*)

  let compare = Cover.compare

  let of_array arg =
    let d = Array.length arg in
    let pos = ref 0 in
    Cover.singleton (
      try
        let () = while !pos < d do
          if BA.is_empty arg.(!pos)
          then raise Exit ;
          incr pos done in
        Cp arg
      with Exit -> Em arg)

  let string_of x = failwith "TPOBA.string_of"

  let is_empty a = try Brick.is_empty (Cover.choose a) with Not_found -> invalid_arg "is_empty [BooleanAlgebra]"

  let is_included a1 a2 = Cover.for_all (fun b1 -> Brick.is_included b1 a2) a1

  let full x =
    try Cover.singleton (Cp (Array.map BA.full (Brick.argument (Cover.choose x))))
    with Not_found -> invalid_arg "full [BooleanAlgebra]"

  let empty x =
    try Cover.singleton (Em (Brick.argument (Cover.choose x)))
    with Not_found -> invalid_arg "empty [BooleanAlgebra]"

  let intersection a1 a2 =
    let f b1 b2 accu2 =
      let brick = Brick.intersection b1 b2 in
        if not (Brick.is_empty brick)
        then Cover.add brick accu2
        else accu2 in
    let g b1 accu1 = Cover.fold (f b1) a2 accu1 in
    let result = Cover.fold g a1 Cover.empty in
    if Cover.is_empty result
    then (
      if Cover.is_empty a2
      then invalid_arg "intersection (second) [BooleanAlgebra]"
      else (try empty a1 with Invalid_argument _ -> invalid_arg "intersection (second) [BooleanAlgebra]") )
    else result

  let intersection_list ?default lx =
    let full = match lx with
      | x :: _ -> full x
      | [] -> (match default with Some x -> x | None -> invalid_arg "intersection_list") in
    List.fold_left intersection full lx

  let intersection_array ?default ax =
    let full = match ax with
      | [||] -> (match default with Some x -> x | None -> invalid_arg "intersection_array")
      | _ -> full ax.(0) in
    Array.fold_left intersection full ax

  let complement a =
    let b = Cover.choose a in
    let a = Cover.remove b a in
    let b = Brick.complement b in
    let f b accu = intersection (Brick.complement b) accu in
    Cover.fold f a b

  let normalize a = complement (complement a)

  let union a1 a2 =
    if not (is_empty a1)
    then
      if not (is_empty a2)
      then Cover.union a1 a2 (*case a1 ≠ Ø ⋀ a2 ≠ Ø*)
      else a1 (*case a2 = Ø*)
    else a2 (*case a1 = Ø*)

  let rec union_list ?default lx =
    let empty = match lx with
      | x :: _ -> empty x
      | [] -> (match default with
        | Some x -> x
        | None -> invalid_arg "intersection_list") in
    List.fold_left union empty lx

  let union_array ?default ax =
    let empty = match ax with
      | [||] -> (match default with Some x -> x | None -> invalid_arg "intersection_array")
      | _ -> empty ax.(0) in
    Array.fold_left union empty ax

  let one = Cover.singleton (Cp [||])

  let zero = Cover.singleton (Em [||])

  (*If both a1 and a2 are normal forms, then the resulting product is so*)

  (*[product a1 a2] is empty iff [a1] or [a2] is empty, which means that the only brick of the covering is the empty one. In
  this case the variable named empty is necessarily empty. Otherwise it is not used hence the fact it might actually not be
  empty, does not matter*)

  let product a1 a2 =
    let empty =
      try Cover.singleton (Brick.product (Cover.choose a1) (Cover.choose a2))
      with Not_found -> invalid_arg "product [BooleanAlgebra]" in
    let f p1 p2 accu2 =
      let new_brick = Brick.product p1 p2 in
      if not (Brick.is_empty new_brick)
      then Cover.add new_brick accu2
      else accu2 in
    let g p1 accu1 = Cover.fold (f p1) a2 accu1 in
    let answer = Cover.fold g a1 (Cover.empty) in
    if Cover.is_empty answer
    then empty
    else answer

  let rec product_list l = match l with
    | x :: l -> product x (product_list l)
    | [] -> one

  let product_array a = Array.fold_left (fun accu x -> product accu x) one a

  let rec product_fun f a b = if a > b then one else product (f a) (product_fun f (succ a) b)

  module AC = Algebra.Combinator (struct
    type t = Cover.t
    let zero = zero
    let one = one
    let full a = full a
    let product = product
    let product_array = product_array
    let product_list = product_list
    let product_fun = product_fun
    let union = union
    let normalize = normalize
    let string_of x = string_of x
  end)

  let generalized_product = AC.generalized_product

  let levelwise_product = AC.levelwise_product

  let locus_higher_than = AC.locus_higher_than

  let locus_lower_than = AC.locus_lower_than

  module FGSL = Algebra.Free.Graded.SemiLattice (struct
    type t = BA.t
    let compare = BA.compare
    let string_of = fun g -> BA.string_of g
  end)

  let factorize a =
    let degree = ref None in
    let f c accu =
      try FGSL.add (Brick.argument c) accu
      with Invalid_argument _ -> degree := Some (Brick.dimension c); accu in
    let a = Cover.fold f a FGSL.zero in
    FGSL.factorize ?degree:!degree ~unpacked:true a

end(*TensorProduct*)

module TensorProduct(BA:BooleanAlgebra):Graded with type generator = BA.t = RawTensorProduct(BA)

module RawTopologicalTensorProduct(TBA:Topological) = struct

  include RawTensorProduct(TBA)

  module TopologicalBrick = struct

    let closure p = match p with
			| Cp arg -> Cp (Array.map TBA.closure arg)
			| Em _ -> p

    (* Warning : the function interior requires its argument be a pre-normal form. *)

    let interior p =
      match p with
        | Cp arg  -> (
            try Cp (Array.map (fun g ->
              let aux = TBA.interior g in
              if not (TBA.is_empty aux)
              then aux
              else raise Exit) arg)
            with Exit -> Em arg)
        | Em _ -> p

  end (*TopologicalBrick*)

  let interior a =
    let f b accu = Cover.add (TopologicalBrick.interior b) accu in
    let aux = Cover.fold f a Cover.empty in
    if Cover.is_empty aux
    then empty a
    else aux

  let closure a =
    let f b accu = Cover.add (TopologicalBrick.closure b) accu in
    Cover.fold f a (empty a)

end(*TensorProductOfTopologicalBooleanAlgebras*)

module TopologicalTensorProduct(TBA:Topological):GradedTopological with type generator = TBA.t = RawTopologicalTensorProduct(TBA)

module RawDirectedTensorProduct(DBA:Directed) = struct

include TopologicalTensorProduct(DBA)

  module DirectedBrick = struct
  end(*DirectedBrick*)

  let future x y = failwith "future NIY"

  let past x y = failwith "past NIY"

end(*TensorProductOfDirectedBooleanAlgebras*)

module DirectedTensorProduct(DBA:Directed):GradedDirected with type generator = DBA.t = RawDirectedTensorProduct(DBA)

