module type S = sig

  (** {2 Types} *)

  type generator
  (** Interval of the half-line or arc of the circle *) 

  type value
  (** Range of values for the bounds of the generators *)

  type point
  (** Points of the space *)

  type t
  (** Cylindrical region, the implemented type *)
  
  (** {2 Exceptions} *)
  
  (** {2 String conversion} *)
  
  val string_of: t -> string
  
  (** {2 Tests} *)
  
  (** {2 Constants} *)
  
  val zero: t
  (** The zero element of the monoid of cylindrical region. It is absorbing in 
  the sense that [zero * x = x * zero] is the empty region of dimension 
  [dim  x].*)
  
  val one: t
  (** The one element of the monoid of cylindrical region. It is neutral in the 
  sense that [one * x = x = x * one].*)
  
  (** {2 Constructors} *)
  
  val of_generator: generator -> t
  (** Returns a one-dimensional region. *)
  
  val product: t -> t -> t
  (** Cartesian product of regions. *)
  
  (** {2 Boolean operators} *)
  (** Parametrized by the dimension. *)
  
  val empty: int -> t
  (** [empty n] is the bottom element of the Boolean algebra of regions of 
  dimension [n]. *)
  
  val full: int -> t
  (** [full n] is the top element of the Boolean algebra of regions of 
  dimension [n]. *)
  
  val meet: t -> t -> t
  (** [meet x y] is the greatest lower bound of [x] and [y], which should 
  have the same dimension. *)
  
  val complement: t -> t
  (** [complement x] is the complement of [x] in the Boolean algebra of 
  regions of dimension [dim x]. *)
  
  (** {2 Directed topology} *)
  
  (** {2 Miscellaneous} *)
  
end (* S *)

module Make(DD:DashDot.S):
  (S with type value = DD.value and type generator = DD.t) = struct
  
  type generator = DD.t
  
  type value = DD.value
    
  type point = value array
  
  module Block = struct

    type t = Empty of int | NonEmpty of (generator array)

    let is_empty b = match b with
      | NonEmpty _ -> false
      | Empty _ -> true

    let dimension b = match b with
      | NonEmpty arg -> Array.length arg
      | Empty n -> n

    let of_generator g = 
      if DD.is_empty g
      then Empty 1
      else NonEmpty [|g|]
  
    let compare b1 b2 = match b1,b2 with
      | NonEmpty arg1,NonEmpty arg2 -> (
          let delta = ref 0 in
          try
            let f k g1 =
              let d = DD.compare g1 arg2.(k) in
              if d <> 0 then (delta:=d ; raise Exit) in
            Array.iteri f arg1 ; 0
          with Exit -> !delta)
      | NonEmpty _,Empty _ -> 1
      | Empty _,NonEmpty _ -> -1
      | _ -> 0
  
    let product b1 b2 = match b1,b2 with
      | NonEmpty arg1,NonEmpty arg2 -> NonEmpty (Array.append arg1 arg2)
      | _ -> Empty (dimension b1 + dimension b2)
  
    let meet b1 b2 =
      let d = dimension b1 in
      let () = assert (d = dimension b2) in
      match b1,b2 with
        | NonEmpty arg1,NonEmpty arg2  -> (
            try
              NonEmpty (
                Array.init d (
                  fun k ->
                    let aux = DD.meet arg1.(k) arg2.(k) in
                    if DD.is_empty aux
                    then raise Exit
                    else aux))
            with Exit -> Empty d)
        | _ -> Empty d

  end (* Block *)

  module Cover = Set.Make(Block)

  type t = Cover.t

  let string_of x = failwith "CylindricalRegion.string_of NIY" (*TODO*)

  let dimension a = 
    try Block.dimension (Cover.choose a)
    with Not_found -> failwith "The empty cover does not represent any region."

  let of_block b = Cover.singleton b

  let of_generator g = of_block (Block.of_generator g)

  let zero = of_block (Block.Empty 0)
  
  let one = of_block (Block.NonEmpty [||])

  let empty n = of_block (Block.Empty n)
  
  let full n = of_block (Block.NonEmpty (Array.make n DD.full))

  (*One assumes that neither cr1 nor cr2 are legal values i.e. they are not 
  Cover.empty.*)

  let product cr1 cr2 =
    let f b1 b2 a = Cover.add (Block.product b1 b2) a in
    let g b1 a = Cover.fold (f b1) cr2 a in
    Cover.fold g cr1 (Cover.empty)

  (* First we compute the complement of a block... *)

  let complement p =
    let d = Block.dimension p in
    match p with
      | Block.NonEmpty arg -> (
          let answer = ref Cover.empty in
          let f k g =
            let aux = DD.complement g in
              if not (DD.is_empty aux)
              then answer :=
                Cover.add
                (Block.NonEmpty(Array.init d (fun x -> if x=k then aux else DD.full)))
                !answer in
          Array.iteri f arg ;
          if Cover.is_empty !answer
          then empty d
          else !answer)
      | Block.Empty _ -> full d

  (* ...then with the help of meet and of the De Morgan's laws... *)

  let meet cr1 cr2 =
    let f b1 b2 a =
      let b = Block.meet b1 b2 in
        if Block.is_empty b
        then a
        else Cover.add b a in
    let g b1 a = Cover.fold (f b1) cr2 a in
    let result = Cover.fold g cr1 Cover.empty in
    if Cover.is_empty result
    then empty (dimension cr1)
    else result

(* ...we generalize to the complement of a region. *)

  let complement a =
    let b = Cover.choose a in
    let a = Cover.remove b a in
    let b = complement b in
    let f b accu = meet (complement b) accu in
    Cover.fold f a b

  let normalize a = complement (complement a)




end (* Make *)
