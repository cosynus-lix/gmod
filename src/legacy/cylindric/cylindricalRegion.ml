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
  
  (** {2 String conversion} *)
  
  val string_of: t -> string list
  
  (** {2 Tests} *)
  
  val is_empty: t -> bool
  (** [is_empty x] returns [true] iff [x] represents the empty region.*)
  
  val mem: point -> t -> bool
  (** [mem p x] returns [true] iff the point [p] belongs to the region 
  represented by [x]. *)
  
  val is_included: t -> t -> bool
  (** It is guaranteed that if [is_included x y] returns [true], then 
  the region represented by [x] is indeed included in the one represented by [y]. 
  If [y] is in prenormal form, the converse is also true.*) 
  
  (** {2 Constructors} *)
  
  val generator: generator -> t
  (** Returns a one-dimensional region. Normal form.*)
  
  val block: generator array -> t
  (** [block arg] is the product of the generators of the argument. 
  Normal form.*)
  
  (** {2 Boolean operators} *)
  (** The binary operators must be called with arguments of the same dimension. 
  The behavior is unspecified otherwise.*)
  
  val empty: int -> t
  (** [empty n] is the bottom element of the Boolean algebra of regions of 
  dimension [n]. Normal form.*)
  
  val full: int -> t
  (** [full n] is the top element of the Boolean algebra of regions of 
  dimension [n]. Normal form. *)
  
  val meet: t -> t -> t
  (** [meet x y] is the greatest lower bound of [x] and [y], which should 
  have the same dimension. Preserves prenormal form.*)
  
  val complement: t -> t
  (** [complement x] is the complement of [x] in the Boolean algebra of 
  regions of dimension [dim x]. Prenormal form. Time consuming.*)
  
  val difference: t -> t -> t
  (** [difference x y = meet x (complement y)] in prenormal form if [x] 
  is in prenormal form.*)
  
  val join: t -> t -> t
  (** [join x y] is the least upper bound of [x] and [y], which should 
  have the same dimension. Does nor preserve prenormal form.*)
  
  (** {2 Monoidal structure} *)

  val product: t -> t -> t
  (** Cartesian product of regions. Preserves normal form.*)

  val zero: t
  (** The zero element of the monoid of cylindrical region. It is absorbing in 
  the sense that [product zero  x = product x zero = empty (dimension x)]. 
  Normal form.*)
  
  val one: t
  (** The one element of the monoid of cylindrical region. It is neutral in the 
  sense that [product one x = x = product x one]. Normal form.*)

  
  (** {2 Topological operators} *)
  
  val interior: (bool array) -> t -> t
  (** [interior shape x] is the topological interior of [x] computed relatively 
  to the [shape], viz a product of circles (represented by [false]) and 
  intervals (represented by [true]). The argument should be in prenormal form. 
  In that case, the output is also in prenormal form.*)

  val closure: (bool array) -> t -> t
  (** [closure shape x] is the topological closure of [x] computed relatively 
  to the [shape], viz a product of circles (represented by [false]) and 
  intervals (represented by [true]). It does not preserve prenormal forms.*)
  
  (** {2 Directed operators} *)
  
  (** {2 Miscellaneous} *)
  
  val normalize: t -> t
  (** Returns the normal form of the argument. Time consuming. *)
  
end (* S *)

module Make(DD:DashDot.S):
  (S with type value = DD.value and type generator = DD.t) = struct
  
  type generator = DD.t
  
  type value = DD.value
    
  type point = value array
  
  module Block = struct

    type t = Empty of int | NonEmpty of (generator array)

    let string_of b = 
        match b with
        | NonEmpty arg -> 
            String.concat "*" (Array.to_list(Array.map DD.string_of arg))
        | Empty d -> "Empty " ^ (string_of_int d)

    let of_generator_array ag = 
      if Array.exists (DD.is_empty) ag
      then Empty (Array.length ag)
      else NonEmpty ag

    let is_empty b = match b with
      | NonEmpty _ -> false
      | Empty _ -> true

    let mem p b = match b with
      | NonEmpty arg -> (
          try 
            Array.iter2 
              (fun v g -> if DD.mem v g then () else raise Exit)
              p arg;
              true
          with Exit -> false)
      | Empty _ -> false

    let dimension b = match b with
      | NonEmpty arg -> Array.length arg
      | Empty n -> n

    let is_included b1 b2 = match b1,b2 with
      | NonEmpty arg1,NonEmpty arg2 -> (
          try 
            Array.iter2 
              (fun g1 g2 -> if DD.is_included g1 g2 then () else raise Exit)
              arg1 arg2;
              true
          with Exit -> false)
      | Empty _, _ -> true
      | _ -> false

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

    let closure shape g = 
      if shape 
      then DD.HalfLine.closure g
      else DD.Circle.closure g

    let closure shape b = match b with
      | NonEmpty arg -> NonEmpty(Array.map2 closure shape arg)
      | _ -> b
      
    let interior shape g = 
      let tmp = 
        if shape 
        then DD.HalfLine.interior g
        else DD.Circle.interior g in
      if DD.is_empty tmp then raise Exit else tmp
      
    let interior shape b = 
      try (
        match b with
          | NonEmpty arg -> NonEmpty(Array.map2 interior shape arg)
          | _ -> b)
      with Exit -> Empty (dimension b)

  end (* Block *)

  module Cover = Set.Make(Block)

  type t = Cover.t

  let string_of x = List.map (Block.string_of) (Cover.elements x)

  let dimension a = 
    try Block.dimension (Cover.choose a)
    with Not_found -> invalid_arg "The empty cover does not represent any region."

  let of_block b = Cover.singleton b

  let block ag = of_block (Block.of_generator_array ag)

  let generator g = of_block (Block.of_generator g)

  let zero = of_block (Block.Empty 0)
  
  let one = of_block (Block.NonEmpty [||])

  let empty n = of_block (Block.Empty n)
  
  let full n = of_block (Block.NonEmpty (Array.make n DD.full))

  let remove_useless_empty_block x = 
    let y = Cover.remove (Block.Empty (dimension x)) x in
    if Cover.is_empty y then x else y 

  let is_empty x = 
    try 
      Cover.iter 
        (fun b -> if Block.is_empty b then () else raise Exit) x; 
      true
    with Exit -> false
  
  let mem p x = Cover.exists (Block.mem p) x

  let is_included x y = 
    Cover.for_all (
      fun b -> Cover.exists (Block.is_included b) y) 
      x

  (*One assumes that neither cr1 nor cr2 are illegal values i.e. they are not 
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
          let k = ref (-1) in
          let f accu g =
            let () = incr k in
            let tmp = DD.complement g in
              if DD.is_empty tmp
              then accu
              else
                Cover.add
                  (Block.NonEmpty(Array.init d 
                    (fun x -> if x = !k then tmp else DD.full)))
                  accu in
          let output = Array.fold_left f (Cover.empty) arg in
          if Cover.is_empty output
          then empty d
          else output)
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

  let complement cr =
    let b = Cover.choose cr in
    let a = Cover.remove b cr in
    let b = complement b in
    let f b accu = meet (complement b) accu in
    Cover.fold f a b

  let difference x y = meet x (complement y)

  let normalize cr = complement (complement cr)

(* The join could be defined through De Morgan's law, but it would be very 
inefficient. *)

  let join cr1 cr2 = Cover.union cr1 cr2

  let closure shape cr = Cover.map (Block.closure shape) cr
  
  let interior shape cr = Cover.map (Block.interior shape) cr

end (* Make *)
