type 'a t = ('a, 'a PSet.t) PMap.t
let null = PMap.empty
let mem = PMap.mem
let succ = PMap.find
let pred e x = 
  let aux a b res =
    if (PSet.mem e b) then PSet.add a res else res
  in
    PMap.foldi aux x PSet.empty
let rec up set x =
  if PSet.is_empty set then PSet.empty else
    let aux e res = PSet.union (PSet.add e res) (up (succ e x) x) 
    in
      PSet.fold aux set PSet.empty 
let leq a b x = PSet.mem b (up (PSet.singleton a) x) 
let add f a x =
  if mem a x then x else
    let y = PMap.add a PSet.empty x
    and sort b c z = 
      if (leq b c z) then z
        else PMap.add b (PSet.add c (PMap.find b z)) z
    in
      let aux d _ w =
        let diff = f a d in
          if diff=0 then w
            else if diff<0 then sort a d w
              else sort d a w
      in
        PMap.foldi aux x y    
let min x =
  let aux a _ res = 
    if PSet.is_empty (pred a x) then PSet.add a res else res
  in
    PMap.foldi aux x PSet.empty
let max x =
  let aux a b res =
    if PSet.is_empty b then PSet.add a res else res
  in
    PMap.foldi aux x PSet.empty
let forget x =
  let aux e _ y = PSet.add e y in PMap.foldi aux x PSet.empty
let chains x =
  let rec chains_from set =
    if PSet.is_empty set then PSet.singleton []
      else
        let laux e bundle = 
          let raux l res = PSet.add (e::l) res in
            PSet.fold raux (chains_from (succ e x)) bundle
        in
          PSet.fold laux set PSet.empty 
  in
    chains_from (min x) 
let discrete x =
  let aux e res = PMap.add e PSet.empty res in
    PSet.fold aux x PMap.empty
let order f x =
  let aux e res = add f e res in
    PSet.fold aux x null
let tex x =
  let laux e set s =
    let raux f t = PSet.add (e^"<"^f) t in 
      PSet.fold raux set (PSet.add (e^"<="^e) s)
  in
    PSet.tex (PMap.foldi laux x PSet.empty)
let map f x =
  let aux e set res = 
    PMap.add (f e) (PSet.map f set) res
  in
    PMap.foldi aux x PMap.empty
let rec ord n =
  if (Nat.compare n Nat.zero)=0 then (add Nat.compare n null)
    else add Nat.compare n (ord (Nat.pred n))  
