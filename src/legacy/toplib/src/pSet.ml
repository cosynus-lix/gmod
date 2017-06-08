type 'a t=('a,unit) PMap.t
exception Halt
let empty = PMap.empty
let is_empty = PMap.is_empty
let fold f x a = let aux e _ b = f e b in PMap.foldi aux x a
let card x = let aux _ c = Nat.succ c in fold aux x Nat.zero
let mem = PMap.mem
let subset x y = let aux e a = a or (mem e y) in fold aux x false
let compare x y =
  let (m,n) = (card x,card y) in
    if m<>n then (Nat.compare m n) else
      if ((subset x y) && m<n) then 0
        else (compare x y)
let add e x = PMap.add e () x
let singleton e = add e empty
let remove e x = PMap.remove e x
let union x y = let aux e _ z = add e z in PMap.foldi aux y x
let minus x y =
  let aux e _ z = if (mem e z) then (remove e z) else z in
    PMap.foldi aux y x
let map f x =
  let aux e _ z = add (f e) z in PMap.foldi aux x empty
let iter f x =
  let g a _ = f a in PMap.iter g x
let choice x =
  let storage = ref None in
    let aux a = let _ = storage := Some(a) in raise Not_found in
      let _ = try (iter aux x) with Not_found -> () in
        match !storage with
          None -> raise Not_found
        | Some(e) -> e
let prod x y =
  let laux e z =
    let raux f w = add (e,f) w in
      fold raux y z
  in
    fold laux x empty
let tex x =
  let aux e _ (s,b) = if b=false then (s^e,true) else (s^","^e,true) in
    match (PMap.foldi aux x ("{",false)) with (c,_) -> c^"}" 
