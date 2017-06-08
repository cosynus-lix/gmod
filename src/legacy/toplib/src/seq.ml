type 'a t = {dim : Nat.t; seq : (Nat.t,'a) PMap.t}
let append i e s =
  {dim=(Nat.max (Nat.succ i) s.dim); seq=PMap.add i e s.seq}
let null = {dim=Nat.zero; seq=PMap.empty}
let access s i = try PMap.find i s.seq with Not_found -> raise Not_found
let size s = s.dim
let change i e s =
  if Nat.sgn(i)<Nat.sgn(s.dim) then append i e s
    else raise Not_found
let expand e s = append s.dim e s
let rec constant n e = 
  if (Nat.compare n Nat.zero)=0 then null 
    else expand e (constant (Nat.pred n) e)
let contract s =
  if (Nat.compare s.dim Nat.zero)=0 then raise Not_found
    else {dim=(Nat.pred s.dim); seq=(PMap.remove (Nat.pred s.dim) s.seq)}
let map f s = {dim=s.dim; seq=PMap.map f s.seq}
let prod s t = 
  let dim = Nat.max s.dim t.dim
  and get u i =
    try Some(access u i) with Not_found -> None
  in
    let rec seq n v =
      if (Nat.compare n dim)=0 then v
        else seq (Nat.succ n) (PMap.add n ((get s n), (get t n)) v)
    in 
      {dim=dim; seq=(seq Nat.zero PMap.empty)}
let fold f s a =
  let rec aux i b =
    if (Nat.compare i s.dim)=0 then b
      else f (access s i) (aux (Nat.succ i) b)
  in
    aux Nat.zero a
let list_from_seq s = let aux e l = e::l in fold aux s []
let seq_from_list l =
  let aux s e = expand e s in List.fold_left aux null l
let concat s t =
  seq_from_list (List.concat [(list_from_seq s);(list_from_seq t)])  
let tex s = "("^(String.concat "," (list_from_seq s))^")"
