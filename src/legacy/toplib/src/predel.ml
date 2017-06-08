type t =
    {
      dom : Nat.t;
      op : Nat.t list
    }

exception Dimension_mismatch of (int * int)

let compare x y =
  let ddom = Nat.compare x.dom y.dom in
    if ddom<>0 then ddom else
      let rec dop t u =
        match (t,u) with
          (i::v,j::w) -> 
            if i=j then (dop v w) else Nat.compare i j
        | ([],[]) -> 0
        | ([],_) -> -1
        | (_,[]) -> 1      
      in
        dop x.op y.op

let rec normal t =
  let rec pass u =
    match u with
      i::(j::v) -> 
      if (Nat.compare i j)<0 then (Nat.pred j)::i::pass v
        else u
    | _ -> u
  in
    let w = pass t in if t=w then t else (normal w)

let dom x = x.dom

let rec codom x = (Nat.mag ((Nat.sgn x.dom)-(List.length x.op)))

let face i x =
  if (Nat.compare i (codom x)) > 0 then
    raise (Dimension_mismatch ((Nat.sgn i),(Nat.sgn (codom x))))
  else 
    {dom=x.dom; op=(normal (i::x.op))}
let id n = {dom=n; op=[]}
let compose x y =
  if (codom y)=x.dom then 
    {dom=y.dom; op=(normal (List.concat [x.op;y.op]))}
  else 
    raise (Dimension_mismatch ((Nat.sgn (codom y)),(Nat.sgn x.dom)))
let malcev x y z =
  if (codom y)<>(codom z) then None  
    else 
      let rec div r s t =
        if r=s then (Some t)
          else match r with
            [] -> None
          | si::u -> 
            match (div u s t) with
              None -> None
            | Some(v) -> Some(si::v)
      in
        match (div x.op y.op z.op) with 
          None -> None
        | Some(w) -> Some {dom=z.dom; op=w}
let hom m n f a =
  if (Nat.compare m n) < 0 then a
    else if m=n then (f (id m) a)
      else
        let rec succ x up =
          match x with
            None -> None
          | (Some []) -> None
          | Some(i::t) -> 
              if i=up then
                match (succ (Some t) i) with
                  None -> None
                | Some(j::u) -> Some(j::(j::u))
                | _ -> None
              else
                Some((Nat.succ i)::t)
         in
           let rec iterate y b =
             match y with
               None -> b
             | Some(v) -> 
               iterate (succ y (Nat.succ n)) (f {dom=m; op=v} b) 
           in
             iterate (Some(Seq.list_from_seq (Seq.constant 
                     (Nat.mag (Nat.compare m n)) Nat.zero))) a
let rec tex x s =
  match x.op with
    [] -> s^"_{"^(Nat.tex x.dom)^"}"
  | i::t -> "d_{"^(Nat.tex i)^"}"^(tex {dom=x.dom; op=t} s)
