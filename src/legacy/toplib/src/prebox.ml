type t = {dom : Nat.t; op : (bool * Nat.t) list}
exception Dimension_mismatch of (int * int)
let compare x y =
  if Nat.sgn(x.dom) < Nat.sgn(y.dom) then -1
    else if Nat.sgn(x.dom) > Nat.sgn(y.dom) then 1
      else 
        let rec dop t u =
          match (t,u) with
            ((si,i)::v,(sj,j)::w) -> 
              if i<>j then (Nat.compare i j) else
                (match (si,sj) with
                  (true,true) -> dop v w
                | (false,false) -> dop v w
                | (false,true) -> -1
                | (true,false) -> 1)
          | ([],[]) -> 0
          | ([],_) -> -1
          | (_,[]) -> 1       
        in
          dop x.op y.op
let rec normal t =
  let rec pass u =
    match u with
    (si,i)::((sj,j)::v) -> 
      if (Nat.compare i j)<0 then (sj,(Nat.pred j))::(si,i)::pass v
        else u
    | _ -> u
  in
    let w = pass t in if t=w then t else (normal w)
let dom x = x.dom
let rec codom x = Nat.mag ((Nat.sgn x.dom)-(List.length x.op))
let face si i x =
  if (Nat.compare i (codom x)) > 0 then
    raise (Dimension_mismatch ((Nat.sgn i),(Nat.sgn (codom x))))
  else 
    if i=Nat.zero then 
      raise (Dimension_mismatch (0,1))
    else 
      {dom=x.dom; op=(normal ((si,i)::x.op))}
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
          | sii::u -> 
            match (div u s t) with
              None -> None
            | Some(v) -> Some(sii::v)
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
          | Some((false,i)::t) -> Some((true,i)::t)
          | Some((true,i)::t) -> 
              if i=up then
                match (succ (Some t) i) with
                  None -> None
                | Some((sj,j)::u) -> Some((false,j)::((sj,j)::u))
                | _ -> None
              else
                Some((false,Nat.succ i)::t)
         in
           let rec iter y b =
             match y with
               None -> b
             | Some(v) -> iter (succ y (Nat.succ n)) (f {dom=m; op=v} b) 
           in
             iter
             (Some(Seq.list_from_seq (Seq.constant (Nat.mag (Nat.compare m n)) 
             (false,Nat.one)))) a
let rec tex x s =
  match x.op with
    [] -> s^"_{"^(Nat.tex x.dom)^"}"
  | (true,i)::t -> "d^+_{"^(Nat.tex i)^"}"^(tex {dom=x.dom; op=t} s)
  | (false,i)::t -> "d^-_{"^(Nat.tex i)^"}"^(tex {dom=x.dom; op=t} s)
