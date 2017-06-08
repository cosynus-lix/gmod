type t = {dim : Nat.t option; boxes : (int list * int list) PSet.t}
type rel = Equals | Contains | Extends | Unrelated 
exception Dimension_mismatch of (int * int)
exception No_case
let singleton x = 
  {dim=Some((Nat.mag (List.length x))); boxes=PSet.singleton (x,x)}
let tex p =
  let minitex (x,y) = 
    let tinitex z = 
      Seq.tex (Seq.map string_of_int (Seq.seq_from_list z))
    in
      "["^(tinitex x)^","^(tinitex y)^"]" 
  in
    let attach h t = h::t in
      String.concat "+" (PSet.fold attach (PSet.map minitex p.boxes) []) 
let dim p = p.dim
let null = {dim=None; boxes=PSet.empty}
(**************************** auxilliary function **************************) 
(* leq a b returns true iff a is less than or equal to b *)
let rec leq a b =
  match (a,b) with
    ([],[]) -> true
  | (ic::c,id::d) -> (ic<=id) && (leq c d)
  | _ -> raise (Dimension_mismatch ((List.length a),(List.length b)))
let interval x y =
  if not (leq x y) then null
    else let (m,n)=(List.length x,List.length y) in if m<>n then null 
      else {dim=Some(Nat.mag m); boxes=PSet.singleton (x,y)}
(**************************** auxilliary functions *************************) 
(* checkdim x y raises Dimension_mismatch if x and y do not share same dim *)
let checkdim x y =
  match (x.dim,y.dim) with
    (None,Some(d)) -> raise (Dimension_mismatch (-1, Nat.sgn d))
  | (Some(d),None) -> raise (Dimension_mismatch (Nat.sgn d, -1))
  | (Some(m),Some(n)) ->
    if (Nat.compare m n)=0 then () 
      else raise (Dimension_mismatch (Nat.sgn m, Nat.sgn n))
  | (None,None) -> ()
(* deg a b returns dimension of the interior of (a,b) *)
let rec deg a b =
  match (a,b) with
    (ic::c,id::d) -> if ic=id then deg c d else 1+(deg c c)
  | ([],[]) -> 0
  | _ -> raise (Dimension_mismatch (List.length a,List.length b))
let less a b = (leq a b) && (a<>b)
(* sup x y returns the supremum of x and y *)
let rec sup a b = 
  match (a,b) with
    (ic::c,id::d) -> (max ic id)::(sup c d)
  | ([],[]) -> []
  | _ -> raise (Dimension_mismatch ((List.length a), (List.length b)))
(* inf a b returns the infemum of a and b *)
let rec inf a b = 
  match (a,b) with
    (ic::c,id::d) -> (min ic id)::(inf c d)
  | ([],[]) -> []
  | _ -> raise (Dimension_mismatch ((List.length a), (List.length b)))
let compare_segments (i,j) (k,l) =
  if i>k or j<l then Unrelated else
    if i<>k or l<>j then Contains else Equals
let rec compare_cubes (a,b) (c,d) =
  match (a,b,c,d) with
    ([],[],[],[]) -> (a,b,c,d,Equals)
  | (ia::a,ib::b,ic::c,id::d) ->  
    (match ((compare_cubes (a,b) (c,d)),(compare_segments (ia,ib) (ic,id))) with
      ((e,f,g,h,Equals),Equals) -> (ia::e,ib::f,ic::g,id::h,Equals)
    | ((e,f,g,h,Equals),Contains) -> (ia::e,ib::f,ic::g,id::h,Contains)
    | ((e,f,g,h,Contains),Equals) -> (ia::e,ib::f,ic::g,id::h,Contains)
    | ((e,f,g,h,Contains),Contains) -> (ia::e,ib::f,ic::g,id::h,Contains)
    | ((e,f,g,h,Unrelated),_) -> (ia::e,ib::f,ic::g,id::h,Unrelated)
    | ((e,f,g,h,Extends),Contains) -> (ia::e,ib::f,ic::g,id::h,Extends)
    | ((e,f,g,h,Extends),Equals) -> (ia::e,ib::f,ic::g,id::h,Extends)
    | ((e,f,g,h,Extends),Unrelated) -> (ia::e,ib::f,ic::g,id::h,Unrelated)
    | ((e,f,g,h,_),Unrelated) -> 
      if ia<ic or id<ib then (ia::e,ib::f,(min ia ic)::g,(max ib id)::h,Extends)
        else (ia::e,ib::f,ic::g,id::h,Unrelated) 
    | _ -> raise No_case)
  | _ -> raise No_case
(* conjectural algorithm for simplifying the representation of a pospace *)
let rec normal s =
  let rec iter tt =
    if PSet.is_empty tt then s else
      let ((a,b),(c,d)) = PSet.choice tt in
        match (compare_cubes (a,b) (c,d)) with
          (_,_,_,_,Contains) -> normal (PSet.remove (c,d) s)
        | (_,_,e,f,Extends) -> normal (PSet.add (e,f) (PSet.remove (c,d) s))
        | _ -> iter (PSet.remove ((a,b),(c,d)) tt)
  in
    iter (PSet.prod s s)
(* boxminus (a,b) (c,d) returns the set of cubes obtains after cutting (c,d) 
*  from (a,b) *)
let boxminus (a,b) (c,d) =
  let (e,f) = (sup a c, inf b d) in
    if not (leq e f) then (PSet.add (a,b) PSet.empty) else
      if (a,b)=(e,f) then PSet.empty else
        let vertices x y =
          let s = Seq.seq_from_list x
          and t = Seq.seq_from_list y
          in
            let rec loop n set =
              if n=Nat.zero then set else
                loop (Nat.pred n) 
                     (PSet.add (Seq.list_from_seq (Seq.change (Nat.pred n) 
                              (Seq.access t (Nat.pred n)) s)) set)
            in
              loop (Seq.size s) PSet.empty 
        in
            normal
            (PSet.union  
            (PSet.map (function x->(x,b)) (vertices a d))
            (PSet.map (function x->(a,x)) (vertices c b))) 
(* concat s t returns set of all possible concatenations of pairs lists in s
 * with pairs of lists in t *)
let concat s t =
  let laux (a,b) u =
    let raux (c,d) v = 
      PSet.add (List.concat [a;c],List.concat [b;d]) v
    in
      PSet.fold raux t u
  in
    PSet.fold laux s PSet.empty
(* boxsect s t returns set of all pairwise intersections of cubes in s with
 * cubes in t *) 
let boxsect s t =
  let st = PSet.prod s t in
    let aux ((a,b),(c,d)) res =
      let (e,f) = (sup a c, inf b d) in
        if (leq e f) then PSet.add (e,f) res else res
  in
    normal (PSet.fold aux st PSet.empty)    
(****************************************************************************)
let prod p q =
  match (p.dim,q.dim) with
    (None,_) -> null
  | (_,None) -> null
  | (Some(m),Some(n)) ->
      {dim=Some(Nat.prod m n); boxes=concat p.boxes q.boxes}
let min p =
  let minimal x = 
    let aux (a,b) decision = decision && not ((less a x) && (leq x b)) in
      PSet.fold aux p.boxes true
  and candidates = PSet.map (function (x,y)->x) p.boxes in
    let chooser e set =
      if (minimal e) then PSet.add e set
        else set
    in
      PSet.fold chooser candidates PSet.empty
let max p =
  let maximal x = 
    let aux (a,b) decision = decision && not ((less x b) && (leq a x)) in
      PSet.fold aux p.boxes true
  and candidates = PSet.map (function (x,y)->y) p.boxes in
    let chooser e set =
      if (maximal e) then PSet.add e set
        else set
    in
      PSet.fold chooser candidates PSet.empty
let mem x p =
  let iterand (a,b) decision = ((leq a x) && (leq x b)) or decision in
    PSet.fold iterand p.boxes false
let union p q =
  let _ = checkdim p q in {dim=p.dim; boxes=normal (PSet.union p.boxes q.boxes)}
let intersect p q =
  let _ = checkdim p q in
    {dim=p.dim; boxes=boxsect p.boxes q.boxes}
let rec minus p q =
  let r = boxsect p.boxes q.boxes in
    let laux (a,b) s =
      let raux (c,d) t =
        let (e,f)=(sup a c,inf b d) in
          if not (leq e f) then t else
             boxsect t (boxminus (a,b) (e,f))
       in
         PSet.union (PSet.fold raux r (PSet.singleton (a,b))) s
     in
       {dim=p.dim; boxes=normal (PSet.fold laux p.boxes PSet.empty)}
let subset p q =
  let _ = checkdim p q in
    let laux (a,b) i =
      let raux (c,d) j = ((leq c a) && (leq b d)) or j in
        (PSet.fold raux q.boxes false) && i
    in
      PSet.fold laux p.boxes true
let proj p n =
  let rec box_proj (a,b) n =
    if (Nat.compare n Nat.zero)=0 then (a,b) else
      match (a,b) with
        ([],[]) -> raise (Dimension_mismatch (0,Nat.sgn n))
      | (ic::c,id::d) ->
        let (e,f) = box_proj (c,d) (Nat.pred n) in
          if (Nat.compare n Nat.one)=0 then (e,f) else (ic::e,id::d)
      | _ -> raise No_case
   in
     match p.dim with
       None -> p
     | Some(d) -> 
       let proj_n (a,b) = box_proj (a,b) n in
         {dim=Some(Nat.pred d); boxes=PSet.map proj_n p.boxes} 
let bounded_lattices p =
  let compare (a,b) (c,d) =
    if (a,b)=(c,d) then 0
      else if (leq a c) && (leq c b) then -1
        else if (leq c a) && (leq a d) then 1
          else 0
  in
    let rec sculpt l =
      match l with
        [] -> null
      | (e,f)::[] -> interval e f
      | (e,f)::t -> union (interval e f) (sculpt t)
    and hasse = PPos.order compare p.boxes 
    in
      PSet.map sculpt (PPos.chains hasse)
let lattices p =
  let restrictions = PSet.prod (min p) (max p)
  and aux (a,b) res = 
    let restrict = interval a b in
      PSet.union (bounded_lattices (intersect p restrict)) res
  in
    PSet.fold aux restrictions PSet.empty
