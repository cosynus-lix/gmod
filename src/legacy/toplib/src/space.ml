module type TOPE =
  sig
   type t
   val compare : t -> t -> int
   val hom : Nat.t -> Nat.t -> (t -> 'a -> 'a) -> 'a -> 'a
   val malcev : t -> t -> t -> t option
   val id : Nat.t -> t
   val compose : t -> t -> t
   val dom : t -> Nat.t
   val codom : t -> Nat.t
   val tex : t -> string -> string
  end

module type POLYTOPE = 
  sig
    type o
    type 'a t 
    exception Dimension_mismatch of (int*int)
    val null : 'a t
    val standard : Nat.t -> 'a -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val union : 'a t -> 'a t -> 'a t
    val quotient : 'a t -> o -> 'a -> o -> 'a -> 'a t
    val topes : 'a t -> Nat.t -> (o * 'a) PSet.t
    val solve : 'a t -> o -> o -> 'a -> (o * 'a) PSet.t
    val dim : 'a t -> Nat.t option
    val mem : o -> 'a -> 'a t -> bool
    val fold : (o -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val tex : string t -> string
  end 

  module Make (Tope : TOPE) : POLYTOPE with type o = Tope.t =
    struct
      type o = Tope.t
      type 'a t = 
        { labels: 'a PSet.t Seq.t; rules: (Tope.t * 'a, Tope.t * 'a) PMap.t}
      exception Dimension_mismatch of (int*int)
      let malcev (t,a) (u,b) (v,c) =
        if a<>b then (t,a)
          else match (Tope.malcev t u v) with
            None -> (t,a)
          | Some(w) -> (w,c)
      let compare (t,a) (u,b) =
        if (Tope.compare t u)=0 then (Pervasives.compare a b)
          else Tope.compare t u
      let sort (t,a) (u,b) =
        if (compare (t,a) (u,b)) < 0 then ((u,b),(t,a)) else ((t,a),(u,b))
      (* union (x,y) returns union of x and y, where None is regarded as the
       * empty set and Some(s) is regarded as s *)
      let union (x,y) =
          match (x,y) with
            (Some(x),Some(y)) -> PSet.union x y
          | (Some(x),None) -> x
          | (None,Some(y)) -> y
          | (None,None) -> PSet.empty
      let mem t a x =
        try (PSet.mem a (Seq.access x.labels (Tope.dom t))) 
          with Not_found -> false
      (* reduce r t a reduces (t,a) with respect to rewriting rules r *)
      let reduce r t a =
          let rec pass (u,b) = 
            let aux (v,c) (w,d) (x,e) = malcev (x,e) (v,c) (w,d) in
              PMap.foldi aux r (u,b)
          in
            let (y,f) = pass (t,a) in if (t,a)=(y,f) then (t,a) else pass (y,f)
      (* label x a n returns sequence of (at least n) sets of labels coinciding
       * with the labels of x, except that the nth set also contains a *)
      let label x a n =
        let y =
          let z = Seq.constant (Nat.succ n) PSet.empty in 
            Seq.map union (Seq.prod x z)
        in
          Seq.change n (PSet.add a (Seq.access y n)) y 
      (* rerule r returns a simplified version of rewriting system r, where
       * all the rhs terms are themselves normalized *)
      let rerule r =
        let aux (t,a) _ s = PMap.add (t,a) (reduce r t a) s in
          PMap.foldi aux r PMap.empty       
      let rec add (t,a) (u,b) r =
        let ((v,c),(w,d)) = ((reduce r t a),(reduce r u b))
        in
          let ((v,c),(w,d))=sort (v,c) (w,d) in
            if (v,c)=(w,d) then r
              else
                let aux (x,e) (y,f) s =
                  PMap.add (malcev (x,e) (v,c) (w,d)) 
                           (malcev (y,f) (v,c) (w,d)) s
                in
                  rerule (PMap.add (v,c) (w,d) (PMap.foldi aux r PMap.empty))
      let map f x = 
        let aux (t,a) (u,b) r = add (t,(f a)) (u,(f b)) r in
          { labels=(Seq.map (PSet.map f) x.labels); 
            rules=PMap.foldi aux x.rules PMap.empty}
      let union x y = 
        { labels=(Seq.map union (Seq.prod x.labels y.labels)); 
          rules=PMap.foldi add x.rules y.rules }  
      let null = { labels=Seq.null; rules=PMap.empty }
      let standard n l = 
        { labels=Seq.change n (PSet.add l PSet.empty) 
          (Seq.constant (Nat.succ n) PSet.empty); rules=PMap.empty }
      let dim x = 
        try (Some(Nat.pred (Seq.size x.labels))) with Not_found -> None  
     (* fold f x a folds x from top dimension downwards *)
     let fold f x a =
        (* recursive fold function on each dimension *)
        let rec degwise n b =
          if (Nat.compare n Nat.zero)=0 then b else
            let s = Seq.access x.labels (Nat.pred n)
            and g e c = f (Tope.id (Nat.pred n)) e c 
            in
              degwise (Nat.pred n) (PSet.fold g s b) 
        in
          degwise (Seq.size x.labels) a
      (* quotient x by relation (t,a)~(u,b) *)
      let quotient x t a u b =
        if (not (mem t a x)) then raise Not_found
          else 
            if (Tope.codom t)<>(Tope.codom u) then 
              raise (Dimension_mismatch
                    ((Nat.sgn (Tope.codom t)),(Nat.sgn (Tope.codom u))))
                else {labels=label x.labels b (Tope.dom u); 
                      rules=add (t,a) (u,b) x.rules} 
      let topes x n =
        match (dim x) with
          None -> PSet.empty
        | Some(d) -> 
          (* faces i tt returns set of i-topes based on tt *)
          let rec faces i tt =
            if (Nat.compare i d)>0 then tt else
              let aux t uu =
                let vv = 
                  try (Seq.access x.labels (Tope.dom t)) with 
                    Not_found -> PSet.empty
                in
                  (* op a ww applies t to a and adds result to ww *)
                  let op a ww = PSet.add (reduce x.rules t a) ww
                  in
                    PSet.fold op vv uu
              in
                faces (Nat.succ i) (Tope.hom i n aux PSet.empty)
          in
            faces n PSet.empty
      let solve x t u b =
        if (Tope.codom t)<>(Tope.codom u) then PSet.empty 
          else
            let (v,c)=reduce x.rules u b in
              let tt = 
                topes x (Tope.dom t)
              and aux (w,d) uu =
                let (x,e)=(reduce x.rules (Tope.compose t w) d) in
                  if (x,e)=(v,c) then PSet.add (w,d) uu
                    else uu
              in
                PSet.fold aux tt PSet.empty
      let tex x = 
        let iterand (t,a) (u,b) seq =
          Seq.expand ((Tope.tex t a)^"="^(Tope.tex u b)) seq
        in
          match (dim x) with
            Some(n) ->
              "dim "^(Nat.tex n)^
              "; generating topes: "^(Seq.tex (Seq.map PSet.tex x.labels))^
              "; relations: "^(Seq.tex (PMap.foldi iterand x.rules Seq.null))
          | None -> "{}"

    end
