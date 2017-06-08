open Globals
open Type
open Color

let cube p = 
  let calculation t answer =
    let rec aux_ter t = match t with 
      | Cbx.Variable(s)             -> Mos.find s answer 
      | Cbx.Brick(c)                -> AC.of_cube                (C.normalize c)
      | Cbx.Complement(t)           -> AC.complement             (aux_ter t)
      | Cbx.Downward(t)             -> AC.downward               (aux_ter t) 
      | Cbx.Upward(t)               -> AC.upward                 (aux_ter t) 
      | Cbx.Cube(t)                 -> AC.of_cube (AC.cubic_hull (aux_ter t))
      | Cbx.Closure(t)              -> AC.closure                (aux_ter t) 
      | Cbx.Interior(t)             -> AC.interior               (aux_ter t)
      | Cbx.Cubical_order_convex(t) -> AC.cubical_order_convex   (aux_ter t)
      | Cbx.Boundary(t)             -> AC.boundary               (aux_ter t)
      | Cbx.Normalize(t)            -> AC.normalize              (aux_ter t)
      | Cbx.Compress(t)             -> AC.compress               (aux_ter t)
      | Cbx.Upper_corners(t)        -> AC.upper_corners_area     (aux_ter t)
      | Cbx.Deadlocks(t)            -> AC.deadlocks              (aux_ter t)
      | Cbx.Reachable(t)            -> AC.reachable              (aux_ter t)
      | Cbx.Might_go_deadlock(t)    -> AC.might_go_deadlock      (aux_ter t)
      | Cbx.Might_go_infinity(t)    -> AC.might_go_infinity      (aux_ter t)
      | Cbx.Ginzu(t)                -> AC.ginzu                  (aux_ter t)
      | Cbx.Deadlock_attractor(t)   -> AC.deadlock_attractor     (aux_ter t)
      | Cbx.Infinity_attractor(t)   -> AC.infinity_attractor     (aux_ter t)
      | Cbx.Base(t)                 -> AC.base                   (aux_ter t)
      | Cbx.Common(t)               -> AC.common                 (aux_ter t)
      | Cbx.Union(t1,t2)            -> AC.join                   (aux_ter t1) (aux_ter t2)
      | Cbx.Intersection(t1,t2)     -> AC.meet                   (aux_ter t1) (aux_ter t2)
      | Cbx.Difference(t1,t2)       -> AC.difference             (aux_ter t1) (aux_ter t2)
      | Cbx.Past_cone(t1,t2)        -> AC.past_cone              (aux_ter t1) (aux_ter t2)
      | Cbx.Future_cone(t1,t2)      -> AC.future_cone            (aux_ter t1) (aux_ter t2)
      | Cbx.Product(t1,t2)          -> AC.product                (aux_ter t1) (aux_ter t2)
      | Cbx.Exponent(t,n)           -> AC.exponent               (aux_ter t) n
      | Cbx.Cubical_set(n,t)        -> 
	  ( 
	    match n with 
	      | 0 -> proj0 (AC.cset2D_of_complement_of_area (aux_ter t))
	      | 1 -> proj1 (AC.cset2D_of_complement_of_area (aux_ter t))
	      | 2 -> proj2 (AC.cset2D_of_complement_of_area (aux_ter t))
	      | _ -> failwith "The extracted cubical set is 2-dimensional"
	  )
    in
      aux_ter t
  in
  let (domain,moct) = 
    try
      let c = open_in_bin p in
      let aux = Parser_cubical_area_over_integer.result Lexer_cubical_area_over_integer.entry_point (Lexing.from_channel c) in
	close_in c;aux
    with
      | Sys_error _ -> failwith (Printf.sprintf "%s unable to open the file %s.\n" error (blue ~bold:true p))
      | Parsing.Parse_error -> failwith (Printf.sprintf "%s unable to parse the file %s given as an argument to %s.\n" error (blue ~bold:true p) opt_cbx)
  in
    try
      List.fold_left (fun accu name -> Mos.add name (calculation (Mos.find name moct) accu) accu) Mos.empty domain
    with
      | Sys_error s -> failwith ("close_in problem "^s) 

let tore p = 
  let calculation t answer =
    let rec aux_ter t = match t with 
      | Ctx.Variable(s)             -> Mos.find s answer 
      | Ctx.Brick(c)                -> AT.of_cube                (T.normalize c)
      | Ctx.Complement(t)           -> AT.complement             (aux_ter t)
	  (*      | Ctx.Cube(t)                 -> AT.of_cube (AT.cubic_hull (aux_ter t)) *)
      | Ctx.Closure(t)              -> AT.closure                (aux_ter t) 
      | Ctx.Interior(t)             -> AT.interior               (aux_ter t)
	  (*      | Ctx.Cubical_order_convex(t) -> AT.cubical_order_convex   (aux_ter t)  *)
      | Ctx.Boundary(t)             -> AT.boundary               (aux_ter t)
      | Ctx.Normalize(t)            -> AT.normalize              (aux_ter t)
      | Ctx.Compress(t)             -> AT.compress               (aux_ter t)
	  (*      | Ctx.Upper_corners(t)        -> AT.upper_corners_area     (aux_ter t)  *)
      | Ctx.Deadlocks(t)            -> AT.deadlocks              (aux_ter t)  
	  (*      | Ctx.Reachable(t)            -> AT.reachable              (aux_ter t)  *)
      | Ctx.Might_go_deadlock(t)    -> AT.might_go_deadlock      (aux_ter t)  
      | Ctx.Might_go_infinity(t)    -> AT.might_go_infinity      (aux_ter t)  
	  (*      | Ctx.Ginzu(t)                -> AT.ginzu                  (aux_ter t)  *)
      | Ctx.Deadlock_attractor(t)   -> AT.deadlock_attractor     (aux_ter t) 
      | Ctx.Infinity_attractor(t)   -> AT.infinity_attractor     (aux_ter t)  
      | Ctx.Union(t1,t2)            -> AT.join                   (aux_ter t1) (aux_ter t2)
      | Ctx.Intersection(t1,t2)     -> AT.meet                   (aux_ter t1) (aux_ter t2)
      | Ctx.Difference(t1,t2)       -> AT.difference             (aux_ter t1) (aux_ter t2)
      | Ctx.Past_cone(t1,t2)        -> AT.past_cone              (aux_ter t1) (aux_ter t2)
      | Ctx.Future_cone(t1,t2)      -> AT.future_cone            (aux_ter t1) (aux_ter t2)
      | Ctx.Product(t1,t2)          -> AT.product                (aux_ter t1) (aux_ter t2)
      | Ctx.Exponent(t,n)           -> AT.exponent               (aux_ter t) n
      | _                        -> failwith "calculations over tori : not implemented yet"
    in
      aux_ter t
  in
  let aux slnmt (* string list'n'map of trees *) = 
    let moct (* map of calculation tree *) = (snd slnmt) 
    in
    let rec aux_bis sl answer =
      match sl with
	| name::sl' -> aux_bis sl' (Mos.add name (calculation (Mos.find name moct) answer) answer)
	| []        -> answer
    in
      aux_bis (fst slnmt) Mos.empty
  in
    aux (Parser_torical_area_over_integer.result Lexer_torical_area_over_integer.entry_point (Lexing.from_string p))

let cyle p = 
  let calculation t answer =
    let rec aux_ter t = match t with 
      | Clx.Variable(s)             -> Mos.find s answer 
      | Clx.Brick(m)                -> AX.of_cube                (X.normalize m)
      | Clx.Complement(t)           -> AX.complement             (aux_ter t)
(*      | Clx.Cube(t)                 -> AX.of_cube (AX.cubic_hull (aux_ter t)) *)
      | Clx.Closure(t)              -> AX.closure                (aux_ter t) 
      | Clx.Interior(t)             -> AX.interior               (aux_ter t)
(*      | Clx.Cubical_order_convex(t) -> AX.cubical_order_convex   (aux_ter t)  *)
      | Clx.Boundary(t)             -> AX.boundary               (aux_ter t)
      | Clx.Normalize(t)            -> AX.normalize              (aux_ter t)
      | Clx.Compress(t)             -> AX.compress               (aux_ter t)
(*      | Clx.Upper_corners(t)        -> AX.upper_corners_area     (aux_ter t)  *)
      | Clx.Deadlocks(t)            -> AX.deadlocks              (aux_ter t)  
(*      | Clx.Reachable(t)            -> AX.reachable              (aux_ter t)  *)
      | Clx.Might_go_deadlock(t)    -> AX.might_go_deadlock      (aux_ter t)  
      | Clx.Might_go_infinity(t)    -> AX.might_go_infinity      (aux_ter t)  
(*      | Clx.Ginzu(t)                -> AX.ginzu                  (aux_ter t)  *)
      | Clx.Deadlock_attractor(t)   -> AX.deadlock_attractor     (aux_ter t) 
      | Clx.Infinity_attractor(t)   -> AX.infinity_attractor     (aux_ter t)  
      | Clx.Union(t1,t2)            -> AX.join                   (aux_ter t1) (aux_ter t2)
      | Clx.Intersection(t1,t2)     -> AX.meet                   (aux_ter t1) (aux_ter t2)
      | Clx.Difference(t1,t2)       -> AX.difference             (aux_ter t1) (aux_ter t2)
      | Clx.Past_cone(t1,t2)        -> AX.past_cone              (aux_ter t1) (aux_ter t2) 
      | Clx.Future_cone(t1,t2)      -> AX.future_cone            (aux_ter t1) (aux_ter t2) 
      | Clx.Product(t1,t2)          -> AX.product                (aux_ter t1) (aux_ter t2)
      | Clx.Exponent(t,n)           -> AX.exponent               (aux_ter t) n
      | _                           -> failwith "calculations over cylinders : not implemented yet"
    in
      aux_ter t
  in
  let aux slnmt (* string list'n'map of trees *) = 
    let moct (* map of calculation tree *) = (snd slnmt) 
    in
    let rec aux_bis sl answer =
      match sl with
        | name::sl' -> aux_bis sl' (Mos.add name (calculation (Mos.find name moct) answer) answer)
        | []        -> answer
    in
      aux_bis (fst slnmt) Mos.empty
  in
    aux (Parser_cylindrical_area_over_integer.result Lexer_cylindrical_area_over_integer.entry_point (Lexing.from_string p))
