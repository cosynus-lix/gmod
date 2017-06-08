(* This file should contains all the types pervasively used in ALCOOL *)

open Color

exception Stop

exception Undefined

(* Sos.t : Set of string *)
module Sos = Set.Make (String)

(* Mos : Maps whose keys are strings *)
module Mos = Map.Make (String)

module Idugsv (* Imperative Directed Unlabelled Graphs with String Vertices *) = 
  Graph.Imperative.Digraph.ConcreteBidirectional
	(
		struct
		  type t = string
		  let default = ""
		  let compare = String.compare
		  let hash = String.length
		  let equal t1 t2 = (t1=t2)
		end
	)

module Scc_Idugsv = Graph.Components.Make(Idugsv)

(* domofdef : domain of definition *)

let domofdef m = Mos.fold (fun key x accu -> Sos.add key accu) m Sos.empty

open BuiltInWithoutParsers

module Geometric_model = Geometric_model.Make(Area)(*Co*)(*CS*)(ForbiddenExtension) (*to be altered*)

module Cbx = 
struct
  type t = 
    | Variable             of  string
    | Brick                of  C.t
    | Complement           of  t
    | Downward             of  t
    | Upward               of  t
    | Closure              of  t
    | Cube                 of  t
    | Interior             of  t
    | Cubical_order_convex of  t
    | Boundary             of  t
    | Normalize            of  t
    | Compress             of  t
    | Upper_corners        of  t
    | Deadlocks            of  t
    | Reachable            of  t
    | Might_go_deadlock    of  t
    | Might_go_infinity    of  t
    | Ginzu                of  t
    | Deadlock_attractor   of  t
    | Infinity_attractor   of  t
    | Base                 of  t
    | Common               of  t
    | Union                of (t * t)
    | Intersection         of (t * t)
    | Difference           of (t * t)
    | Past_cone            of (t * t)
    | Future_cone          of (t * t)
    | Product              of (t * t)
    | Exponent             of (t * int)
(*
    | Cubical_set          of (int * t)
*)
end

module Ctx = 
struct
type t = 
  | Variable             of  string
  | Brick                of  T.t
  | Complement           of  t
  | Downward             of  t
  | Upward               of  t
  | Closure              of  t
  | Cube                 of  t
  | Interior             of  t
  | Cubical_order_convex of  t
  | Boundary             of  t
  | Normalize            of  t
  | Compress             of  t
  | Upper_corners        of  t
  | Deadlocks            of  t
  | Reachable            of  t
  | Might_go_deadlock    of  t
  | Might_go_infinity    of  t
  | Ginzu                of  t
  | Deadlock_attractor   of  t
  | Infinity_attractor   of  t
  | Union                of (t * t)
  | Intersection         of (t * t)
  | Difference           of (t * t)
  | Past_cone            of (t * t)
  | Future_cone          of (t * t)
  | Product              of (t * t)
  | Exponent             of (t * int)
end

module Clx =
struct
type t = 
  | Variable             of  string
  | Brick                of  X.t
  | Complement           of  t
  | Downward             of  t
  | Upward               of  t
  | Closure              of  t
  | Cube                 of  t
  | Interior             of  t
  | Cubical_order_convex of  t
  | Boundary             of  t
  | Normalize            of  t
  | Compress             of  t
  | Upper_corners        of  t
  | Deadlocks            of  t
  | Reachable            of  t
  | Might_go_deadlock    of  t
  | Might_go_infinity    of  t
  | Ginzu                of  t
  | Deadlock_attractor   of  t
  | Infinity_attractor   of  t
  | Union                of (t * t)
  | Intersection         of (t * t)
  | Difference           of (t * t)
  | Past_cone            of (t * t)
  | Future_cone          of (t * t)
  | Product              of (t * t)
  | Exponent             of (t * int)
end

(* Set of keys binded in a map of strings *)

let forbidden          = ref (AC.empty ~d:0 ())
and state              = ref (AC.empty ~d:0 ())
and sources            = ref (AC.empty ~d:0 ())
and deadlocks          = ref (AC.empty ~d:0 ())
and deadlock_attractor = ref (AC.empty ~d:0 ())
and hopeful            = ref (AC.empty ~d:0 ())
and local_deadlock     = ref (AC.empty ~d:0 ())
and reachable          = ref (AC.empty ~d:0 ())
and unreachable        = ref (AC.empty ~d:0 ())
and safe               = ref (AC.empty ~d:0 ())
and hazardous          = ref (AC.empty ~d:0 ())
and overflow           = ref (AC.empty ~d:0 ())
and reachable_overflow = ref (AC.empty ~d:0 ())
and critical_section   = ref (AC.empty ~d:0 ())
and printable_list_of_conflicts:((string list) ref) = ref []
(*
and cset               = ref (CS.empty 0)
and maximal_dipaths    = ref (Co.empty)
*)

let dod_of_map_of_strings m (* dod : Domain Of Definition *) = 
  let df = ref (Sos.empty) in
  let aux s a = (df := (Sos.add s (!df))) in
  Mos.iter aux m ; (!df)
