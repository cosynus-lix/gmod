(** Internal representation of programs. *)

open Stdlib

(** Types. *)
module T = struct
  type t =
    | Void
    | Bool
    | Int
    | Mutex
    | Semaphore
    | Barrier

  let to_string = function
    | Void -> "void"
    | Int -> "int"
    | Bool -> "bool"
    | Mutex -> "mutex"
    | Semaphore -> "semaphore"
    | Barrier -> "barrier"
end

(** Expressions. *)
module E = struct
  type value =
    | Bot (** undefined value *)
    | Int of int
    | Bool of bool

  type t =
    | Val of value
    | Assign of string * t
    | New_var of T.t * string * t option
    | Assert of t
    | Var of string

  let rec to_string e =
    let to_string e =
      match e with
      | Var _ | Val _ -> to_string e
      | _ -> Printf.sprintf "(%s)" (to_string e)
    in
    match e with
    | Assign (s,e) -> Printf.sprintf "%s = %s" s (to_string e)
    | New_var (t,v,e) -> Printf.sprintf "%s %s" (T.to_string t) v
    | Var v -> v
    | Val Bot -> "?"
    | Val (Int n) -> string_of_int n
    | Val (Bool b) -> Printf.sprintf "%b" b
    | Assert e -> Printf.sprintf "assert(%s)" (to_string e)

  let rec subst s = function
    | Val _ as e -> e
    | Assign (x,e) -> Assign (x, subst s e)
    | New_var (t,x,e) -> New_var (t,x,Option.map (subst s) e)
    | Assert e -> Assert (subst s e)
    | Var x -> (try List.assoc x s with Not_found -> Var x)
end

(** Programs. *)
module P = struct
  type t =
    | Spawn of t
    | P of string
    | V of string
    | Cmd of E.t
    | Seq of t list
    | If of E.t * t * t
    | While of E.t * t
    | Call of string * E.t list
    | Return of E.t

  (** Function declaration. *)
  type decl =
    {
      typ : T.t;
      name : string;
      args : (T.t * string) list option;
      prog : t option;
    }
end
