(** Internal representation of programs. *)

open Mystdlib

(** Types. *)
module T = struct
  type t =
    | Void
    | Bool
    | Int
    | Fun of t array * t
    | Mutex

  let rec to_string = function
    | Void -> "void"
    | Int -> "int"
    | Bool -> "bool"
    | Fun (a,t) ->
       let a = Array.to_list a in
       let a = List.map to_string a in
       let a = String.concat "," a in
       let t = to_string t in
       Printf.sprintf "(%s) -> %s" a t
    | Mutex -> "mutex"
end

(** Expressions. *)
module E = struct
  (** Operations. *)
  module Op = struct
    type t =
      | Add
  end

  (** values *)
  type value =
    | Bot (** undefined value *)
    | Int of int (** an integer *)
    | Bool of bool (** a boolean *)

  type t =
    | Val of value (** a value *)
    | Assign of string * t (** change a variable *)
    | New_var of T.t * string * t option (** declare a variable *)
    | Assert of t (** assert a boolean condition *)
    | Var of string (** contents of a variable *)
    | Op of Op.t * t array (** an operation *)

  let rec to_string e =
    let to_string e =
      match e with
      | Var _ | Val _ -> to_string e
      | _ -> Printf.sprintf "(%s)" (to_string e)
    in
    match e with
    | Assign (s,e) -> Printf.sprintf "%s = %s" s (to_string e)
    | New_var (t,v,_) -> Printf.sprintf "%s %s" (T.to_string t) v
    | Var v -> v
    | Val Bot -> "?"
    | Val (Int n) -> string_of_int n
    | Val (Bool b) -> Printf.sprintf "%b" b
    | Assert e -> Printf.sprintf "assert(%s)" (to_string e)
    | Op(Op.Add,e) -> Printf.sprintf "%s + %s" (to_string e.(0)) (to_string e.(1))

  let rec subst s = function
    | Val _ as e -> e
    | Assign (x,e) -> Assign (x, subst s e)
    | New_var (t,x,e) -> New_var (t,x,Option.map (subst s) e)
    | Assert e -> Assert (subst s e)
    | Var x -> (try List.assoc x s with Not_found -> Var x)
    | Op(o,e) -> Op(o, Array.map (subst s) e)
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
end

(** Declarations. *)
module D = struct
  (** A declaration. *)
  type decl =
    {
      name : string;
      typ : T.t;
      args : (T.t * string) array option;
      prog : P.t;
    }

  let fct typ name args prog =
    { name; typ; args = Some args; prog }

  let cst typ name prog =
    { name; typ; args = None; prog }

  type t = decl list
end
