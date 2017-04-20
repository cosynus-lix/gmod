(** Types. *)
module T = struct
  type t =
    | Void
    | Bool
    | Int
    | Mutex

  let to_string = function
    | Void -> "void"
    | Int -> "int"
    | Bool -> "bool"
    | Mutex -> "mutex"
end

(** Expressions. *)
module E = struct
  type t =
    | Assign of string * t
    | New_var of T.t * string
    | Assert of t
    | Return of t
    | Var of string
    | Int of int
    | Bool of bool
    | Add of t * t
    | Sub of t * t
    | Not of t
    | Mult of t * t
    | Div of t * t
    | Is_eq of t * t
    | Le of t * t
    | Lt of t * t
    | And of t * t
    | Or of t * t

  let rec to_string e =
    let to_string e =
      match e with
      | Var _ | Int _ -> to_string e
      | _ -> "(" ^ to_string e ^ ")"
    in
    match e with
    | Assign (s,e) -> Printf.sprintf "%s = %s" s (to_string e)
    | New_var (t,v) -> Printf.sprintf "%s %s" (T.to_string t) v
    | Var v -> v
    | Int n -> string_of_int n
    | Bool b -> if b then "true" else "false"
    | Add (e1, e2) -> Printf.sprintf "%s+%s" (to_string e1) (to_string e2)
    | Sub (e1, e2) -> Printf.sprintf "%s-%s" (to_string e1) (to_string e2)
    | Mult (e1, e2) -> Printf.sprintf "%s*%s" (to_string e1) (to_string e2)
    | Div (e1, e2) -> Printf.sprintf "%s/%s" (to_string e1) (to_string e2)
    | Le (e1, e2) -> Printf.sprintf "%s <= %s" (to_string e1) (to_string e2)
    | Lt (e1, e2) -> Printf.sprintf "%s < %s" (to_string e1) (to_string e2)
    | Is_eq (e1, e2) -> Printf.sprintf "%s == %s" (to_string e1) (to_string e2)
    | Not e -> Printf.sprintf "!%s" (to_string e)
    | And (e1, e2) -> Printf.sprintf "%s && %s" (to_string e1) (to_string e2)
    | Or (e1, e2) -> Printf.sprintf "%s || %s" (to_string e1) (to_string e2)
    | Return e -> Printf.sprintf "return %s" (to_string e)
    | Assert e -> Printf.sprintf "assert(%s)" (to_string e)
end

(** Programs. *)
module P = struct
  type t =
    | Spawn of app
    | P of string
    | V of string
    | Cmd of E.t
    | Seq of t list
    | If of E.t * t * t
    | While of E.t * t
    | Call of app
   and app =
     string * E.t list
              

  (** Function declaration. *)
  type decl =
    {
      typ : T.t;
      name : string;
      args : (T.t * string) list option;
      prog : t option;
    }
end
