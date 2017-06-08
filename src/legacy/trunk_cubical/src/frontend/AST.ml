type loc = Lexing.position * Lexing.position

module Type =
struct
  type t =
  | Mutex of int
  | Int
end

module T = Type

module Expr =
struct
  type var = string

  type t =
    {
      desc : desc;
      loc : loc;
    }
  and desc =
  | P of var
  | V of var
  | Seq of t * t
  | Spawn of t
end

module E = Expr

module Prog =
struct
  (* TODO: do we want to emit all globals? *)
  type t =
    {
      main : E.t;
    }
end
