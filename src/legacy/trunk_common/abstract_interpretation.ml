open Apron

let debug_transition = ref false

let man = Box.manager_alloc ()

(*
let path p =
  let env = ref (Environment.make [||] [||]) in
  let env_map = ref [] in
  let abs = ref (Abstract1.top man !env) in
    List.iter
      (function
         | ENew_var (TInt, v) ->
             let var = Var.of_string v in
               env_map := (v, (TInt,var)) :: !env_map;
               env := Environment.add !env [|var|] [||];
               abs := Abstract1.change_environment man !abs !env false
         | EAssign (v,e) ->
             let var = snd (List.assoc v !env_map) in
               abs := Abstract1.assign_linexpr man !abs var (Apron.Parser.linexpr1_of_string !env (string_of_expr e)) None
         | _ -> assert false
      ) p;
    !abs
*)

let get_some = function
  | Some x -> x
  | None -> assert false

type type_expr = TInt

(*
(* TODO *)
let string_of_expr _ = assert false
let string_of_constr _ = assert false

module Apron =
struct
  type 'a t =
      {
        env : Environment.t;
        env_map : (string * (type_expr * Var.t)) list;
        abs : 'a Abstract1.t;
      }

  let create () =
    let env = Environment.make [||] [||] in
    {
      env = env;
      env_map = [];
      abs = Abstract1.top man env;
    }

  let add_var e v =
    let var = Var.of_string v in
    let env_map = (v, (TInt,var)) :: e.env_map in
    let env = Environment.add e.env [|var|] [||] in
    let abs = Abstract1.change_environment man e.abs env false in
    {
      env = env;
      env_map = env_map;
      abs = abs;
    }

  let assign e v expr =
    let var = snd (List.assoc v e.env_map) in
    let abs = Abstract1.assign_linexpr man e.abs var (Apron.Parser.linexpr1_of_string e.env (string_of_expr expr)) None in
    {e with abs = abs}

  let meet_cons e cons =
    let cons = List.map string_of_constr cons in
    let abs = Abstract1.meet_lincons_array man e.abs (Parser.lincons1_of_lstring e.env cons) in
    {e with abs = abs}

  let to_string e =
    let buf = Buffer.create 10 in
    Format.bprintf buf "%a" Abstract1.print e.abs;
    Buffer.contents buf

  (* Both environments are assumed to have the same declared variables. *)
  let join e1 e2 =
    {e1 with abs = Abstract1.join man e1.abs e2.abs}

  let meet e1 e2 =
    {e1 with abs = Abstract1.meet man e1.abs e2.abs}

  let widening e1 e2 =
    {e2 with abs = Abstract1.join (*widening*) man e1.abs e2.abs}

  let is_eq e1 e2 =
    Abstract1.is_eq man e1.abs e2.abs
end
*)

module Interval =
struct
  module Num =
  struct
    type t = int

    let bot = min_int

    let top = max_int

    let is_extr n =
      n = min_int || n = max_int

    (* TODO: assert that the addition is defined ie no infty-infty *)
    let add m n =
      if is_extr m then m
      else if is_extr n then n
      else m + n

    let sup (m:t) (n:t) = max m n

    let inf (m:t) (n:t) = min m n

    let to_string n =
      if n = bot then "-∞"
      else if n = top then " ∞"
      else Printf.sprintf "%02s" (string_of_int n)
  end

  (** Domain of intervals. *)
  module Int =
  struct
    type t = (Num.t * Num.t) option

    let make (m, n) =
      assert (m <= n);
      Some (m, n)

    let may_make (m, n) =
      if m <= n then
        Some (m, n)
      else
        None

    let bot = None

    let top = make (Num.bot, Num.top)

    let to_string i =
      match i with
        | None -> "⊥       "
        | i when i = top -> "⊤       "
        | Some (i1, i2) -> Printf.sprintf "[%s, %s]" (Num.to_string i1) (Num.to_string i2)

    let join i j =
      match i, j with
        | None, _ -> j
        | _, None -> i
        | Some i, Some j ->
          let i1, i2 = i in
          let j1, j2 = j in
          may_make (Num.inf i1 j1, Num.sup i2 j2)

    let meet i j =
      match i, j with
        | None, _ -> j
        | _, None -> i
        | Some i, Some j ->
          let i1, i2 = i in
          let j1, j2 = j in
          may_make (Num.sup i1 j1, Num.inf i2 j2)

    let add i j =
      match i, j with
        | None, _ -> None
        | _, None -> None
        | Some (i1, i2), Some (j1, j2) ->
          make (Num.add i1 j1, Num.add i2 j2)

    let closure_down i =
      match i with
        | None -> None
        | Some (n, _) -> Some (Num.bot, n)

    let closure_up i =
      match i with
        | None -> None
        | Some (n, _) -> Some (n, Num.top)

    (** Simulate an interval open in the first bound. *)
    let open_down i =
      match i with
        | None -> None
        | Some (i1, i2) -> Some (Num.add i1 1, i2)

    let open_up i =
      match i with
        | None -> None
        | Some (i1, i2) -> Some (i1, Num.add i2 1)

    let rec of_expr var = function
      | AbstractSyntax.Ac n -> make (n, n)
      | AbstractSyntax.Va v -> var v
      | AbstractSyntax.Pl (e1, e2) -> add (of_expr var e1) (of_expr var e2)
      | _ -> assert false

    let widening i j =
      match i with
        | None ->
          (
            match j with
              | None -> None
              | _ -> top
          )
        | Some (i1, i2) ->
          let j1, j2 = get_some j in
          assert (j1 <= i1);
          assert (i2 <= j2);
          make
            ((if i1 = j1 then i1 else Num.bot),
             (if i2 = j2 then i2 else Num.top))
  end

  type t = (string * Int.t) list

  let create () = []

  let add_var e v = (v, Int.top)::e

  let rm_var e v = List.filter (fun (w, _) -> w <> v) e

  let val_var e v = List.assoc v e

  let assign_int e v i = (v, i)::(rm_var e v)

  let assign e v expr = assign_int e v (Int.of_expr (val_var e) expr)

  let meet_int e v i =
    assign_int e v (Int.meet i (val_var e v))

      (*
  let rec meet_cons e cons =
    match cons with
      | EIs_eq (EVar v, e2) ->
        let i = Int.of_expr (val_var e) e2 in
        meet_int e v i
      | ELe (EVar v, e2) ->
        let i = Int.of_expr (val_var e) e2 in
        meet_int e v (Int.closure_down i)
      | ELe (e1, EVar v) ->
        let i = Int.of_expr (val_var e) e1 in
        meet_int e v (Int.closure_up i)
      | ELt (EVar v, e2) ->
        let i = Int.of_expr (val_var e) e2 in
        meet_int e v (Int.open_up (Int.closure_down i))
      | ELt (e1, EVar v) ->
        let i = Int.of_expr (val_var e) e1 in
        meet_int e v (Int.open_down (Int.closure_up i))
      | _ -> assert false

  let meet_cons e cons =
    List.fold_left meet_cons e cons
      *)

  let iter2 f e1 e2 =
    List.iter (fun (v, i) -> f v i (val_var e2 v)) e1

  let fold2 f x e1 e2 =
    let x = ref x in
    iter2 (fun v i j -> x:= f !x v i j) e1 e2;
    !x

  let join e1 e2 =
    fold2 (fun e v i j -> assign_int e v (Int.join i j)) (create ()) e1 e2

  let widening e1 e2 =
    fold2 (fun e v i j -> assign_int e v (Int.widening i j)) (create ()) e1 e2

  let is_eq e1 e2 =
    List.for_all (fun (v, i) -> val_var e2 v = i) e1

  let to_strings e =
    List.map
      (fun (v, i) -> Printf.sprintf "%s∊%s" v (Int.to_string i))
      (List.sort (fun (v1,_) (v2,_) -> compare v1 v2) e)

  let to_string e =
    List.fold_left
      (fun s (v, i) ->
        Printf.sprintf "%s  %s∊%s" s v (Int.to_string i)
      ) "" (List.sort (fun (v1,_) (v2,_) -> compare v1 v2) e)
end

module A = Interval (* Apron *)

module Environment = struct
  let create () = A.create ()

  let transition e p =
    (* Printf.printf "  %s\n%!" (Lang.string_of_expr p); *)
    match p with
      | AbstractSyntax.D (AbstractSyntax.T_int,v) -> A.add_var e v
      | AbstractSyntax.T (v,expr) -> A.assign e v expr
      | _ -> e

  let join = A.join

  let to_strings = A.to_strings

  let to_string = A.to_string
end

module Env = Environment

let eval p =
  List.fold_left (fun e i -> Environment.transition e i) (Environment.create ()) p

let () =
  Display_with_gtk2.abstract_interpretation := (fun p -> Environment.to_strings (eval p))
