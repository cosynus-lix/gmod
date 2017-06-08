open Common
open Cil

(* Parse a C file. *)
let parse fname =
  initCIL ();
  Printf.printf "Using CIL %s parser.\n%!" Cil.cilVersion;
  let f = Frontc.parse fname () in
  dumpFile defaultCilPrinter stdout fname f;
  let globals = f.globals in
  let main =
    List.find_map
      (function
      | GFun (f,_) when f.svar.vname = "main" -> Some f
      | _ -> None
      ) globals
  in
  let resources = ref [] in
  let variables = ref [] in

  (* Register global variables and resources. *)
  let add_global_var v =
    match v.vtype with
    | TNamed (i,_) when i.tname = "pthread_mutex_t" ->
      resources := (v, `Mutex `Normal) :: !resources
    | TNamed (i,_) when i.tname = "pthread_barrier_t" ->
      (* We don't know yet the arity of the barrier. *)
      resources := (v, `Barrier None) :: !resources
    | _ -> variables := v.vname :: !variables
  in
  List.iter
    (function
    | GVar (v,_,_) -> add_global_var v
    | _ -> ()
    ) globals;
  List.iter add_global_var main.slocals;
  Printf.printf "We have %d resources and %d variables.\n%!" (List.length !resources) (List.length !variables);

  (* Functions to build processes. *)
  let processes = ref [] in
  let rec process_block b =
    List.flatten
      (List.map
         (fun s ->
           (* Printf.printf "statement: %s\n%!" (Pretty.sprint ~width:80 (printStmt defaultCilPrinter () s)); *)
           match s.skind with
           | Instr i -> process_instr i
         ) b.bstmts)
  and process_instr i =
    List.flatten
      (List.map_tl
         (fun i ii ->
           let pos loc =
             if ii = [] then
               loc.byte, loc.byte+1
             else
               let loc' =
                 match List.hd ii with
                 | Call (_,_,_,loc) -> loc
                 | Set (_,_,loc) -> loc
               in
               loc.byte, loc'.byte
           in
           (* Printf.printf "instr: %s\n%!" (Pretty.sprint ~width:80 (printInstr defaultCilPrinter () i)); *)
           match i with
           | Call (_, Lval (Var v, NoOffset), args, loc) when v.vname = "pthread_mutex_lock" ->
             let m = List.hd args in
             (
               match m with
               | AddrOf (Var v, NoOffset) ->
                 [Semantics.P v.vname,pos loc]
             )
           | Call (_, Lval (Var v, NoOffset), args, loc) when v.vname = "pthread_mutex_unlock" ->
             let m = List.hd args in
             (
               match m with
               | AddrOf (Var v, NoOffset) ->
                 [Semantics.V v.vname,pos loc]
             )
           | Call (_,_,_,loc) | Set (_,_,loc) -> [Semantics.Nop, pos loc]
         ) i)
  in

  (* Parse main in order to build processes. *)
  let rec main_block b =
    List.iter_tl
      (fun s tl ->
        match s.skind with
        | Instr i -> main_instr i
        | Return _ -> assert (tl = [])
        | _ -> failwith "Cannot handle main with control flow for now."
      ) b.bstmts
  and main_instr i =
    List.iter
      (function
      | Call (_, Lval (Var v, NoOffset), args, _) when v.vname = "pthread_mutex_init" -> ()
      | Call (_, Lval (Var v, NoOffset), args, _) when v.vname = "pthread_create" ->
        let f = List.nth 2 args in
        let f =
          (* Printf.printf "%s\n%!" (Pretty.sprint ~width:80 (printExp defaultCilPrinter () f)); *)
          match f with
          | AddrOf (Var v, NoOffset) ->
            let p =
              List.find_map
                (function
                | GFun (f,_) when f.svar.vid = v.vid -> Some f
                | _ -> None
                ) globals
            in
            (* TODO: declare variables in p.slocals *)
            let p = process_block p.sbody in
            processes := p :: !processes
          | _ -> assert false
        in
        ()
      | Call (_, Lval (Var v, NoOffset), args, _) when v.vname = "pthread_barrier_init" ->
        let b = List.nth 0 args in
        let n = List.nth 2 args in
        let b =
          let rec aux = function
            | AddrOf (Var v, NoOffset) -> v
            | CastE (_, b) -> aux b
            | _ -> assert false
          in
          aux b
        in
        let n =
          match n with
          | Const (CInt64 (n,_,_)) -> Int64.to_int n
          | _ -> assert false
        in
        (* TODO: check that it was an uninitialized synchronization barrier *)
        resources := List.map (fun (v,k) -> if v.vid = b.vid then (v,`Barrier (Some n)) else (v,k)) !resources
      | Call (_, Lval (Var v, NoOffset), args, _) -> Printf.printf "Unhandled function %s.\n%!" v.vname
      | Set _ -> ()
      | _ -> assert false
      ) i
  in
  main_block main.sbody;

  (* Generate the "semantics". *)
  let processes = List.rev !processes in
  let processes = List.mapi (fun n p -> Printf.sprintf "p%d" n, p) processes in
  let resources =
    List.map
      (fun (v, kind) ->
        let name = v.vname in
        name, match kind with
        | `Mutex mode -> Semantics.Mutex mode
        | `Barrier None -> failwith (Printf.sprintf "Barrier %s has unknown arity." name)
        | `Barrier (Some n) -> Semantics.Synchronization n
      ) !resources
  in
  let semantics =
    {
      Semantics.
      resources = resources;
      variables = !variables;
      equations = processes;
      initials = List.map fst processes;
      runpro = [||];
      inclusions = [];
    }
  in
  Interpreter.init_runpro semantics;
  semantics

let () = Analyzer.semantics_from_c_file := parse
