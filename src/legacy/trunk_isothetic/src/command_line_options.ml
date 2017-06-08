(*--------------------------------------------------------------------------------------------------*)
(*                                                                                                  *)
(*                                                                                                  *)
(*                                     COMMAND_LINE_OPTIONS                                         *)
(*                                                                                                  *)
(*                                                                                                  *)
(*--------------------------------------------------------------------------------------------------*)

open Common
open Globals
open Color
open Display.Terminal_output

let mode s =
  Flag.mode_options := (Str.split (Str.regexp "[' ']+") s);
  let rec mode s = match s with
    | "depth"::s ->
      let argument_provided = ref true in
      let depth = try int_of_string (List.hd s) with _ -> argument_provided := false; 0 in
      if !argument_provided then
        Flag.depth := depth
      else
        print_string "depth requires an integer argument\n";
      mode s
    | "random-branching"::s ->
      Flag.branching_mode := "random-branching";
      mode s
    | "semaphore-seeker-branching"::s ->
      Flag.branching_mode := "semaphore-seeker-branching";
      mode s
    | "explore"::s ->
      let argument_provided = ref true in
      let argument = try int_of_string (List.hd s) with _ -> argument_provided := false; 1 in
      if !argument_provided then
        (
          Flag.branching_mode := "random-branching";
          Flag.explore := argument
        )
      else
        print_string "explore requires an integer argument\n";
      mode s
    | _ -> ()
  in
  mode !Flag.mode_options

let bound =
  [
    Arg.String
      (
	fun x -> match x with
	  | "g"
	  | "gat"
	  | "gatling" -> Interpreter.schedule_algorithm := "gatling"
	  | _ -> failwith "Not an available schedule mode"
      );
    Arg.Int(fun x -> Interpreter.max_execution_steps := x)
  ]

let shortcuts = ["-tc-off";"-tc-on";"-tsw";"-fvrb";"-vrb";"--help";"-help";"-contact"]

let speclist display_options =
  let display s = display_options := !display_options ^ "," ^ s in
  let rec help () =
    print_endline (Option_user_manual.paragraph "");
    List.iter (* pour éviter les doublons *)
      (fun (a,_,c) ->
	if (c <> "") && (List.for_all (fun s -> a <> s) shortcuts) then
          print_endline ("  "^(blue ~bold:true a)^" "^c))
      args
  and args =
    [
      "-terminal-color-on", Arg.Set Settings.use_terminal_colors, Option_user_manual.paragraph "terminal color on";
      "-tc-on", Arg.Set Settings.use_terminal_colors, "";
      "-terminal-color-off", Arg.Clear Settings.use_terminal_colors, Option_user_manual.paragraph "terminal color off";
      "-tc-off", Arg.Clear Settings.use_terminal_colors, "";
      "-utf8-on", Arg.Set Settings.utf8_enabled, Option_user_manual.paragraph "utf8 on";
      "-utf8-off", Arg.Clear Settings.utf8_enabled, Option_user_manual.paragraph "utf8 off";
      "-display", Arg.String display, Option_user_manual.paragraph "display";
      "-d", Arg.String display, "";
      "-mode", Arg.String mode,Option_user_manual.paragraph "mode";
      "-unroll", Arg.Tuple bound, Option_user_manual.paragraph "parabounding";
      "-verbose", Arg.Set Flag.verbose, Option_user_manual.paragraph "verbose";
      "-v", Arg.Set Flag.verbose, "";
      "-very-verbose", Arg.Set Flag.full_verbose, Option_user_manual.paragraph "fullverbose";
      "-vv", Arg.Set Flag.full_verbose, "";
      "-terminal-screen-width", Arg.Int(fun x -> Settings.terminal_width := x), Option_user_manual.paragraph "terminal screen width";
      "-tsw",Arg.Set_int Settings.terminal_width, "" ;
      "-people", Arg.Unit (fun () -> print_string (Option_user_manual.people ())), Option_user_manual.paragraph "people";
      "-email", Arg.Unit (fun () -> print_string (Option_user_manual.email ())), Option_user_manual.paragraph "econtact";
      "-contact", Arg.Unit (fun () -> print_string (Option_user_manual.email ())), "";
      "-adress", Arg.Unit (fun () -> print_string (Option_user_manual.address ())), Option_user_manual.paragraph "adress";
      "-credits", Arg.Unit (fun () -> print_string (Option_user_manual.credits ())), Option_user_manual.paragraph "credits";
      "-pvsyntax", Arg.Unit (fun () -> print_string (Option_user_manual.Syntaxes.pv2 ())), Option_user_manual.paragraph "pv syntax";
      "-pv", Arg.Unit (fun () -> print_string (Option_user_manual.Syntaxes.pv2 ())), "";
      "-pv-old", Arg.Unit (fun () -> print_string (Option_user_manual.Syntaxes.pv ())), "";
      "-help", Arg.Unit help, "display this list of options" ;
      "--help", Arg.Unit help, "display this list of options"
    ]
  in
  let args =
    if !Globals.have_gtk then
      args@["-gui",Arg.Set Flag.gui, ((blue ~bold:true "Graphical User Interface")^": interactive mode with graphical interface")]
    else
      args
  in
  args

let anon_fun s =
  if !name_of_the_input_file <> None then
    Printf.printf
      "%s Don't know what to do with %s: argument discarded.\n"
      (yellow ~bold:true "Warning:")
      (blue ~bold:true s)
  else
    (
      (* TODO: why two variables? *)
      name_of_the_input_file := Some s;
      name_of_file_to_analyze := Filename.chop_extension (Filename.basename s)
    )

(* TODO: remove parser/lexer_display_request *)
let parse_display d =
  let commands = Str.split (Str.regexp "[,]+") d in
  let commands = List.map (fun s -> Str.split (Str.regexp "[ ]+") s) commands in
  let gm = Type.Geometric_model.default () in
  let sem = ref None in
  let sem () =
    match !sem with
      | Some sem -> sem
      | None ->
        let fname =
          match !name_of_the_input_file with
            | Some f -> f
            | None ->
              raise (Failure (Printf.sprintf "%s a file to analyze should be specified.\n" (red ~bold:true "Error:")))
        in
        let s = Analyzer.semantics_from_file fname in
        sem := Some s; s
  in
  let header title ?subtitle f () =
    let title = Printf.sprintf " %s " title in
    let subtitle = match subtitle with
      | None ->
        (
          match !name_of_the_input_file with
            | None -> ""
            | Some s -> s
        )
      | Some s -> s in
    print_string ("\n" ^ Display.header title subtitle);
    print_string (f gm (sem ()))
  in
  let geomview = ref [] in
  let add_geomview s = geomview := s :: !geomview in
  let handle = function
    | ["forbidden"]
    | ["frb"] ->
      fun () ->
        add_geomview "frb";
        header "Forbidden area" forbidden ()
    | ["local-forbidden"] ->
      header "Local forbidden area" local_forbidden
    | ["state"]
    | ["st"] ->
      header "State space" state
    | ["Cech-complex"]
    | ["cech-complex"]
    | ["Čech-complex"]
    | ["Cech"]
    | ["cech"] -> failwith "option cech / Cech / Čech-complex / cech-complex / Cech-complex not allowed"
      (* header "Čech complex" cech_complex *)
    | ["Cech-complex-for-jplex"]
    | ["cech-complex-for-jplex"]
    | ["Čech-complex-for-jplex"]
    | ["Cech-for-jplex"]
    | ["cech-for-jplex"] -> failwith "option cech-for-jplex / Cech-for-jplex / Čech-complex-for-jplex / cech-complex-for-jplex / Cech-complex-for-jplex not allowed"
      (* header "Čech complex JPlex format" cech_complex_for_jplex *)
    | ["deadlock-attractor"]
    | ["dla"] ->
      fun () ->
        add_geomview "dla";
        header "Deadlock attractor" deadlock_attractor ()
    | ["local-deadlock-attractor"; n]
    | ["ldla"; n]
        when String.is_int n ->
      let n = int_of_string n in
      header (Printf.sprintf "Local (%d fold) deadlock attractor" n) (local_deadlock_with_parameters n)
    | ["local-deadlock-attractor"]
    | ["ldla"] ->
      header "Local deadlock attractor" local_deadlock_attractor
    | ["weak-deadlock-attrator"]
    | ["wdla"]
    | ["concur98"] -> failwith "option concur98 / wdla / weak-deadlock-attractor not allowed"
      (* header "Underapproximation of the deadlock attractor" deadlock_attractor_weak *)
    | ["deadlocks"]
    | ["dl"] ->
      header "Deadlocks" deadlocks
    | ["sources"]
    | ["src"] ->
      header "Sources" sources
    | ["safe"] ->
      header "Safe area" safe
    | ["reachable"] ->
      header "Reachable" reachable
    | ["unreachable"] ->
      header "Unreachable" unreachable
    | ["hazardous"] ->
      header "Hazardous" hazardous
    | ["message-loss"] ->
      header "Stack/queue overflow" message_loss
    | ["reachable-message-loss"] ->
      header "Reachable stack/queue overflow" reachable_message_loss
    | ["resources"] ->
      header "Resources" (fun _ -> AbstractSyntax.resources)
    | ["extract-maximal-traces"]
    | ["emt"] -> failwith "option extract-maximal-traces / emt not allowed"
      (* header "Representatives of execution traces up to dihomotopy" extract_maximal_traces *)
    | ["maximal-dihomotopy-classes"]
    | ["mdc"] -> failwith "option maximal-dihomotopy-classes / mdc not allowed"
      (* header "Dihomotopy classes of maximal dipaths" maximal_dihomotopy_classes *)
    | ["equations"]
    | ["eq"] ->
      header "Process definitions" (fun _ sem -> AbstractSyntax.equations sem None)
    | ["equation-names"]
    | ["en"] ->
      header "Process names" (fun _ sem -> AbstractSyntax.equation_names sem.AbstractSyntax.equations)
    | ["the-equation"]
    | ["te"] ->
      header "Some process definitions" (fun _ -> AbstractSyntax.specify_equations)
    | ["critical-section"]
    | ["csec"] ->
      header "Critical sections" critical_section
    | ["yoneda"]
    | ["Yoneda"]
    | ["yon"] -> failwith "option yoneda / Yoneda / yon not allowed"
      (* header "Yoneda components" yoneda *)
    | ["running-processes"]
    | ["runpro"]
    | ["rp"] ->
      header "Running processes" (fun _ -> AbstractSyntax.running_processes)
    | ["factorize"]
    | ["fac"] ->
      header "Factorization" factorize
    | ["trace"] -> failwith "option trace not allowed"
      (* header "Representatives of execution traces up to dihomotopy" trace *)
    | ["trace-matrix"] -> failwith "option trace-matrix not allowed"
      (* header "Matrix representation of the the trace space" (trace ~matrix:true) *)
    | ["2D"] | ["2d"] -> failwith "option 2D / 2d not allowed"
      (* fun () -> Display.atlas2D gm (sem ()) *)
    | ["3D"] | ["3d"] -> failwith "option 3D / 3d not allowed"
      (* fun () -> Display.atlas3D_geomview !geomview gm *)
    | ["cubical-set"]
    | ["cset"] -> failwith "option cset / cubical-set not allowed"
      (* header "Cubical set from state space" cset_state *)
    (* TODO: add cardinal *)
    | l ->
      raise
        (Failure
           (Printf.sprintf
              "Could not understand display option \"%s\""
              (String.concat " " l)))
  in
  List.map handle commands

let parse () =
  let display_options = ref "" in
  Arg.parse (speclist display_options) anon_fun "usage_msg";
  parse_display !display_options

(* Whether colors should be displayed on the terminal output or not. *)

let switch_color () =
  Array.iter
    (fun s ->
      if s = "-terminal-color-on" || s = "-tc-on" then
	Settings.use_terminal_colors := true
      else if s = "-terminal-color-off" || s = "-tc-off" then
	Settings.use_terminal_colors := false
    ) Sys.argv;
  Settings.save ()
