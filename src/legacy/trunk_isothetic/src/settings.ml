open Common

(* TODO: remove Filename.expand_tilde *)

let base_dir = Sys.getenv "HOME" ^ "/.alcool/"
let filename = base_dir ^ "alcool.conf"
let pv_dir = ref (base_dir ^ "pv/")
let oogl_dir = ref (base_dir ^ "oogl/")
let tikz_dir = ref (base_dir ^ "tikz/")
let calculator_dir = ref (base_dir ^ "calculs/")
let miel_dir = ref (base_dir ^ "miel/")
let terminal_width = ref 80
let use_terminal_colors = ref true
let utf8_enabled = ref true
let gui_window_size    = ref (300,175)
let verbosity          = ref "modelling calcul"
let model_options      = ref "forbidden state deadlock"

(** Generate contents of a configuration file with current settings. *)
let dump () =
  String.concat "\n"
  [
    "(*-------------------- Options file for Alcool --------------------*)";
    "(*      This file should be located at ~/.alcool/.alcool.rc        *)";
    "(*-----------------------------------------------------------------*)";
    "";
    Printf.sprintf "pv_files_path      = %s" !pv_dir;
    Printf.sprintf "oogl_files_path    = %s" !oogl_dir;
    Printf.sprintf "tikz_files_path    = %s" !tikz_dir;
    Printf.sprintf "calcul_files_path  = %s" !calculator_dir;
    Printf.sprintf "miel_command_path  = %s" !miel_dir;
    Printf.sprintf "shell_screen_width = %i" !terminal_width;
    Printf.sprintf "verbosity          = %s" !verbosity;
    Printf.sprintf "terminal_color     = %b" !use_terminal_colors;
    Printf.sprintf "gui_window_size    = %ix%i" (fst !gui_window_size) (snd !gui_window_size);
    Printf.sprintf "model_options      = %s" !model_options;
    "";
    "(*------------------------- End of file ---------------------------*)"
  ]

let verbose = false

(* If the file f does not exist, then nothing is done, otherwise the file is
   renamed with the guarantee that no existing file is overwritten. *)
let shift_file_or_directory_to_old f =
  let purified_f = Filename.expand_tilde f in
    if Sys.file_exists purified_f
    then
      begin
	let counter = ref 0 in
	let filename = ref (purified_f^".old."^(string_of_int !counter)) in
	  begin
	    while Sys.file_exists !filename do
              incr counter;
	      filename := (f^".old."^(string_of_int !counter))
	    done;
	    ignore(Sys.command (Printf.sprintf "mv %s %s" purified_f !filename));
	    if verbose then
	      Printf.printf ("The existing %s has been moved to %s\n") purified_f !filename;
	    !filename
	  end
      end
    else
      ""

(** Create a directory if it does not already exist. *)
let create_directory dir =
  let dir =
    let x = ref ((String.length dir)-1)
    in
    while dir.[!x] = '/' do decr x done
    ;
    String.sub dir 0 (!x+1)
  in (* Remove the useless occurences of the character '/' that might appear at the end of dir. *)
  let shift_n_create dir =
    let file_moved = shift_file_or_directory_to_old dir in
    (Printf.printf
       "There should be a directory %s used by Alcool.
However a file %s already exists.
Alcool will move this file to %s and create the directory %s.\n"
       dir
       dir
       file_moved
       dir
    );
    Unix.mkdir dir 0o755;
    if verbose then
      Printf.printf "The directory %s has been created.\n" dir
  in
  if (try Sys.is_directory dir with Sys_error _ -> false) then
    (
      if verbose then
	Printf.printf "The directory %s already exists.\n" dir
    )
  else
    shift_n_create dir

(** Create the configuration file. *)
let create ?(overwrite=false) () =
  create_directory base_dir;
  let create_file () =
    let fd = Unix.openfile filename [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_RDWR] 0o644 in
    if (not overwrite) && verbose then Printf.printf "The file %s does not exist.\n" filename;
    let contents = dump () in
    assert (Unix.write fd contents 0 (String.length contents) = String.length contents);
    Unix.close fd;
    if (not overwrite) && verbose then Printf.printf "The file %s has been created.\n" filename
  in
  try
    if overwrite then
      create_file ()
    else
      if Sys.is_directory filename then
	(
	  Printf.printf "There should be a file %s used by Alcool.
However a directory %s already exists.
Alcool will move this directory to %s so it can work properly.\n"
	  filename
	  filename
	  (shift_file_or_directory_to_old filename);
	  (*	    print_endline ("There should be a file "^filename^" used by Alcool.\nHowever a directory "^filename^" already exists."^"\nAlcool will move this directory to "^(shift_file_or_directory_to_old filename)^" so it can work properly.");*)
	  create_file ()
	)
      else
	if verbose then
	  Printf.printf "The file %s already exists.\n" filename
  with
    | Sys_error _ ->
      create_file ()

let save () = create ~overwrite:true ()

let create () = create ()

let create_dirs () =
  create_directory !pv_dir;
  create_directory !oogl_dir;
  create_directory !tikz_dir;
  create_directory !calculator_dir
