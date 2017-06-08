(*
Mail.ml – part of the Alcool project
LGPL license should be inserted here
*)

open Common
open Globals
open Color

let read_configuration_file () =
  let file_name = Settings.filename in
  let ic = open_in_bin file_name in
  Parser_config.config Lexer_config.entry_point (Lexing.from_channel ic);
  close_in ic

let () =
  Settings.create ();
  Settings.create_dirs ();
  read_configuration_file ();
  try
    Command_line_options.switch_color ();
    if Array.length Sys.argv = 1
    then (
      print_endline "";
      Option_user_manual.welcome ();
      print_endline "";
      Option_user_manual.help ());
    let actions = Command_line_options.parse () in
    if !Flag.gui
    then
      !display_with_gtk_gui ()
    else (
      Glyph.init !Settings.utf8_enabled;
      List.iter (fun f -> f ()) actions)
  with Failure s -> print_endline s ; exit 1
