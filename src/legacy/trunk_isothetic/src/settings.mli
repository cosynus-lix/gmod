(** Settings for ALCOOL. *)

(** Full path of the settings file. *)
val filename: string

val pv_dir: string ref

val tikz_dir: string ref

val oogl_dir: string ref

val calculator_dir: string ref

val miel_dir: string ref

val use_terminal_colors: bool ref

val utf8_enabled: bool ref

(** Width of the terminal in characters. *)
val terminal_width: int ref

(*
(** Read the configuration file. *)
val read: unit -> unit
*)

(** Create the configuration file if it does not already exist. *)
val create: unit -> unit

(** Create necessary directories if they do not already exist. *)
val create_dirs: unit -> unit

val save: unit -> unit
