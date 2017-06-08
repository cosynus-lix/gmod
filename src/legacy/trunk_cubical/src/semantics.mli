module Highlight:
sig
  val utf8_enabled: bool ref
  val color_enabled: bool ref
  val boolean: (string -> string) ref
  val integer: (string -> string) ref
  val variable: (string -> string) ref
(*
  val instruction: (string -> string) ref
*)
  val unknown: (string -> string) ref
end

(** A type. *)
type ex_type =
  | T_int (** Integer. *)
  | T_bool (** Boolean. *)

(** An expression. *)
type ex_aribo =
  | Va of string (** A variable. *)
  | Unknown      (** Dummy default value. *)
  (* ---------------------------------------------------- *)
  | Ac of  int                 (** Integer (arithmetic constant). *)
  | Ne of  ex_aribo            (** Opposite (negative). *)
  | Ab of  ex_aribo            (** Absolute value (magnitude). *)
  | Pl of  ex_aribo * ex_aribo (** Addition. *)
  | Mi of  ex_aribo * ex_aribo (** Substraction. *)
  | Ti of  ex_aribo * ex_aribo (** Multiplication. *)
  | Di of  ex_aribo * ex_aribo (** Division. *)
  (* ---------------------------------------------------- *)
  | Bc of  bool                (** Boolean. *)
  | An of  ex_aribo * ex_aribo (** And. *)
  | Or of  ex_aribo * ex_aribo (** Or. *)
  | No of  ex_aribo            (** Not. *)
  | Eq of  ex_aribo * ex_aribo (** Equal. *)
  | Lq of  ex_aribo * ex_aribo (** Less or equal. *)
  | Ls of  ex_aribo * ex_aribo (** Strictly less. *)
  | Gq of  ex_aribo * ex_aribo (** Greater or equal. *)
  | Gs of  ex_aribo * ex_aribo (** Strictly greater. *)
  | Nq of  ex_aribo * ex_aribo (** Not equal. *)
  | I_EmptyMessageBox of string (** Internal instruction – not available to programer. *)
  (* --------------------------------------------- *)
  | It of  ex_aribo * ex_aribo (** Interval – for static analysis. *)

(** An instruction. *)
type instr =
| P of string (** Lock mutex. *)
| V of string (** Unlock mutex. *)
| R of string * string (*receive 1st argument: name of the stack/queue 2nd argument: message*)
| S of string * string (*send 1st argument: name of the stack/queue 2nd argument: container*)
| M of string * string (*monitoring*)
| N of string (*One of the processes waiting for the signal may keep on going*)
| A of string (*All processes blocked by the argument are released*)
| F of string (*fork*)
| W of string (*wait*)
| C of string (*call*)
| E of string (*?*)
| D of ex_type * string (** Dariable a variable. *)
| T of string * ex_aribo (** Assign a value to a variable. *)
| ITE of ((instruction list * ex_aribo) list) (** Case branching. *)
| NIL (*The instruction that does nothing*)
| Nop

(** An instruction along with beginning and end position. *)
and instruction = instr * pos

(** A position: offset of beginning and end (from the begining of the file). *)
and pos = int * int

type aribo_kind = Arithmetic | Boolean | Identifier

type process =
  {
    father: int                         ; (** father's process pid *)
    mutable spawns: int list            ; (** children of the process *)
    mutable ip    : int                 ; (** next instruction to perform *)
    mutable code  : (instruction array)   (** process source code; might be altered by symbolic execution *)
  }

type mutex_var = [ `Error_check | `Normal | `Recursive | `Switch ]

type sem_var = [ `Switch | `Quantitative ]

type resource_kind =
| Mutex of mutex_var
| Monitor
| Semaphore of (sem_var * int)
| Fifo of int
| Lifo of int
| Synchronization of int
| FifoMutex (* Always supposed to be of `Switch kind *)
| LifoMutex (* Always supposed to be of `Switch kind *)

type t =
    {
      resources: (string * resource_kind) list;
      variables: string list;
      equations: (string * instruction list) list; (* Map names to body of instructions *)
      initials: string list; (* List of the prossesses initially running *)
      mutable runpro: process array; (* running processes *)
      inclusions: string list;
    }

val variable_occurs_in_expression: string -> ex_aribo -> bool

val lifo: t -> (string * int) list

val lifo_mutex: t -> string list

val fifo: t -> (string * int) list

val fifo_mutex: t -> string list

val semaphore: sem_var -> t -> (string * int) list

val semaphore_arity: string -> t -> int

val semaphore_kind: string -> t -> sem_var

val mutex: mutex_var -> t -> string list

val synchronization: t -> (string * int) list

val monitor: t -> string list

val empty : unit -> t

val string_of_ex_aribo: ex_aribo -> string

val string_of_ex_type: ex_type -> string

val string_of_instruction: ?hide_branchings:bool -> instruction -> string

val string_of_instruction_list: ?no_new_line:bool -> instruction list -> string

val print_instruction: instruction -> string

val left_margin: string ref

val is_mutex: resource_kind -> bool

val is_semaphore: resource_kind -> bool

val arity_of_kind: resource_kind -> int option

val is_synchronization: resource_kind -> bool

val resources: t -> string

val equations: t -> string list option -> string

val equation_names: (string * 'a) list -> string

val specify_equations: t -> string

val running_processes: t -> string

val variables: t -> string

val initials: t -> string
