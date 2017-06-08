module type S = sig

module Highlight:
sig
  val utf8_enabled: bool ref
  val color_enabled: bool ref
  val boolean: (string -> string) ref
  val integer: (string -> string) ref
  val variable: (string -> string) ref
  val unknown: (string -> string) ref
end

exception PV_error of string

(** A type. *)
type ex_type =
  | T_int (** Integer. *)
  | T_bool (** Boolean. *)

(** An expression. *)
type expression =
  | Va of string (** A variable. *)
  | Unknown      (** Dummy default value. *)
  (* ---------------------------------------------------- *)
  | Ac of  int                     (** Integer (arithmetic constant). *)
  | Ne of  expression              (** Opposite (negative). *)
  | Ab of  expression              (** Absolute value (magnitude). *)
  | Pl of  expression * expression (** Addition. *)
  | Mi of  expression * expression (** Substraction. *)
  | Ti of  expression * expression (** Multiplication. *)
  | Di of  expression * expression (** Division. *)
  | Mo of  expression * expression (** Modulo. *)
  (* ---------------------------------------------------- *)
  | Bc of  bool                    (** Boolean. *)
  | An of  expression * expression (** And. *)
  | Or of  expression * expression (** Or. *)
  | No of  expression              (** Not. *)
  | Eq of  expression * expression (** Equal. *)
  | Lq of  expression * expression (** Less or equal. *)
  | Ls of  expression * expression (** Strictly less. *)
  | Gq of  expression * expression (** Greater or equal. *)
  | Gs of  expression * expression (** Strictly greater. *)
  | Nq of  expression * expression (** Not equal. *)
  | I_EmptyMessageBox of string (** Internal instruction – not available to programer. *)
  (* --------------------------------------------- *)
  | It of  expression * expression (** Interval – for static analysis. *)

val int_of_expression: expression -> int

val bool_of_expression: expression -> bool

val and_expression: expression -> expression -> expression

type aribo_kind = Arithmetic | Boolean | Identifier

type mutex_var = [ `Error_check | `Normal | `Recursive | `Default ]

type channel_var = [ `Fifo | `Lifo ]

type semaphore_var = [ `Default | `Quantitative ]

type access_var = [ `Read | `Write ]

type notify_var = [ `Some | `All ]

type resource_kind =
  | Mutex of mutex_var
  | Semaphore of (semaphore_var * int)
  | Synchronization of int
  | Channel of (channel_var * int)
  | Monitor
    (* Internal use in BuzySection.ml (or PV2CFG.ml) as extended keys for maps of busy sections *)
  | Notify of notify_var
  | Variable of access_var

module Resource:
  sig
    type t = string * resource_kind
    val compare: t -> t -> int
    val id: t -> string
    val kind: t -> resource_kind
  end

module Mod: Map.S with type key = Resource.t

module Sod: Set.S with type elt = Resource.t

(** An instruction. *)
type instr =
| P of Resource.t (** Lock mutex*)
| V of Resource.t (** Unlock mutex*)
| S of Resource.t * string (**send 1st argument: name of the stack/queue 2nd argument: message*)
| R of Resource.t * string (**receive 1st argument: name of the stack/queue 2nd argument: container*)
| M of Resource.t * string (**monitoring*)
| N of Resource.t (**One of the processes waiting for the signal may keep on going – non determinism*)
| A of Resource.t (**All processes blocked by the argument are released*)
| F of string (**fork*)
| W of Resource.t (**wait*)
| C of string (**call*)
| E of string (**?*)
| D of ex_type * string (**Dariable – declaration ? – a variable*)
| T of string * expression (**Assign a value to a variable*)
| ITE of ((expression * instruction list) list) (** Case branching*)
| Nop (*The instruction that does nothing*)

(** An instruction along with beginning and end position. *)
and instruction = instr * pos

(** A position: offset of beginning and end (from the begining of the file). *)
and pos = int * int

type process =
  {
    father: int                         ; (** father's process pid *)
    mutable spawns: int list            ; (** children of the process *)
    mutable ip    : int                 ; (** next instruction to perform *)
    mutable code  : (instruction array)   (** process source code; might be altered by symbolic execution *)
  }

type process_class =
  {
    name: string ;
    resources: Sod.t ;
    calls: string list ;
    body: instruction list
  }

val name_from_process_class: process_class -> string

val body_from_process_class: process_class -> instruction list

val name_n_body_from_process_class: process_class -> string * (instruction list)

val equations_converter: (process_class list) -> (string * (instruction list)) list

val name_to_body: string -> process_class list -> instruction list

type t =
    {
      declarations: Resource.t list ;
      equations: process_class list ; (* Map names to body of instructions *)
      initials: string list ; (* List of the prossesses initially running *)
      mutable runpro: process array ; (* running processes *)
      inclusions: string list ;
    }

val empty : unit -> t

val semvar_of_kind: resource_kind -> semaphore_var

val mutex_kind: t -> string -> resource_kind

val semaphore_kind: t -> string -> resource_kind

val notify_kind: t -> string -> resource_kind

val monitor_kind: t -> string -> resource_kind

val read_kind: t -> string -> resource_kind

val write_kind: t -> string -> resource_kind

val variable_kind: t -> string -> resource_kind

val channel_kind: t -> string -> resource_kind

val synchronization_kind: t -> string -> resource_kind

val mutex_var: t -> string -> mutex_var

val semaphore_var: t -> string -> semaphore_var

val semaphore_arity: t -> string -> int

val synchronization_arity: t -> string -> int

val arity_of_kind: resource_kind -> int option

val variable_occurs_in_expression: string -> expression -> bool

val all_variables_occurring_in_expression: expression -> Common.StringSet.t

val all_mutex: t -> Resource.t list

val all_semaphores: t -> Resource.t list

val all_channels: t -> Resource.t list

val all_synchronizations: t -> Resource.t list

val all_write_variables: t -> Sod.t

val all_read_variables: t -> Sod.t

val all_variables: t -> Sod.t

val all_monitors: t -> Resource.t list

val all_notify: t -> Resource.t list

val all_notify_all: t -> Resource.t list

val all_notify_some: t -> Resource.t list

val runpro: t -> process array

val string_of_expression: expression -> string

val string_of_ex_type: ex_type -> string

val string_of_resource_kind: resource_kind -> string

val string_of_mutex_var: mutex_var -> string

val string_of_semaphore_var: semaphore_var -> string

val string_of_access_var: access_var -> string

val string_of_resource: Resource.t -> string

val string_of_instruction: ?hide_branchings:bool -> instruction -> string

val string_of_instruction_list: ?no_new_line:bool -> instruction list -> string

(*
val print_instruction: instruction -> string
*)

val left_margin: string ref

val is_mutex: resource_kind -> bool

val is_semaphore: resource_kind -> bool

val is_lifo: resource_kind -> bool

val is_fifo: resource_kind -> bool

val is_synchronization: resource_kind -> bool

val is_read: resource_kind -> bool

val is_write: resource_kind -> bool

val is_monitor: resource_kind -> bool

val is_channel: resource_kind -> bool

val is_notify: resource_kind -> bool

val is_notify_some: resource_kind -> bool

val is_notify_all: resource_kind -> bool

val is_variable: resource_kind -> bool

val resources: t -> string

val equations: t -> string list option -> string

val equation_names: (string * 'a) list -> string

val all_declared_processes: t -> string list

val running_processes: t -> string

val variables: t -> string

val initials: t -> string

val colliding_declarations: Resource.t -> Resource.t -> bool
(**A sempahore and a mutex should not share the same identifier. Neither should two resources of the same kind. For example
however a mutex and a queue may share the same identifier. Thus [colliding_declarations d1 d2] returns [true] if the
resources declared by [d1] and [d2] share the same identifier while they should not*)

end (*S*)
