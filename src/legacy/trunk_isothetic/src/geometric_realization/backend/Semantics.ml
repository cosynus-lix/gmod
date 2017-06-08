(*Functions with prefix data_ are references. They are devoted to intermediate result storage.
  Functions with prefix set_ actually proceed calculations from existing data.
  Functions with prefix get_ returns the expected data if previously computed, otherwise call the corresponding function set_.*)
module type Parameters = sig
  val program:AbstractSyntax.t
  val reduce_cfg:bool
  val verbosity:int
end (*Parameters*)

module type S = sig
  val control_flow_graph: string -> (PV2CFG.D.Skeleton.t * PV2CFG.D.Skeleton.vertex list)
  val busy_section: (resource:(AbstractSyntax.Resource.t) -> procname:string -> PV2CFG.value_for_busy_section)
  val busy_section_of_mutex: string -> string -> PV2CFG.value_for_busy_section
  val busy_section_of_semaphore: string -> string -> PV2CFG.value_for_busy_section
  val busy_section_of_synchronization: string -> string -> PV2CFG.value_for_busy_section
  val busy_section_of_channel: string -> string -> PV2CFG.value_for_busy_section
  val busy_section_of_monitor: string -> string -> PV2CFG.value_for_busy_section
  val busy_section_of_notify: string -> string -> PV2CFG.value_for_busy_section
  val busy_section_of_write: string -> string -> PV2CFG.value_for_busy_section
  val busy_section_of_read: string -> string -> PV2CFG.value_for_busy_section
  val busy_section_of_variable: string -> string -> PV2CFG.value_for_busy_section
  val empty_area: PV2CFG.C.Area.t
  val forbidden_area_of_the_resource: AbstractSyntax.Resource.t -> PV2CFG.C.Area.t
  val forbidden_area_from_mutex: unit -> PV2CFG.C.Area.t
  val forbidden_area_from_semaphores: unit -> PV2CFG.C.Area.t
  val forbidden_area_from_synchronizations: unit -> PV2CFG.C.Area.t
  val forbidden_area_from_monitors: unit -> PV2CFG.C.Area.t
  val forbidden_area_from_channels: unit -> PV2CFG.C.Area.t
  val read_critical_area: unit -> PV2CFG.C.Area.t
  val write_critical_area: unit -> PV2CFG.C.Area.t
  val critical_area: unit -> PV2CFG.C.Area.t
  val forbidden_area: unit -> PV2CFG.C.Area.t
  val swept_forbidden_area: unit -> PV2CFG.C.Area.t
  val normalized_forbidden_area: unit -> PV2CFG.C.Area.t
  val states_space: unit -> PV2CFG.C.Area.t
  val factorization: unit -> bool array list
  val fast_factorization: unit -> bool array list
  val unsafe_fast_factorization: unit -> bool array list
  val safe_fast_factorization: unit -> bool array list
  val weak_factorization: unit -> bool array list
end (*S*)

module Make(AS:Parameters) =
struct

(* Get and Set *)

module type SetSig = sig
  val name2semantics: unit -> unit
  val forbidden_area_of_the_resource:
    AbstractSyntax.Resource.t -> unit
  val forbidden_area_from_mutex: unit -> unit
  val forbidden_area_from_semaphores: unit -> unit
  val forbidden_area_from_synchronizations: unit -> unit
  val forbidden_area_from_monitors: unit -> unit
  val forbidden_area_from_channels: unit -> unit
  val write_critical_area: unit -> unit
  val read_critical_area: unit -> unit
  val critical_area: unit -> unit
  val forbidden_area: unit -> unit
  val swept_forbidden_area: unit -> unit
  val normalized_forbidden_area: unit -> unit
  val states_space: unit -> unit
  val factorization: unit -> unit
  val fast_factorization: unit -> unit
  val unsafe_fast_factorization: unit -> unit
  val safe_fast_factorization: unit -> unit
  val weak_factorization: unit -> unit
end (*SetSig*)

module type GetSig = sig
  val name2cfg: unit -> (string -> (PV2CFG.D.Skeleton.t * PV2CFG.D.Skeleton.vertex list))
  val name2busy_sections: unit -> (resource:(AbstractSyntax.Resource.t) -> procname:string -> PV2CFG.value_for_busy_section)
  val empty_area: unit -> PV2CFG.C.Area.t
  val forbidden_area_of_the_resource:
    AbstractSyntax.Resource.t -> PV2CFG.C.Area.t
  val forbidden_area_from_mutex: unit -> PV2CFG.C.Area.t
  val forbidden_area_from_semaphores: unit -> PV2CFG.C.Area.t
  val forbidden_area_from_synchronizations: unit -> PV2CFG.C.Area.t
  val forbidden_area_from_channels: unit -> PV2CFG.C.Area.t
  val forbidden_area_from_monitors: unit -> PV2CFG.C.Area.t
  val critical_area: unit -> PV2CFG.C.Area.t
  val write_critical_area: unit -> PV2CFG.C.Area.t
  val read_critical_area: unit -> PV2CFG.C.Area.t
  val forbidden_area: unit -> PV2CFG.C.Area.t
  val swept_forbidden_area: unit -> PV2CFG.C.Area.t
  val normalized_forbidden_area: unit -> PV2CFG.C.Area.t
  val states_space: unit -> PV2CFG.C.Area.t
  val factorization: unit -> bool array list
  val fast_factorization: unit -> bool array list
  val unsafe_fast_factorization: unit -> bool array list
  val safe_fast_factorization: unit -> bool array list
  val weak_factorization: unit -> bool array list
end (*GetSig*)

(*miscellaneous*)

let errors_to_perform: (unit -> unit) list ref = ref []

let add_error e = errors_to_perform := e :: !errors_to_perform

let verbose_output ?(startline=false) ?(endline=false) ?(shift=false) threshold message =
  if AS.verbosity > threshold
  then Printf.printf "%s%s%s%s"
    (if startline then "\n" else "")
    (if shift then String.make (2 * threshold) ' ' else "")
    message
    (if endline then "\n" else "")

let default_value_for_Held cfg =
  let original = fst cfg in
  let open PV2CFG in
  let copy = copy_without_code original in
  Held (D.empty_from_skeleton copy)

let default_value_for_Level cfg =
  let original = fst cfg in
  let open PV2CFG in
  let copy = PV2CFG.copy_without_code original in
  Level [| D.full_from_skeleton copy |]

let default_value_for_section_of_nonsemaphore = default_value_for_Held

let default_value_for_section_of_semaphore sem cfg =
  match AbstractSyntax.semvar_of_kind (snd sem) with
  | `Quantitative -> default_value_for_Level cfg
  | `Default -> default_value_for_Held cfg

module Data =
struct
(*The following fields should be filled by the results of the computations*)
let name2cfg = ref None
let name2busy_sections = ref None
let empty_area = ref None
let forbidden_area_from_resource: (PV2CFG.C.Area.t AbstractSyntax.Mod.t) ref = ref AbstractSyntax.Mod.empty
let forbidden_area_from_mutex = ref None
let forbidden_area_from_semaphores = ref None
let forbidden_area_from_synchronizations = ref None
let forbidden_area_from_channels = ref None
let forbidden_area_from_monitors = ref None
let forbidden_area = ref None
let swept_forbidden_area = ref None
let normalized_forbidden_area = ref None
let states_space = ref None
let read_critical_area = ref None
let write_critical_area = ref None
let critical_area = ref None
let factorization = ref None
let fast_factorization = ref None
let unsafe_fast_factorization = ref None
let safe_fast_factorization = ref None
let weak_factorization = ref None
end (*Data*)

module rec Set:SetSig = struct

let name2semantics () =
  let detail = AS.verbosity > 3 && (AbstractSyntax.all_declared_processes AS.program) <> [] in
  let () = verbose_output ~endline:detail ~shift:true 1 "Computing control flow graphs, busy sections, and testing coherence: " in
  let f (cfgmap,bsmap) name =
    let cfg , busy_sections , _ ,  endpoints =
      verbose_output ~shift:true 3 (Printf.sprintf "treating process %s: " name) ;
      PV2CFG.cfg ~terminal_display:false ~dot_display:false name AS.program in (*extract semantics*)
    let cfg =
      if AS.reduce_cfg (*shall we reduce ?*)
      then PV2CFG.reduce_skeleton cfg ;
      cfg in
    let endpoints = List.filter (*remove fake endpoints*)
      (fun ep -> try PV2CFG.D.Skeleton.succ cfg ep = []
        with Invalid_argument _ -> false) endpoints in
    let () = verbose_output ~endline:true 3 "done" in
    Common.StringMap.(add name (cfg,endpoints ) cfgmap , add name busy_sections bsmap) in
  let em = Common.StringMap.empty in
  let (cfgmap , bsmap) = List.fold_left f (em , em) (AbstractSyntax.all_declared_processes AS.program) in
  Data.name2cfg := Some (fun name -> Common.StringMap.find name cfgmap) ;
  Data.name2busy_sections := Some (
    fun ~resource ~procname ->
      try
        AbstractSyntax.Mod.find resource (
          try Common.StringMap.find procname bsmap
          with Not_found -> (add_error (fun () -> Printf.printf "%s Undeclared process %s" Message.error procname) ; exit 0) )
      with Not_found -> (
        let cfg = Common.StringMap.find procname cfgmap in
        if AbstractSyntax.is_semaphore (snd resource)
        then default_value_for_section_of_semaphore resource cfg
        else default_value_for_section_of_nonsemaphore cfg)  ) ;
  verbose_output ~shift:detail ~endline:true 1 "done"

let forbidden_area_of_the_resource resource =
  let initials_array = Array.of_list (AS.program.AbstractSyntax.initials) in
  let busy_sections ~resource ~procname = (Get.name2busy_sections ()) ~resource ~procname in
  let forbidden_area = ForbiddenExtension.forbidden_area_of_the_resource resource busy_sections initials_array in
  Data.forbidden_area_from_resource :=
    AbstractSyntax.Mod.add resource forbidden_area !Data.forbidden_area_from_resource

(*
  [data] is the reference into which the result is stored
  [resources] is the list of resources the forbidden area of which is requested
*)

let generic resources data =
  let empty_area = Get.empty_area () in
  let f accu resource =
    PV2CFG.C.Area.union accu (Get.forbidden_area_of_the_resource resource) in
  let content = List.fold_left f empty_area resources in
  data := Some content

let forbidden_area_from_mutex () =
  generic (AbstractSyntax.all_mutex AS.program) (Data.forbidden_area_from_mutex)

let forbidden_area_from_semaphores () =
  generic (AbstractSyntax.all_semaphores AS.program) (Data.forbidden_area_from_semaphores)

let forbidden_area_from_synchronizations () =
  generic (AbstractSyntax.all_synchronizations AS.program) (Data.forbidden_area_from_synchronizations)

let forbidden_area_from_monitors () =
  generic (AbstractSyntax.all_monitors AS.program) (Data.forbidden_area_from_monitors)

let forbidden_area_from_channels () =
  generic (AbstractSyntax.all_channels AS.program) (Data.forbidden_area_from_channels)

let read_critical_area () =
  let all_read_access = AbstractSyntax.Sod.elements (AbstractSyntax.all_read_variables AS.program) in
  generic all_read_access (Data.read_critical_area)

let write_critical_area () =
  let all_write_access = AbstractSyntax.Sod.elements (AbstractSyntax.all_write_variables AS.program) in
  generic all_write_access (Data.write_critical_area)

let critical_area () =
  let content = PV2CFG.C.Area.union (Get.read_critical_area ()) (Get.write_critical_area ()) in
  Data.critical_area := Some content

let forbidden_area () =
  let forbidden_areas = List.map (fun f -> f ())
    Get.([ forbidden_area_from_mutex ;
      forbidden_area_from_semaphores ;
      forbidden_area_from_synchronizations ;
      forbidden_area_from_channels ;
      forbidden_area_from_monitors ]) in
  let content = PV2CFG.C.Area.union_list forbidden_areas in
  Data.forbidden_area := Some content

let swept_forbidden_area () =
  let content = PV2CFG.C.Area.clean_sweep (Get.forbidden_area ()) in
  Data.swept_forbidden_area := Some content

let normalized_forbidden_area () =
  let content = PV2CFG.C.Area.(clean_sweep (complement (clean_sweep (Get.states_space ())))) in
  Data.normalized_forbidden_area := Some content

let states_space () =
  let content = PV2CFG.C.Area.(clean_sweep (complement (Get.swept_forbidden_area ()))) in
  Data.states_space := Some content

let factorization () =
  let content = PV2CFG.C.Area.factorize (Get.states_space ()) in
  Data.factorization := Some content

let fast_factorization () =
  let content = PV2CFG.C.Area.(fast_factorize (Get.swept_forbidden_area ())) in
  Data.fast_factorization := Some content

let unsafe_fast_factorization () =
  let content = PV2CFG.C.Area.(fast_factorize (Get.forbidden_area ())) in
  Data.unsafe_fast_factorization := Some content

let safe_fast_factorization () =
  let content = PV2CFG.C.Area.(fast_factorize (Get.normalized_forbidden_area ())) in
  Data.safe_fast_factorization := Some content

let weak_factorization () =
  let content = PV2CFG.C.Area.(weak_factorize (Get.swept_forbidden_area ())) in
  Data.weak_factorization := Some content

end (*Set*)
and Get:GetSig = struct

let rec generic data set () = match !data with
  | Some x -> x
  | None -> set () ; generic data set ()

let name2cfg () = generic Data.name2cfg Set.name2semantics ()

(*The function empty_area should be a constant, however, due to technical
restriction on signatures of recursive modules in OCaml 4.00.1 it has to
have a parameter*)

let empty_area () =
  let open AbstractSyntax in
  let abstract_syntax = AS.program in
  let initials_array = Array.of_list (abstract_syntax.initials) in
  let cfg procname = PV2CFG.D.empty_from_skeleton (fst ((Get.name2cfg ()) procname) ) in
  let cfgs_array = Array.map cfg initials_array in
  let brick = PV2CFG.C.Brick.of_array cfgs_array in
  PV2CFG.C.Area.of_brick brick

let name2busy_sections () = generic Data.name2busy_sections Set.name2semantics ()

let rec forbidden_area_of_the_resource resource =
  try AbstractSyntax.Mod.find resource !Data.forbidden_area_from_resource
  with Not_found -> (
    Set.forbidden_area_of_the_resource resource ;
    forbidden_area_of_the_resource resource)

let forbidden_area_from_mutex () = generic Data.forbidden_area_from_mutex Set.forbidden_area_from_mutex ()

let forbidden_area_from_semaphores () = generic Data.forbidden_area_from_semaphores Set.forbidden_area_from_semaphores ()

let forbidden_area_from_synchronizations () = generic Data.forbidden_area_from_synchronizations Set.forbidden_area_from_synchronizations ()

let forbidden_area_from_monitors () = generic Data.forbidden_area_from_monitors Set.forbidden_area_from_monitors ()

let forbidden_area_from_channels () = generic Data.forbidden_area_from_channels Set.forbidden_area_from_channels ()

let read_critical_area () = generic Data.read_critical_area Set.read_critical_area ()

let write_critical_area () = generic Data.write_critical_area Set.write_critical_area ()

let critical_area () = generic Data.critical_area Set.critical_area ()

let forbidden_area () = generic Data.forbidden_area Set.forbidden_area ()

let swept_forbidden_area () = generic Data.swept_forbidden_area Set.swept_forbidden_area ()

let normalized_forbidden_area () = generic Data.normalized_forbidden_area Set.normalized_forbidden_area ()

let states_space () = generic Data.states_space Set.states_space ()

let factorization () = generic Data.factorization Set.factorization ()

let fast_factorization () = generic Data.fast_factorization Set.fast_factorization ()

let unsafe_fast_factorization () = generic Data.unsafe_fast_factorization Set.unsafe_fast_factorization ()

let safe_fast_factorization () = generic Data.safe_fast_factorization Set.safe_fast_factorization ()

let weak_factorization () = generic Data.weak_factorization Set.weak_factorization ()

end (*Get*)

(*Accessible from the outside*)

let control_flow_graph name = (Get.name2cfg ()) name

let busy_section ~resource ~procname = (Get.name2busy_sections ()) ~resource ~procname

let busy_section_of_resource resource_identifier kind_of procname =
  let resource = resource_identifier , kind_of AS.program resource_identifier in
  busy_section ~resource ~procname

let busy_section_of_mutex ident procname =
  busy_section_of_resource ident AbstractSyntax.mutex_kind procname

let busy_section_of_semaphore ident procname =
  busy_section_of_resource ident AbstractSyntax.semaphore_kind procname

let busy_section_of_synchronization ident procname =
  busy_section_of_resource ident AbstractSyntax.synchronization_kind procname

let busy_section_of_channel ident procname =
  busy_section_of_resource ident AbstractSyntax.channel_kind procname

let busy_section_of_monitor ident procname =
  busy_section_of_resource ident AbstractSyntax.monitor_kind procname

let busy_section_of_read ident procname =
  busy_section_of_resource ident AbstractSyntax.read_kind procname

let busy_section_of_write ident procname =
  busy_section_of_resource ident AbstractSyntax.write_kind procname

let busy_section_of_variable ident procname = failwith "Semantics.busy_section_of_variable NIY"

let busy_section_of_notify ident procname = failwith "Semantics.busy_section_of_notify NIY"

let empty_area = Get.empty_area ()

let forbidden_area_of_the_resource resource = Get.forbidden_area_of_the_resource resource

let forbidden_area_from_mutex = Get.forbidden_area_from_mutex

let forbidden_area_from_semaphores = Get.forbidden_area_from_semaphores

let forbidden_area_from_synchronizations = Get.forbidden_area_from_synchronizations

let forbidden_area_from_monitors = Get.forbidden_area_from_monitors

let forbidden_area_from_channels = Get.forbidden_area_from_channels

let read_critical_area = Get.read_critical_area

let write_critical_area = Get.write_critical_area

let critical_area = Get.critical_area

let forbidden_area = Get.forbidden_area

let swept_forbidden_area = Get.swept_forbidden_area

let normalized_forbidden_area = Get.normalized_forbidden_area

let states_space = Get.states_space

let factorization = Get.factorization

let fast_factorization = Get.fast_factorization

let unsafe_fast_factorization = Get.unsafe_fast_factorization

let safe_fast_factorization = Get.safe_fast_factorization

let weak_factorization = Get.weak_factorization

end (*Make*)
