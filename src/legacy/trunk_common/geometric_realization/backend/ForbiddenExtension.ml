(*ForbiddenExtension*)

module D = PV2CFG.D
module B = PV2CFG.C.Brick
module A = PV2CFG.C.Area

(*Provide the array containing the cfgs of all the running processes*)

let pack x = A.of_brick (B.of_array [|x|])

let unpack_Held aobs = Array.map (fun x -> match x with PV2CFG.Held x -> pack x | _ -> failwith "Crash in FrobiddenExtension") aobs

let unpack_Level aobs = Array.map (fun x -> match x with PV2CFG.Level x -> Array.map pack x | _ -> failwith "Crash in FrobiddenExtension") aobs

module ForbiddenAreaByKindOfResource =
struct

  (*The identifier [aobs] stands for "array of busy sections" and should be understood as follows: the entry of index [i] is
  the busy section of some given resource for the running process of PID [i]. The PID of the process being its position in
  an array.*)

  let any_mutex aobs = A.generalized_product 2 (unpack_Held aobs)

  let default_semaphore arity aobs = A.generalized_product arity (unpack_Held aobs)

  let quantitative_semaphore arity aobs = A.locus_higher_than arity (unpack_Level aobs)

  let synchronization arity aobs =
    let aobs = unpack_Held aobs in
    let open A in
    let blocked = generalized_product 1 aobs in
    let freed = generalized_product arity aobs in
    difference blocked freed

  let monitor notify_some_aobs notify_all_aobs monitor_aobs =
    let notify_some_aobs = unpack_Held notify_some_aobs in
    let notify_all_aobs = unpack_Held notify_all_aobs in
    let monitor_aobs = unpack_Held monitor_aobs in
    let open A in
    let blocked = generalized_product 1 monitor_aobs in
    let one_freed = difference (generalized_product 1 notify_some_aobs) (generalized_product 2 monitor_aobs) in
    let all_freed = generalized_product 1 notify_all_aobs in
    let freed = union one_freed all_freed in
    difference blocked freed

  let access_conflict access_i access_j =
    let d = Array.length access_i in
    let f i j k =
      if k = i
      then access_i.(i)
      else (
        if k = j
        then access_j.(j)
        else A.full access_i.(k)) in
    let open A in
    let answer = ref (A.of_brick (A.empty (A.product_array access_i))) in
    let () =
      for i = 0 to pred d do
        for j = 0 to pred d do
          if i <> j
          then (
            let brick = A.product_array (Array.init d (f i j)) in
            answer := A.union brick !answer)
        done
      done in
    !answer

  let read_write_conflict read_aobs write_aobs =
    let read_access = unpack_Held read_aobs in
    let write_access = unpack_Held write_aobs in
    access_conflict read_access write_access

  let write_write_conflict write_aobs =
    let write_access = unpack_Held write_aobs in
    access_conflict write_access write_access

(* Remark: write_write_conflict could also be written as follows:
  let write_write_conflict write_aobs = A.generalized_product 2 (unpack_Held write_aobs) *)

end (*ForbiddenAreaByKindOfResource*)

(*

  resource: Resource.t

  resources: Resource.t list

  initials_array: string array

  empty_area: A.t

  busy_sections: ~resource:Resource.t -> ~procname:string -> PV2CFG.value_for_busy_sections

*)

let forbidden_area_of_the_resource resource busy_sections initials_array =
  let open AbstractSyntax in
  let open ForbiddenAreaByKindOfResource in
  let aobs = Array.(map (fun procname -> busy_sections ~resource ~procname) initials_array) in
  match snd resource with
    | Mutex _ -> any_mutex aobs
    | Semaphore (`Default , arity) -> default_semaphore arity aobs
    | Semaphore (`Quantitative, arity) -> quantitative_semaphore arity aobs
    | Monitor -> (
        let notify_some_aobs = Array.(map (fun procname -> busy_sections ~resource:(fst resource , Notify `Some) ~procname) initials_array ) in
        let notify_all_aobs = Array.(map (fun procname -> busy_sections ~resource:(fst resource , Notify `All) ~procname) initials_array ) in
        monitor notify_some_aobs notify_all_aobs aobs)
    | Synchronization arity -> synchronization arity aobs
    | Variable `Write -> write_write_conflict aobs
    | Variable `Read -> (
        let write_aobs = Array.(map (fun procname -> busy_sections ~resource:(fst resource , Variable `Write)  ~procname) initials_array ) in
        read_write_conflict aobs write_aobs)
    | _ -> failwith "forbidden_area_of_the_resource [ForbiddenExtension]"

let access_forbidden_area varname busy_sections initials_array =
  let open AbstractSyntax in
  let read_aobs =
    let resource = varname , Variable `Read in
    Array.(map (fun procname -> busy_sections ~procname ~resource) initials_array) in
  let write_aobs =
    let resource = varname , Variable `Write in
    Array.(map (fun procname -> busy_sections ~procname ~resource) initials_array) in
  let open ForbiddenAreaByKindOfResource in
  let open A in
  union (read_write_conflict read_aobs write_aobs) (write_write_conflict write_aobs)

let forbidden_area_of_the_resources resources empty_area busy_sections initials_array =
  List.fold_left
    (fun accu resource -> A.union (forbidden_area_of_the_resource resource busy_sections initials_array) accu)
      empty_area
      resources
