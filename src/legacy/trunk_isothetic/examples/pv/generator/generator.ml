let dim = ref 2
let philo = ref 0
let philo_nd = ref 0
let cubes_diag = ref 0
let cubes_tiling = ref 0
let mutex_diag = ref 0
let mutex_tiling = ref 0
let swiss_flag = ref false
let swiss_flag_inf = ref false
let synchro = ref false
let factorizing = ref ""
let nonfactorizing = ref ""

let rec list_init f n =
  if n = 0 then [] else (f (n-1))::(list_init f (n-1))

let list_init f n = List.rev (list_init f n)

let general sem proc_nb proc =
  Printf.printf "%s" sem;
  Printf.printf "\nprocs:\n%!";
  for i = 0 to proc_nb - 1 do
    Printf.printf "p%d = %s\n%!" i (proc i)
  done;
  Printf.printf "init:\n";
  for i = 0 to proc_nb - 1 do
    Printf.printf "p%d " i
  done;
  Printf.printf "\n%!"

let simple sem_dim sem_nb proc_nb proc =
  let sem =
    let ans = ref "" in
    let add s = ans := !ans ^ s in
    if sem_dim = 2 then
      add "#mtx"
    else
      add (Printf.sprintf "#sem %d" sem_dim);
    for i = 0 to sem_nb - 1 do
      add (Printf.sprintf " a%d" i)
    done;
    !ans
  in
  general sem proc_nb proc


let pv_groups factorizing s_param =
  let plus = Str.regexp_string "," in
  let dot = Str.regexp_string "." in
  let s_list = Str.split plus s_param in
  let group_list = List.map (fun s -> List.map int_of_string (Str.split dot s)) s_list in
  let number_of_groups = List.fold_left (fun accu x -> match x with [x] -> succ accu | [x;_] -> x + accu | _ -> failwith "Crash" ) 0 group_list in
  let mutexes = "#mtx " ^ (String.concat " " (Array.(to_list (init number_of_groups (fun i -> Printf.sprintf "a%i" i))))) in
  let semaphore = Printf.sprintf "#sem %i b" (if factorizing then succ number_of_groups else number_of_groups) in
  let procs = Array.(to_list (init number_of_groups (fun i -> Printf.sprintf "p%i = P(a%i).P(b).V(b).V(a%i)" i i i))) in
  let current_group = ref 0 in
  let rec f x = match x with
    | [a;b] ->
      (
        let accu = ref [] in
        let c = !current_group + a in
        for i = !current_group to pred c do
          accu := (Printf.sprintf "%ip%i" b i) :: !accu
        done ;
        current_group := c ;
        List.rev !accu
      )
    | [b] -> f [1;b]
    | _ -> failwith "Crash 2" in
  let init = String.concat " " List.(concat (map f group_list)) in
  print_endline mutexes ;
  print_endline semaphore ;
  print_endline "procs:" ;
  List.iter print_endline procs ;
  print_endline "init:" ;
  print_endline init

let () =
  let args =
    [
      "--dim", Arg.Set_int dim, "d Dimension of the space (used by some generators).";
      "--cube", Arg.Unit (fun () -> cubes_diag := 1), " Generate a cube.";
      "--cubes-diagonal", Arg.Set_int cubes_diag, "n Generate n consecutive cubes in dimension d on the diagonal.";
      "--cubes-tiling", Arg.Set_int cubes_tiling, "n Generate an array of n^d cubes (where d is the dimension).";
      "--philosophers", Arg.Set_int philo, "n Generate n philosophers.";
      "--philosophers-nd", Arg.Set_int philo_nd, "n Generate a non-deadlocking variant of n philosophers.";
      "--mutex-diagonal", Arg.Set_int mutex_diag, "n Generate n consecutive takes of mutexes.";
      "--mutex-tiling", Arg.Set_int mutex_diag, "n Generate an array of mutexes takes.";
      "--synchronization", Arg.Set synchro, " A synchronization between d processes.";
      "--swiss-flag", Arg.Set swiss_flag, " Generate a Swiss flag.";
      "--swiss-flag-infinite", Arg.Set swiss_flag_inf, " Generate a Swiss flag with infinite branches.";
      "--factorizing-groups", Arg.Set_string factorizing," An argument matching the regular expression <int>DOT<int>(COMMA<int>DOT<int>)* should be given.
The expression n_1.s_1,...,n_k.s_k corresponds to a PV program with n_1+...+n_k groups of programs of sizes s_1,...,s_k respectively.
The program factorizes. Yet it suffices to decrease the arity of the semaphore to make it nonfactorizing.";
      "--nonfactorizing-groups", Arg.Set_string nonfactorizing," see --factorizing-groups."
    ]
  in
  let args = Arg.align args in
  Arg.parse args (fun s -> philo := int_of_string s) "No milk today.";
  let dim = !dim in
  let philo = !philo in
  let cubes_diag = !cubes_diag in
  let cubes_tiling = !cubes_tiling in
  if philo > 0 then
    simple 2 philo philo (fun i -> Printf.sprintf "P(a%d).P(a%d).V(a%d).V(a%d)" i ((i+1) mod philo) i ((i+1) mod philo))
  else if !philo_nd > 0 then
    simple 2 !philo_nd !philo_nd
      (fun i ->
        let i, i' = if i <> 0 then i, (i+1) mod !philo_nd else (i+1) mod !philo_nd, i in
        Printf.sprintf "P(a%d).P(a%d).V(a%d).V(a%d)" i i' i i')
  else if cubes_diag > 0 then
    let proc = list_init (fun i -> Printf.sprintf "P(a%d).V(a%d)" i i) cubes_diag in
    let proc = String.concat "." proc in
    simple dim cubes_diag dim (fun _ -> proc)
  else if !mutex_diag > 0 then
    let proc = list_init (fun i -> Printf.sprintf "P(a%d).V(a%d)" i i) !mutex_diag in
    let proc = String.concat "." proc in
    simple 2 !mutex_diag dim (fun _ -> proc)
  else if cubes_tiling > 0 then
    let proc = list_init (fun _ -> Printf.sprintf "P(a0).V(a0)") cubes_tiling in
    let proc = String.concat "." proc in
    simple dim 1 dim (fun _ -> proc)
  else if !mutex_tiling > 0 then
    let proc = list_init (fun _ -> Printf.sprintf "P(a0).V(a0)") !mutex_tiling in
    let proc = String.concat "." proc in
    simple 2 1 dim (fun _ -> proc)
  else if !swiss_flag then
    simple dim 1 dim (fun _ -> "P(a0).V(a0)")
  else if !swiss_flag_inf then
    simple 2 1 dim (fun _ -> "P(a0).V(a0)")
  else if !synchro then
    let sem = Printf.sprintf "#syn %d s" dim in
    general sem dim (fun _ -> "W(s)")
  else if !factorizing <> "" then
    pv_groups true !factorizing
  else if !nonfactorizing <> "" then
    pv_groups false !nonfactorizing
  else
    (
      Printf.eprintf "Please select a PV scheme to generate.\n%!";
      exit 1
    )
