let expand_the_option (keys,spec,doc) =
	let f k = (k,spec,"") in
	List.map f keys

let expand_all_options list_of_options = 
	List.concat (List.map expand_the_option list_of_options)

let sum_int l_int = List.fold_left (+) 0 l_int

let column_size ?(align_opt=false) speclist =
  if align_opt 
  then
    let rec f accu keys =
      match accu,keys with
        | a::accu,k::keys -> max a (String.length k) :: f accu keys
        | []     ,k::keys ->         String.length k :: f accu keys
        | a::accu,[]      ->                       a :: f accu keys
        | _               -> [] in 
    List.fold_left (fun accu (keys,_,_) -> f accu keys) [] speclist
  else
    let f accu (keys,_,_) =
      let l_keys = List.map String.length keys in      
      if sum_int accu < sum_int l_keys then l_keys else accu in
    List.fold_left f [] speclist

let prologue ?(align_opt=false) ?(align_doc=false) highlight_opt separator pattern keys =
  let align =
    if align_opt 
    then 
      let rec align pattern keys =
        match pattern , keys with
          | p::pattern , k::keys ->
            (Printf.sprintf "%s%s" k (String.make (p - (String.length k)) ' ')) :: align pattern keys
          | _ -> [] in
      align
    else (fun _ x -> x) in
  String.concat separator (List.map highlight_opt (align pattern keys))

let make
  ?(align_doc = true)
  ?(align_opt = false)
  ?usage_msg
  ?(indent_opt = 2) 
  ?(help_doc = " Display this list of options")
  ?(separator = "/")
  ?(highlight_opt = (fun x -> x))
  speclist
=
  let list_of_options = ref speclist in
  let pattern = column_size ~align_opt speclist in
  let prologue_column = (List.fold_left (+) 0 pattern) + indent_opt - 1 in
  let rec help_message () =
    let () = match usage_msg with | Some m -> print_endline m | None -> () in
    let f (keys,_,doc) =
      let prologue = prologue ~align_opt highlight_opt separator pattern keys in
      let raw_prologue = Common.Terminal.untag prologue in
      let prologue_length = Common.Terminal.length raw_prologue in
      let complement =
        let x = prologue_column - prologue_length in
        if align_doc then String.make x ' ' else "" in
      let prologue = prologue ^ complement in
      if doc <> ""
      then Printf.printf "%s%s%s\n" (String.make indent_opt ' ') prologue doc in
    List.iter f !list_of_options in
  let () = list_of_options := List.append !list_of_options [["-help";"--help"],Arg.Unit help_message,help_doc] in
  expand_all_options !list_of_options
