open Common
open Globals
open Type
open AbstractSyntax
open Color

let header = Option_user_manual.header

(*
let parallel_separator =
  String.make !Settings.terminal_width '_' ^ "\n\n"
*)


let center n s =
  String.make (!Settings.terminal_width - String.length s + n / 2) ' ' ^ s


(* Dans la fonction qui suit, string_of_bind doit-être une fonction qui renvoie
   une chaîne à partir d'une valeur dont le type est celui des "binds". *)
let print_mos string_of_bind m =
  let aux k b =
    print_string ((green k)^" ⟼ "^(string_of_bind b)^"\n")
  in
  Mos.iter aux m

let print_mos_bis print_bind m =
  let aux k b =
    print_string ((green k)^" ⟼ ");
    print_bind b;
    print_string "\n"
  in
  Mos.iter aux m

let string_of_mos string_of_bind m =
  let aux k b accu = Printf.sprintf "%s = %s\n%s" k (string_of_bind b) accu in
  Mos.fold aux m ""

let string_of_mos ?(arw=" ⟼ ") string_of_bind m =
  let aux k b accu =
    Printf.sprintf "%s%s%s%s\n" accu k arw (string_of_bind b)
  in
  Mos.fold aux m ""

(* One should use Graphviz instead of the next function. *)

let dot_of_idugsv g =
  let intro = "digraph unix {\nsize=\"6,6\";\nnode [color=lightblue2, style=filled];\n" in
  let outro = "}" in
  let quote_caps s = "\""^s^"\"" in
  let dot_to_picture = ref "" in
  let add_to_dot v1 v2 = ( dot_to_picture := (!dot_to_picture)^(quote_caps v1)^" -> "^(quote_caps v2)^" ;\n" ) in
  Idugsv.iter_edges add_to_dot g;
  intro^(!dot_to_picture)^outro

let greatest_regular_value n a =
  let upper =
    AC.fold
      (fun c accu ->
	try
	  max (I.glb (C.slice n c)) accu
	with
	  | _ -> accu)
      a
      I.least_regular_value
  in
  let lower =
    AC.fold
      (fun c accu ->
	try
	  max (I.lub (C.slice n c)) accu
	with
	  | _ -> accu)
      a
      I.least_regular_value
  in
  max lower upper

let bounding_box_of_area a =
  Array.init
    (AC.dimension a)
    (fun n -> greatest_regular_value n a + 1)

(* The function representation returns a list of pairs of float. The float values are comprised between 0 and 1. *)

let representation ?bb a =
  let bb =
    match bb with
      | Some bb -> bb
      | None    -> bounding_box_of_area a
  in
  let dim = AC.dimension a in
  let extremities_of_cube c =
    let aux = Array.init dim (fun d -> C.slice d c)  in
    Array.mapi
      (fun k i -> (float_of_int (try I.glb i with | _ -> bb.(k)+1)) /. (float_of_int (bb.(k)+1)))
      aux
      ,
    Array.mapi
      (fun k i -> (float_of_int (try I.lub i with | _ -> bb.(k)+1)) /. (float_of_int (bb.(k)+1))) aux
  in
  AC.fold (fun c accu -> (extremities_of_cube c)::accu) a []

let highlight_pv c runpro =
  let ans =
    if C.is_empty c then [||]
    else
      Array.mapi
        (fun i proc ->
	  let itv = C.slice i c in
          fst (snd (proc.code.(max 0 (I.glb itv - 1)))) (* first character highlighted *)
            ,
          snd (snd (proc.code.(try I.lub itv - 1 with I.Undefined -> Array.length proc.code - 1))) (* last character highlighted *)
        )
        runpro
  in
  let ans = Array.to_list ans in
  List.filter (fun (x,y) -> x < y) ans

let total_greatest_regular_value a =
  let dim = AC.dimension a in
  let answer = ref 0 in
  for n = 0 to dim-1 do
    answer := max (greatest_regular_value n a) !answer
  done;
  !answer

let tikz_rectangle color x_inf y_inf x_sup y_sup =
  Printf.sprintf
    "\\fill[%s](%dmm,%dmm) rectangle (%dmm,%dmm);\n" color x_inf y_inf x_sup y_sup

let square_to_tikz c length_unit color max_h_coord max_v_coord =
  if C.dimension c = 2 then
    if c <> C.empty 2 then
      let horizontal = C.slice 0 c in
      let vertical   = C.slice 1 c in
      let x_inf = try I.glb horizontal with _ -> I.least_regular_value in
      let x_sup = try I.lub horizontal with _ -> max_h_coord in
      let y_inf = try I.glb vertical with _ -> I.least_regular_value in
      let y_sup = try I.lub vertical with _ -> max_v_coord in
      tikz_rectangle
        color
        (x_inf * length_unit)
        (y_inf * length_unit)
        (x_sup * length_unit)
        (y_sup * length_unit)
    else
      ""
  else
    failwith "The tikz format cannot display higher dimensional cubes"

let edge_square_to_tikz a e length_unit color max_h_coord max_v_coord =
  if C.dimension e = 2 then
    if e <> C.empty 2 then
      let horizontal = C.slice 0 e in
      let vertical   = C.slice 1 e in
      let x_inf = try I.glb horizontal with _ -> I.least_regular_value in
      let x_sup = try I.lub horizontal with _ -> max_h_coord in
      let y_inf = try I.glb vertical with _ -> I.least_regular_value in
      let y_sup = try I.lub vertical with _ -> max_v_coord in
      let width = length_unit / 7 in
      (
        if
	  AC.exists
	    (fun c ->
	      let meet = C.meet e (C.front 0 c) in
	      C.is_not_empty meet && C.is_included meet c)
	    a
	then
          tikz_rectangle
            color
            (x_inf * length_unit)
            (y_inf * length_unit)
            (x_inf * length_unit + width)
            (y_sup * length_unit)
	else
	  ""
      ) ^ (
	if
	  AC.exists
	    (fun c ->
	      let meet = C.meet e (C.back 0 c) in
	      C.is_not_empty meet && C.is_included meet c)
	    a
        then
          tikz_rectangle
            color
            (x_sup * length_unit - width)
            (y_inf * length_unit)
            (x_sup * length_unit)
            (y_sup * length_unit)
        else
          ""
       ) ^ (
	if
	  AC.exists
	    (fun c ->
	      let meet = C.meet e (C.front 1 c) in
	      C.is_not_empty meet && C.is_included meet c)
	    a
        then
          tikz_rectangle
            color
            (x_inf * length_unit)
            (y_inf * length_unit)
            (x_sup * length_unit)
            (y_inf * length_unit + width)
        else
          ""
       ) ^ (
	if
	  AC.exists
	    (fun c ->
	      let meet = C.meet e (C.back 1 c) in
	      C.is_not_empty meet && C.is_included meet c)
	    a
        then
          tikz_rectangle
            color
            (x_inf * length_unit)
            (y_sup * length_unit - width)
            (x_sup * length_unit)
            (y_sup * length_unit)
        else
          ""
       )
    else
      ""
  else
    failwith "The tikz format cannot display higher dimensional cubes"

(* On peut sans doute afficher toutes les frontières à partir de cette fonction,
   l'idée étant d'utiliser des "choose" là où il y a des "exists" *)
let vertex_square_to_tikz ab v length_unit color max_h_coord max_v_coord =
  let horizontal = C.slice 0 v in
  let vertical   = C.slice 1 v in
  let x = try I.glb horizontal with _ -> I.least_regular_value in
  let y = try I.glb vertical with _ -> I.least_regular_value in
  let width = length_unit / 7 in
  if
    AC.exists (fun e -> try v = C.atom (C.lub e) && e = C.front 0 e with _ -> false) ab &&
      AC.exists (fun e -> try v = C.atom (C.lub e) && e = C.front 1 e with _ -> false) ab
  then
    tikz_rectangle
      color
      (x * length_unit)
      (y * length_unit)
      (x * length_unit + width)
      (y * length_unit + width)
  else
    ""

let boundary_area_to_tikz max_h max_v a color_name chnl =
  let ia = AC.interior a in
  let ba = AC.normalize (AC.difference a ia) in
  let vba = ref (AC.empty ~d:2 ()) in
  let scale = !output_scale in
  AC.iter
    (fun c ->
      AC.iter
	(fun c' -> if c <> c' then vba := AC.add (C.meet c c') !vba)
	ba)
    ba;
  AC.iter
    (fun e -> output_string chnl (edge_square_to_tikz a e scale color_name max_h max_v))
    ba;
  AC.iter
    (fun v -> output_string chnl (vertex_square_to_tikz ba v scale color_name max_h max_v))
    !vba

(* La longueur et la largeur du cadre doivent être donné globalement. *)
let area_to_tikz max_h max_v a color_name chnl =
  let scale = !output_scale in
  AC.iter
    (fun c ->
      print_endline (C.string_of c);
      output_string chnl (square_to_tikz c scale color_name max_h max_v))
    a

let printo ?runpro k ip =
  try
    Printf.printf "Processus %s %s : %s\n"
      (highlight ~bold:true (string_of_int k))
      (cyan ("["^(string_of_int (ip.(k)))^"]"))
      (
	AbstractSyntax.string_of_instruction ~hide_branchings:true
	  (get_some runpro).(k).code.(ip.(k)-1)
      )
  with
    | _ -> ()

let print_path runpro max_coord cdh = ignore (Path_extract.generic ~runpro printo max_coord cdh)

let stringo ?runpro k ip =
  let answer = ref [] in
  let add_line s = answer := s::!answer in
  (
    try
      add_line
	(
	  Printf.sprintf "Processus %s %s : %s\n"
	    (highlight ~bold:true (string_of_int k))
	    (cyan ("["^(string_of_int (ip.(k)))^"]"))
	    (
	      string_of_instruction ~hide_branchings:true
		(get_some runpro).(k).code.(ip.(k)-1)
	    )
	)
    with
      | _ -> ()
  );
  String.concat "" (List.rev !answer)

let string_of_path runpro max_coord cdh = String.concat "" (Path_extract.generic ~runpro stringo max_coord cdh)

let atlas2D ?(msg_lss=false) ?(scale=100) akgs semantics =
  let file_name_basis = !Settings.tikz_dir ^ !Globals.name_of_file_to_analyze in
  Printf.printf "Short filename basis : %s\n" !Globals.name_of_file_to_analyze;
  Printf.printf "Complete filename basis : %s\n" file_name_basis;
  let fts = file_name_basis^".tikz" in (* file to contain answer in tikz format *)
  let tmptex = file_name_basis^".tex" in (* temporary tex file *)
  let ftv = file_name_basis^".pdf" in (* file to visualize *)
  let log_trash = file_name_basis^".log" in
  let aux_trash = file_name_basis^".aux" in
  let undetermined_color = "red!50!blue!70" in
  let deadlock_color = "red!75" in
  let safe_color = "blue!30" in
  let unreachable_color = "black!60" in
  let msg_lss_color = "yellow!75" in
  let boundary_undetermined_color = "red!50!blue!70!black!85" in
  let boundary_deadlock_color = "red!75!black!90" in
  let boundary_safe_color =	"blue!30!black!50" in
  let boundary_unreachable_color = "black!75" in
  let boundary_msg_lss_color = "yellow!75!black!85" in
  let cfts (* output channel *) =
    try open_out_bin fts
    with Sys_error fts -> raise (Arg.Bad ((red ~bold:true "Unable")^" to open the given tikz file (fts)")) in
  let ctmptex (* output channel *) =
    try open_out_bin tmptex
    with Sys_error tmptex -> raise (Arg.Bad ((red ~bold:true "Unable")^" to open the given tikz file (tmptex)")) in
  let states_space = Geometric_model.state ~akgs semantics in
  let overflow = Geometric_model.overflow ~akgs semantics in
  let reachable = Geometric_model.reachable ~akgs semantics in
  let unreachable = Geometric_model.unreachable ~akgs semantics in
  let deadlock_attractor = Geometric_model.deadlock_attractor ~akgs semantics in
  let might_go_deadlocks = Geometric_model.hazardous ~akgs semantics in
  let infinity_attractor = Geometric_model.safe ~akgs semantics in
  let f accu a = max accu ((greatest_regular_value 0 a)+1) in
  let max_h = List.fold_left f 0
      [infinity_attractor; might_go_deadlocks; deadlock_attractor; unreachable; reachable; overflow; states_space] in
  let f accu a = max accu ((greatest_regular_value 1 a)+1) in
  let max_v = List.fold_left f 0
      [infinity_attractor; might_go_deadlocks; deadlock_attractor; unreachable; reachable; overflow; states_space] in
  let aux la lic lbc =
    output_scale := scale / max (total_greatest_regular_value states_space) (total_greatest_regular_value overflow);
    List.iter2
      (fun ar co -> area_to_tikz max_h max_v ar co cfts)
      la lic;
    List.iter2
      (fun ar co -> boundary_area_to_tikz max_h max_v ar co cfts)
      la lbc;
    List.iter2
      (fun ar co -> area_to_tikz max_h max_v ar co ctmptex)
      la lic;
    List.iter2
      (fun ar co -> boundary_area_to_tikz max_h max_v ar co ctmptex)
      la lbc
  in
  output_string cfts "\\begin{tikzpicture}\n";
  output_string ctmptex
    (
      "\\documentclass{article}\n"
      ^"\\usepackage{tikz,pgf}\n"
      ^"\\begin{document}\n"
      ^"\\thispagestyle{empty}\n"
      ^"\\begin{tikzpicture}\n"
    );
  aux
    [might_go_deadlocks; deadlock_attractor; infinity_attractor; unreachable]
    [undetermined_color; deadlock_color; safe_color; unreachable_color]
    [boundary_undetermined_color; boundary_deadlock_color; boundary_safe_color; boundary_unreachable_color]
  ; (* Mettre ici un groupe de transparence  *)
  if msg_lss then
    (
      output_string cfts "\\begin{scope}[opacity = 0.4,transparency group]";
      output_string ctmptex "\\begin{scope}[opacity = 0.4,transparency group]";
      aux
	[overflow]
	[msg_lss_color]
	[boundary_msg_lss_color];
      output_string cfts "\\end{scope}";
      output_string ctmptex "\\end{scope}"
    );
  output_string cfts "\\end{tikzpicture}\n";
  output_string ctmptex "\\end{tikzpicture}\n\\end{document}\n";
  close_out cfts;
  close_out ctmptex;
  let directory = !Settings.tikz_dir in
  ignore (Sys.command ("cd "^directory^" ; pdflatex "^(Filename.quote tmptex)^" ; cd .."));
  ignore (Sys.command ("xpdf -z width \""^ftv^"\""));
  ignore (Sys.command ("rm "^(Filename.quote log_trash)));
  ignore (Sys.command ("rm "^(Filename.quote aux_trash)));
  ignore (Sys.command ("rm "^(Filename.quote fts)))

(** Generate an oogl file and launch geomview. *)
(* TODO : corriger le bug au niveau de la gestion des noms de fichiers *)

let atlas3D_geomview to_be_displayed_by_geomview akgs =
  if to_be_displayed_by_geomview = [] then
    Printf.printf "%s Geomview has nothing to display, you should provide an option like %s, %s or %s.\n"
      (red ~bold:true "Error:")
      (blue ~bold:true "-d \"3D frb\"")
      (blue ~bold:true "-d \"3D dla\"")
      (blue ~bold:true "-d \"3D frb dla\"")
  else
    try
      let forbidden = get_some akgs.Geometric_model.forbidden in
      Printf.printf "Access path the Oogl directory = %s\n" !Settings.oogl_dir;
      let file_name_basis = !Settings.oogl_dir ^ !Globals.name_of_file_to_analyze in
      print_endline ("file_name_basis = "^file_name_basis);
      print_endline ("name_of_file_to_analyze = "^(!Globals.name_of_file_to_analyze));
      let fts (* file to contain answer in geomview format *) = file_name_basis ^ ".oogl" in
      let cfts (* output channel *) =
	let channel = open_out_bin fts in
	Printf.printf "File containing oogl: %s\n" fts ;
	channel
      in
      output_string cfts "LIST \n";
      let cglb = I.least_regular_value in
	(*let forbidden = match akgs.Geometric_model.forbidden with
	  | None -> (AC.empty ~d:0 ()) (* should raise an exception  *)
	  | Some a -> a
	  in*)
      let club n = (greatest_regular_value n (AC.boundary forbidden(*!Type.forbidden*))) + 1 in
      let make_quad j cub =
	let soigl b i =
          string_of_int
	    (
	      if b then
		(
		  try
		    I.lub (C.slice i cub)
		  with
		    | _ -> club i
		)
	      else
		(
                  try
		    I.glb (C.slice i cub)
                  with
		    | _-> cglb
		)
	    ) ^ " "
	in
	let colors j =
          (string_of_float (mod_float j 2.))^" "
          ^(string_of_float (floor (j/.(2.))))^" "
          ^(string_of_float (floor (1.-. j/.(2.))))^" "
	and transparency t = string_of_float t in
        if C.is_not_empty cub then
	  (
	    output_string cfts
	      (
                "{= CQUAD  "^
		  (soigl false 0)^(soigl false 1)^(soigl false 2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl true  0)^(soigl false 1)^(soigl false 2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl true  0)^(soigl true  1)^(soigl false 2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl false 0)^(soigl true  1)^(soigl false 2)^(colors j)^(transparency 0.8)^"\n}\n"^
		  "{= CQUAD  "^
		  (soigl false 0)^(soigl false 1)^(soigl false 2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl true  0)^(soigl false 1)^(soigl false 2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl true  0)^(soigl false 1)^(soigl true  2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl false 0)^(soigl false 1)^(soigl true  2)^(colors j)^(transparency 0.8)^"\n}\n"^
		  "{= CQUAD  "^
		  (soigl false 0)^(soigl false 1)^(soigl false 2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl false 0)^(soigl false 1)^(soigl true  2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl false 0)^(soigl true  1)^(soigl true  2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl false 0)^(soigl true  1)^(soigl false 2)^(colors j)^(transparency 0.8)^"\n}\n"^
		  "{= CQUAD  "^
		  (soigl true  0)^(soigl false 1)^(soigl false 2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl true  0)^(soigl false 1)^(soigl true  2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl true  0)^(soigl true  1)^(soigl true  2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl true  0)^(soigl true  1)^(soigl false 2)^(colors j)^(transparency 0.8)^"\n}\n"^
		  "{= CQUAD  "^
		  (soigl false 0)^(soigl true  1)^(soigl false 2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl true  0)^(soigl true  1)^(soigl false 2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl true  0)^(soigl true  1)^(soigl true  2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl false 0)^(soigl true  1)^(soigl true  2)^(colors j)^(transparency 0.8)^"\n}\n"^
		  "{= CQUAD  "^
		  (soigl false 0)^(soigl false 1)^(soigl true  2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl true  0)^(soigl false 1)^(soigl true  2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl true  0)^(soigl true  1)^(soigl true  2)^(colors j)^(transparency 0.8)^"\n\t"^
		  (soigl false 0)^(soigl true  1)^(soigl true  2)^(colors j)^(transparency 0.8)^"\n}\n")
	  )
        else
	  output_string cfts ""
      in
      let deadlock_attractor = match akgs.Geometric_model.deadlock_attractor with
	| None -> (AC.empty ~d:0 ()) (* should raise an exception  *)
	| Some a -> a
      in
      let display (*trigger*) area color =
        if AC.dimension area = 3 then
	  (print_endline "3D display using Geomview" ; AC.iter (make_quad color) area)
        else
	  print_string ("The model is not 3-dimensional.\n")
      in
      if (List.mem "frb" to_be_displayed_by_geomview) then display forbidden 0.;
      if (List.mem "dla" to_be_displayed_by_geomview) then display deadlock_attractor 1. ;
      close_out cfts;
      ignore (Sys.command ("geomview " ^ Filename.quote fts))
    with
      | Not_found ->
        Printf.printf
          "%s You should provide a file containing a PV program.\n"
	  (red ~bold:true "Error:")
      | Sys_error fts ->
        Printf.printf
          "%s Unable to open the given geomview file %s.\n"
          (red ~bold:true "Error:")
          fts

module Terminal_output = struct

  let print_area a =
    Type.AC.string_of a ^ "\n\n"

  let card_area s a =
    let n = Type.AC.cardinal a in
    center 0
      (Printf.sprintf
         "%s is made of %i maximal %s."
         s
         n
         (if n > 1 then "cubes" else "cube")
      ) ^ "\n\n"

  let card_covering s co =
    let n = Type.Co.cardinal co in
    center 0
      (Printf.sprintf
         "%s contains %i %s."
         s
         n
         (if n > 1 then "components" else "component")
      ) ^ "\n\n"


  let card_set_of_points sing plur a =
    let n = Type.AC.cardinal a in
    center 0
      (Printf.sprintf
         "There %s %i %s."
         (if n > 1 then "are" else "is")
         n
         (if n > 1 then plur else sing)
      ) ^ "\n\n"

  let string_of_seq s = String.concat " " (List.map (fun i -> Printf.sprintf "%3i" i) s)

  let string_of_min_brick brick =
    String.concat " "
      (List.map (fun interval -> Printf.sprintf "%3i" (I.glb interval)) (C.to_list brick))

  (* 100 is a dummy value to substitute to +oo *)

  let string_of_max_brick brick =
    String.concat " "
      (List.map (fun interval -> try Printf.sprintf "%3i" (I.lub interval) with _ -> "  #") (C.to_list brick))

  let string_of_simplex ?(show_bricks_bounds = false) ?(show_bricks = false) (seq,brick) =
    if show_bricks_bounds
    then Printf.sprintf "%s %s %s" (string_of_seq seq) (string_of_min_brick brick) (string_of_max_brick brick)
    else
      if show_bricks
      then "NIY"
      else Printf.sprintf "%s" (string_of_seq seq)

  let string_of_nsimplices ?(show_bricks_bounds = false) ?(show_bricks = false) nsimplices = String.concat "\n"
    (List.rev_map (fun simplex -> string_of_simplex ~show_bricks_bounds ~show_bricks simplex) nsimplices)

  let string_of_cech_complex ?(show_bricks_bounds = false) ?(show_bricks = false) cc =
    Array.fold_left (fun accu nsimplices -> Printf.sprintf "%s\n%s" accu
      (string_of_nsimplices ~show_bricks_bounds ~show_bricks nsimplices)) "" cc

  (*Output format for Éric experiments. Not readable.*)

  let string_of_cech_complex_header cc =
    (Printf.sprintf "%3i %3i\n"
      (Array.length cc)
      (try AC.cech_complex_dimension cc with _ -> invalid_arg "string_of_cech_complex"))
    ^ (String.concat " " (Array.to_list (Array.map (fun x -> Printf.sprintf "%3i" (List.length x)) cc)))

  let string_of_cech_complex_raw cc = (string_of_cech_complex cc) ^ "\n"

  let string_of_cech_complex_for_jplex cc = (string_of_cech_complex_header cc) ^ (string_of_cech_complex ~show_bricks_bounds:true cc) ^ "\n"

  let forbidden akgs semantics = print_area (Geometric_model.forbidden ~akgs semantics)

  let card_forbidden akgs semantics = card_area "The forbidden area" (Geometric_model.forbidden ~akgs semantics)

  let local_forbidden akgs semantics = print_area (Geometric_model.forbidden ~local:true ~akgs semantics)

  let card_local_forbidden akgs semantics = card_area "The local forbidden area" (Geometric_model.forbidden ~local:true ~akgs semantics)

  let state akgs semantics = print_area (Geometric_model.state ~akgs semantics)

  let card_state akgs semantics = card_area "The state space" (Geometric_model.state ~akgs semantics)

  let deadlock_attractor akgs semantics = print_area (Geometric_model.deadlock_attractor ~akgs semantics)

  let card_deadlock_attractor akgs semantics = card_area "The deadlock attractor" (Geometric_model.deadlock_attractor ~akgs semantics)

  let deadlock_attractor_weak akgs semantics = print_area (Geometric_model.deadlock_attractor_weak ~akgs semantics)

  let card_deadlock_attractor_weak akgs semantics = card_area "The underapproximation of the deadlock attractor" (Geometric_model.deadlock_attractor_weak ~akgs semantics)

  let local_deadlock_attractor akgs semantics = print_area (Geometric_model.local_deadlock_attractor ~akgs semantics)

  let card_local_deadlock_attractor akgs semantics = card_area "The local deadlock attractor" (Geometric_model.local_deadlock_attractor ~akgs semantics)

  let local_deadlock_with_parameters n akgs semantics =
    print_area (Geometric_model.local_deadlock_with_parameter ~akgs n semantics)

  let card_local_deadlock_with_parameters akgs n semantics =
    card_area "The local deadlock attractor" (Geometric_model.local_deadlock_with_parameter ~akgs n semantics)

  let deadlocks akgs semantics = print_area (Geometric_model.deadlocks ~akgs semantics)

  let card_deadlocks akgs semantics = card_set_of_points "deadlock" "deadlocks" (Geometric_model.deadlocks ~akgs semantics)

  let sources akgs semantics = print_area (Geometric_model.sources ~akgs semantics)

  let card_sources akgs semantics = card_set_of_points "source" "sources" (Geometric_model.sources ~akgs semantics)

  let safe akgs semantics = print_area (Geometric_model.safe ~akgs semantics)

  let card_safe akgs semantics = card_area "The safe area" (Geometric_model.safe ~akgs semantics)

  let reachable akgs semantics = print_area (Geometric_model.reachable ~akgs semantics)

  let card_reachable akgs semantics = card_area "The reachable area" (Geometric_model.reachable ~akgs semantics)

  let unreachable akgs semantics = print_area (Geometric_model.unreachable ~akgs semantics)

  let card_unreachable akgs semantics = card_area "The reachable area" (Geometric_model.unreachable ~akgs semantics)

  let hazardous akgs semantics = print_area (Geometric_model.hazardous ~akgs semantics)

  let card_hazardous akgs semantics = card_area "The hazardous area" (Geometric_model.hazardous ~akgs semantics)

  let message_loss akgs semantics = print_area (Geometric_model.overflow ~akgs semantics)

  let card_message_loss akgs semantics = card_area "The overflow area" (Geometric_model.overflow ~akgs semantics)

  let reachable_message_loss akgs semantics = print_area (Geometric_model.reachable_overflow ~akgs semantics)

  let card_reachable_message_loss akgs semantics = card_area "The reachable overflow area" (Geometric_model.reachable_overflow ~akgs semantics)

  let cech_complex akgs semantics = string_of_cech_complex_raw (AC.cech_complex (Geometric_model.cech_complex ~akgs semantics))

  let cech_complex_for_jplex akgs semantics = string_of_cech_complex_for_jplex (AC.cech_complex (Geometric_model.cech_complex ~akgs semantics))

  (*let cset_state semantics = CS.print (Geometric_model.cset_state semantics)*)

  let cset_state akgs semantics =
    CS.to_string (Geometric_model.cset_state ~akgs semantics)

  let print_title title = match title with
    | Some title -> print_string ("\n"^(header title (get_some !name_of_the_input_file)))
    | None -> ()


  let critical_section akgs semantics =
    let answer = ref "" in
    let add_line s = answer := !answer ^ s in
    let conflicts = Geometric_model.critical_section ~akgs semantics in
    (
      List.iter (fun s -> add_line s) conflicts;
      match List.length conflicts with
      | 0 -> add_line ((green "No conflict\n")^".")
      | 1 -> add_line ((red "\nOne conflict\n")^".")
      | n -> add_line ((red ("\n"^(string_of_int n)^" conflicts"))^".\n")
    );
    !answer


  let yoneda akgs semantics =
    let forbidden_area = Geometric_model.forbidden ~akgs semantics in
    akgs.Geometric_model.forbidden <- Some forbidden_area;
    String.concat ""
      (Type.Co.string_list_of
	 (Co.remove
            (AC.empty ~d:(AC.dimension forbidden_area) ())
            (Co.yoneda forbidden_area)))

  let card_yoneda akgs semantics =
    let forbidden_area = Geometric_model.forbidden ~akgs semantics in
    akgs.Geometric_model.forbidden <- Some forbidden_area ;
    card_covering "The Yoneda partition"
      (Co.remove (AC.empty ~d:(AC.dimension forbidden_area) ()) (Co.yoneda forbidden_area))

  let factorize akgs semantics =
    let state = Geometric_model.state ~akgs semantics in
    let factors = Type.AC.factorize state in
    let number_of_factors = List.length factors in
    let prelude =
      if number_of_factors = 0
      then "There is no group...it is surprising! In fact there is a bug!\n"
      else
	if number_of_factors = 1 then "There is 1 group\n"
	else Printf.sprintf "There are %i independent groups:\n" number_of_factors
    in
    let human_readable ba = Algebra.BooleanArray.string_of_support ba
    in
    prelude^(String.concat " " (List.rev_map human_readable factors))^"\n"

  let dining_philosophers arity =
    let answer = ref "" in
    let add_line s = answer := !answer ^ s in
    let go_ahead = ref true in
    if arity > 8 then
      (
        Printf.printf "The calculations for %i philosophers may take a while, do you want to continue (yes/no) ? " arity;
        if read_line () <> "yes" then
          (
            print_endline "Aborted";
            go_ahead := false
          )
      );
    if !go_ahead then
      (
	let aux = Algebra.Monoid.Philosopher.orbits arity in
	let aux2 = List.sort Pervasives.compare (map_n_sweep (fun orbit -> List.length (List.hd orbit)) aux) in
	List.iter
	  (fun size ->
	    add_line (Printf.sprintf "\nOrbits whose classes contain %i traces\n\n" size);
	    List.iter
	      (fun orbit ->
                if List.length (List.hd orbit) = size then
		  List.iter
		    (fun eqcl -> add_line ((Algebra.Monoid.Philosopher.string_of_class eqcl)^"\n"))
		    orbit
	      ) aux
	  )
	  aux2;
	add_line (Printf.sprintf "\nThere are %i orbits\n" (List.length aux));
      );
    !answer

  let extract_maximal_traces ?runpro akgs semantics =
    let runpro =
      match runpro with
	| Some runpro -> runpro
	| None -> Interpreter.init_runpro semantics
    in
    let answer = ref "" in
    let add_line s = answer := !answer ^ s in
    let state_space = Geometric_model.state ~runpro ~akgs semantics in
    let d = AC.dimension state_space in
    let max_coord = Array.init d (* a priori il faut donc déjà que runpro ait été initialisé... *)
      (
	fun k -> Array.length runpro.(k).code + 1
      )
    in
    Co.iter
      (fun a ->
	add_line(string_of_path runpro max_coord a) ;
	add_line("\n"^(String.make !Settings.terminal_width '_')^"\n\n")
      )
      (
        let maxdip = Geometric_model.maximal_dipaths ~akgs semantics in
	akgs.Geometric_model.maximal_dipaths <- Some maxdip ; maxdip
      );
    !answer


  let trace ?runpro ?(matrix=false) akgs semantics =
    let runpro =
      match runpro with
	| Some runpro -> runpro
	| None -> Interpreter.init_runpro semantics
    in
    let answer = ref "" in
    let add_line s = answer := !answer ^ s in
    (* let state_space = Geometric_model.state ~akgs semantics in *)
    (* let d = AC.dimension state_space in *)
    let d = Array.length runpro in
    let max_coord =
      Array.init d (fun k -> Array.length runpro.(k).code + 1)
    in
    (* if not matrix then *)
      (* add_line ("\n" ^ center 0 "(Martin Raussen's algorithm implemented by Alex Lang)" ^ "\n\n"); *)
    Co.iter
      (fun a ->
	add_line (string_of_path runpro max_coord a);
	add_line ("\n"^(String.make !Settings.terminal_width '_')^"\n\n"))
      (if matrix then
          Analyzer.trace ~components:false ~euler:false ~covering:false semantics
       else
          Analyzer.trace semantics);
    !answer


  let maximal_dihomotopy_classes akgs semantics =
    let maxdip = Geometric_model.maximal_dipaths ~akgs semantics in
    let _ = akgs.Geometric_model.maximal_dipaths <- Some maxdip in
    Co.string_of maxdip


  let card_maximal_dihomotopy_classes akgs semantics =
    let maxdip = Geometric_model.maximal_dipaths ~akgs semantics in
    let _ = akgs.Geometric_model.maximal_dipaths <- Some maxdip in
    let n = Co.cardinal maxdip in
    center 0
      (Printf.sprintf
         "    There %s %i dihomotopy classes of maximal traces.\n\n"
         (if n > 1 then "are" else "is")
         n)

   let cbx () =
    try
      string_of_mos ~arw:" = " AC.string_of (Calculator.cube (get_some !name_of_the_input_file))
    with
      | Not_found -> failwith (Printf.sprintf "%s the option %s needs to be given a file.\n" error opt_cbx)

    let ctx prog = string_of_mos AT.string_of (Calculator.tore prog)

    let clx prog = string_of_mos AX.string_of (Calculator.cyle prog)

end (* Terminal_output *)
