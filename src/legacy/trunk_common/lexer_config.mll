{

  open Parser_config

  let word_appear_in_string w s =
    try
      ignore(Str.search_forward (Str.regexp_string_case_fold w) s 0);
      true
    with
      | Not_found -> false

  let extract_gui_dimensions_from_string s =
    let width = (ignore(Str.search_forward (Str.regexp "[0-9]+") s 0)) ; (int_of_string (Str.matched_string s))
    in
    let height = (ignore(Str.search_forward (Str.regexp "[0-9]+") s (Str.match_end ()))) ; (int_of_string (Str.matched_string s))
    in
      (Globals.Gui_parameters.width := width ; Globals.Gui_parameters.height := height)

  let model_options_selection s =
    Globals.Gui_parameters.compute_forbidden   := word_appear_in_string "forbidden" s   ;
    Globals.Gui_parameters.compute_state       := word_appear_in_string "state" s       ;
    Globals.Gui_parameters.compute_deadlock    := word_appear_in_string "deadlock" s    ;
    Globals.Gui_parameters.compute_hopeful     := word_appear_in_string "hopeful" s     ;
    Globals.Gui_parameters.compute_hazardous   := word_appear_in_string "hazardous" s   ;
    Globals.Gui_parameters.compute_safe        := word_appear_in_string "safe" s        ;
    Globals.Gui_parameters.compute_reachable   := word_appear_in_string "reachable" s   ;
    Globals.Gui_parameters.compute_unreachable := word_appear_in_string "unreachable" s

  let verbosity_selection s =
    Flag.verbosity_during_configuration := word_appear_in_string "configuration" s || word_appear_in_string "config" s;
    Flag.verbosity_during_model_building := word_appear_in_string "modelling" s || word_appear_in_string "model-building" s;
    Flag.verbosity_during_calculation := word_appear_in_string "calculation" s || word_appear_in_string "calcul" s

  let bool_of_string_extended_vocabulary s = match s with
    | "y" | "yes" | "true" -> bool_of_string "true"
    | "n" | "no" | "false" -> bool_of_string "false"
    | _ -> failwith "bool_of_string_extended_vocabulary only recognizes \"y\",\"n\",\"yes\",\"no\",\"true\" and \"false\""

}

let blanc = [' ']

let chiffre = ['0'-'9'] (* Un chiffre est l'un des symboles 0 1 2 3 4 5 6 7 8 9. *)

let entier = (chiffre+)

let minuscule = ['a'-'z']

let majuscule = ['A'-'Z']

(*let accentue = ['à' 'é' 'è' 'ê' 'ë' 'î' 'ï' 'ô' 'ö' 'û' 'ü']*)

let special = ['_' '$']

let alphabetique = minuscule | majuscule (*| accentue*)

let alphanumerique = alphabetique | chiffre

let comment = ("(*")((alphanumerique | [' ' ''' '~' ',' '.' ';' ':' '-' '_' '!' '?' '(' ')' '[' ']' '{' '}' '%' '$' '/' '\t' '\r' '\n'])*)("*)")

let equality = (' '*)('=')(' '*)

let pv = "pv_files_path"equality

let oogl = "oogl_files_path"equality

let tikz = "tikz_files_path"equality

let calcul = "calcul_files_path"equality

let miel = "miel_command_path"equality

let terminal_color = "terminal_color"equality

let boolean = ("true" | "yes" | "y" | "false" | "no" | "n")

let shell_screen_width = ("ssw" | "shell_screen_width")equality

let gui_window_size = ("gws" | "gui_window_size")equality(entier(' '*)('x'|'X')(' '*)entier)

let model_options = ("modopt" | "model_options")equality(("forbidden"|"deadlock"|"state"|"safe"|"hopeful"|"hazardous"|"unreachable"|"reachable")(' '+))*((("forbidden"|"deadlock"|"state"|"safe"|"hopeful"|"hazardous"|"unreachable"|"reachable")(' '*))?)

let verbosity = ("verbosity")equality(("calcul"|"calculation"|"model-building"|"modelling"|"config"|"configuration")(' '+))*((("calcul"|"calculation"|"model-building"|"modelling"|"config"|"configuration")(' '*))?)

let directory_name = ((alphanumerique|['.' '_' '-' ':' ' '])*)

let full_directory_name = (directory_name('/'))

let absolute_path = (full_directory_name+)

let path = ("~" | (("~/"| full_directory_name )(full_directory_name)*))

  rule entry_point = parse
      " "                 {entry_point lexbuf}
    | "\t"                {entry_point lexbuf}
    | "\n"                {entry_point lexbuf}
    | "\r"                {entry_point lexbuf}
    | comment             {entry_point lexbuf}
    | pv                  {PV}
    | oogl                {OOGL}
    | tikz                {TIKZ}
    | calcul              {CALCUL}
    | miel                {MIEL}
    | shell_screen_width  {SCREEN_SHELL_WIDTH}
    | terminal_color      {TERMINAL_COLOR}
    | gui_window_size     {UNIT(extract_gui_dimensions_from_string(Lexing.lexeme lexbuf))}
    | model_options       {UNIT(model_options_selection (Lexing.lexeme lexbuf))}
    | verbosity           {UNIT(verbosity_selection (Lexing.lexeme lexbuf))}
    | boolean             {BOOL (bool_of_string_extended_vocabulary (Lexing.lexeme lexbuf))}
    | entier              {INT (int_of_string (Lexing.lexeme lexbuf))}
    | path                {PATH (Lexing.lexeme lexbuf)}
    | eof                 {END}
