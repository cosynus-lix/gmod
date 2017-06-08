(* The following attributes alter the appearence of the output. *)
(* Depending on the shell, some of them may not be available. *)

(* from /etc/termcap L15419
   (O) SGR parameter values:
   0 = default mode (attributes off),
   1 = bold,
   2 = dim,
   3 = italicized,
   4 = underlined,
   5 = slow blink,
   6 = fast blink,
   7 = reverse video,
   8 = invisible,
   9 = crossed-out (marked for deletion),
   10 = primary font,
   10 + n (n in 1..9) = nth alternative font,
   20 = Fraktur,
   21 = double underline,
   22 = turn off 2,
   23 = turn off 3,
   24 = turn off 4,
   25 = turn off 5,
   26 = proportional spacing,
   27 = turn off 7,
   28 = turn off 8,
   29 = turn off 9,
   30 = black fg,
   31 = red fg,
   32 = green fg,
   33 = yellow fg,
   34 = blue fg,
   35 = magenta fg,
   36 = cyan fg,
   37 = white fg,
   38 = set fg color as in CCIT T.416, (enables 256-colors mode)
   39 = set default fg color,
   40 = black bg,
   41 = red bg,
   42 = green bg,
   43 = yellow bg,
   44 = blue bg,
   45 = magenta bg,
   46 = cyan bg,
   47 = white bg,
   48 = set bg color as in CCIT T.416,
   39 = set default bg color,
   50 = turn off 26,
   51 = framed,
   52 = encircled,
   53 = overlined,
   54 = turn off 51 & 52,
   55 = not overlined,
   56-59 = reserved,
   61-65 = variable highlights for ideograms.
*)

let highlight ?(active=true) ?(start=0) ?length ?(bold=false) ?(crossed_out=false) ?(underline=false) ?color str =
  let max_length = (String.length str)-start in
  let length = match length with
    | Some x -> if x<=max_length then x else max_length
    | None -> max_length
  in
  if active then
    (
      let param = ref
				(
				  match color with
				    | Some color -> [color]
				    | None -> []
				)
      in
      if bold        then param := (1::!param);
      if underline   then param := (4::!param);
      if crossed_out then param := (9::!param);
      let rec aux li = match li with
	| x::[] -> string_of_int x
	| x::s -> (string_of_int x)^";"^(aux s)
	| [] -> ""
      in
      let highlighten_substring = String.sub str start length in
      (String.sub str 0 start)^("\027["^(aux !param)^"m"^highlighten_substring^"\027[m")^(let k = length + start in String.sub str k (String.length str - k))
    )
  else
    str

let black   ?active ?start ?length ?bold ?underline s = highlight ~active:(match active with | None -> !Settings.use_terminal_colors | Some x -> x) ?start ?length ?bold ?underline ~color:30 s
let red     ?active ?start ?length ?bold ?underline s = highlight ~active:(match active with | None -> !Settings.use_terminal_colors | Some x -> x) ?start ?length ?bold ?underline ~color:31 s
let green   ?active ?start ?length ?bold ?underline s = highlight ~active:(match active with | None -> !Settings.use_terminal_colors | Some x -> x) ?start ?length ?bold ?underline ~color:32 s
let yellow  ?active ?start ?length ?bold ?underline s = highlight ~active:(match active with | None -> !Settings.use_terminal_colors | Some x -> x) ?start ?length ?bold ?underline ~color:33 s
let blue    ?active ?start ?length ?bold ?underline s = highlight ~active:(match active with | None -> !Settings.use_terminal_colors | Some x -> x) ?start ?length ?bold ?underline ~color:34 s
let magenta ?active ?start ?length ?bold ?underline s = highlight ~active:(match active with | None -> !Settings.use_terminal_colors | Some x -> x) ?start ?length ?bold ?underline ~color:35 s
let cyan    ?active ?start ?length ?bold ?underline s = highlight ~active:(match active with | None -> !Settings.use_terminal_colors | Some x -> x) ?start ?length ?bold ?underline ~color:36 s
let white   ?active ?start ?length ?bold ?underline s = highlight ~active:(match active with | None -> !Settings.use_terminal_colors | Some x -> x) ?start ?length ?bold ?underline ~color:37 s
let crossed_out ?active ?start ?length ?bold ?underline s = highlight ~active:(match active with | None -> !Settings.use_terminal_colors | Some x -> x) ?start ?length ?bold ?underline ~color:9 s

let orange ?active ?start ?length ?bold ?underline s = Common.Terminal.rgb ~active:(match active with | None -> !Settings.use_terminal_colors | Some x -> x) ?start ?length ?bold ?underline 5 3 0 s

let error = red ~bold:true "Error:"
let warning = orange "Warning:"
let opt_d = (blue ~bold:true "-d")^" / "^(blue ~bold:true "-display")
let opt_cbx = (blue ~bold:true "-cbx")
let internal_info = green "Internal info:"

