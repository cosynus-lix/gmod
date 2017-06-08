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

let active = true

let red    ?active ?start ?length ?bold ?underline s = highlight ?active ?start ?length ?bold ?underline ~color:31 s
let green  ?active ?start ?length ?bold ?underline s = highlight ?active ?start ?length ?bold ?underline ~color:32 s
let yellow ?active ?start ?length ?bold ?underline s = highlight ?active ?start ?length ?bold ?underline ~color:33 s
let blue   ?active ?start ?length ?bold ?underline s = highlight ?active ?start ?length ?bold ?underline ~color:34 s
let orange ?active ?start ?length ?bold ?dim ?underline s = Common.Terminal.rgb ?active ?start ?length ?bold ?dim ?underline 5 3 0 s
let white  ?active ?start ?length ?bold ?dim ?underline s = Common.Terminal.rgb ?active ?start ?length ?bold ?dim ?underline 5 5 5 s


let error   = red    ~bold:true "Error:"
let warning = orange ~bold:true "Warning:"
let direct_sum = green ~bold:true (Glyph.direct_sum ())
let cartesian_product = green ~bold:true (Glyph.cartesian_product ())
let union = green ~bold:true (Glyph.cup ())
let intersection = green ~bold:true (Glyph.cap ())
let logical_and = green ~bold:true (Glyph.wedge ())
let logical_or = green ~bold:true (Glyph.vee ())

let hl s = Common.Terminal.rgb ~bold:true 2 2 5 s
