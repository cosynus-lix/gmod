module V =
struct

  module Integer =
  struct
    type t = int
    let compare = Pervasives.compare
    let equal = Pervasives.(=)
    let hash n = n
    let string_of = string_of_int
  end

  module String =
  struct
    type t = string
    let compare = Pervasives.compare
    let equal = Pervasives.(=)
    let hash s = String.length s
    let string_of s = s
  end

end

module E =
struct

  module Integer =
  struct
    type t = int
    let default = 0
    let string_of = string_of_int
  end

  module String =
  struct
    include String
    let default = ""
    let string_of s = s
  end

end

module B =
struct

    module Integer =
    struct
      type t = int
      let least_regular_value = 0
      let min = Pervasives.min
      let max = Pervasives.max
      let compare = Pervasives.compare
      let string_of = string_of_int
      let of_string = int_of_string
      let strict_upper_bound ?(gap=1) a b = (max a b)+1
    end(*Integer*)

    module String =
    struct
      type t = string
      let least_regular_value = ""
      let min = Pervasives.min
      let max = Pervasives.max
      let compare = Pervasives.compare
      let string_of s = s
      let of_string s = s
      let strict_upper_bound ?(gap="") s s' = s ^ gap ^ s'
    end(*String*)

    module Float =
    struct
      type t = float
      let least_regular_value = 0.0
      let min = Pervasives.min
      let max = Pervasives.max
      let compare = Pervasives.compare
      let string_of = string_of_float
      let of_string = float_of_string
      let strict_upper_bound ?gap x x' =
	match gap with
	  | Some epsilon -> x+.x'+.epsilon
	  | None -> (max x x')+.(min (abs_float x) (abs_float x'))

    end(*Float*)

end

module ODA =
struct

  module OverInteger =
  struct

    exception WrongCharacter of int

    module Sh = ODA.BooleanAlgebra(B.Integer)

    module HL = ODA.HalfLine(B.Integer)

    module Ci = ODA.Circle(B.Integer)

    let delimiters = Str.regexp "\\[\\|\\]\\|{\\|}\\|,\\| +\\|\\+oo"

    let closed_on_the_right = ref false

    let delimiter_is_a_blank s = s<>"["&&s<>"]"&&s<>"{"&&s<>"}"&&s<>","&&s<>"+oo"

    let delimiter_is_expandable s = s<>"["&&s<>"]"&&s<>"+oo"

    let delimiter_is_a_valid_opening s = s="["||s="]"||s="{"

    let noninteger = Str.regexp "[^0-9]"

    let valid_integer s =
      try
	let n = Str.search_forward noninteger s 0
	in
	failwith (Printf.sprintf "%s is not an integer: character %c is not a digit" s s.[n])
      with
	| Not_found -> ()

    let of_string s =
      let position = ref 0 in
      let update_position s = position := !position + (String.length s) in
      let rec remove_expandable lx = match lx with
	| Str.Delim s as x::lx -> update_position s;
	  if delimiter_is_expandable s
	  then remove_expandable lx
	  else x::remove_expandable lx
	| (Str.Text s' as x)::lx ->
	  (
	    try
	      let n = Str.search_forward noninteger s' 0 + !position
	      in failwith (Message.red ~start:n ~length:1 s)
	    with
	      | Not_found -> (update_position s';x::remove_expandable lx)
	  )
	| [] -> []
      in
      let rec of_string lx = match lx with
	| Str.Delim d1::Str.Text t1::Str.Text t2::Str.Delim d2::lx ->
	  (
	    match d1 with
	      | "[" -> Sh.cls (int_of_string t1)
	      | "]" -> Sh.opn (int_of_string t1)
	      | _ -> failwith "[ or ] expected"
	  )::
	    (
	      match d2 with
		| "[" -> Sh.opn (int_of_string t2)
		| "]" -> Sh.cls (int_of_string t2)
		| _ -> failwith "[ or ] expected"
	    )::(of_string lx)
	| Str.Text t::lx -> Sh.iso (int_of_string t)::(of_string lx)
	| Str.Delim d1::Str.Text t::Str.Delim d2::Str.Delim d3::lx ->
	  closed_on_the_right := d3 = "]";
	  if d2="+oo"&&(d3="]"||d3="[")
	  then
	    (
	      match d1 with
		| "[" -> Sh.cls (int_of_string t)
		| "]" -> Sh.opn (int_of_string t)
		| _ -> failwith "[ or ] expected"
	    )::(of_string lx)
	  else failwith "+oo[ or +oo] expected"
	| [] -> []
	| _ -> failwith "malformed expression"
      in
      Sh.normalize (of_string (remove_expandable (Str.full_split delimiters s)))

  end (*OverInteger*)
end (*O*)

(* Proper names *)

(* Dirty Hack: in order to avoid name masking *)

(*

module V = BuiltInWithoutParsers.V
module E = BuiltInWithoutParsers.E
module B = BuiltInWithoutParsers.B


module D = DGROG.Make(V)(E)(B)
*)

module DGROG = DGROG.Make(V.String)(E.String)(B.Integer)
module CPODGROG = CPODGROG.Make(DGROG)
module Brick = CPODGROG.Brick
module Area = CPODGROG.Area

(*Globals data for parsers and lexers*)

type support = HalfLine | CompactHalfLine | Circle

let string_of_support_option support_option = match support_option with
  | Some HalfLine -> "Some HalfLine"
  | Some Circle  -> "Some Circle"
  | Some CompactHalfLine -> "Some CompactHalfLine"
  | None -> "None"

(*let support:support option ref = ref None*)

(*The_support function check makes a preparsing*)
(*Actually this function should be made obsolete*)

let check_support s =
  let hls = "#R+" in
  let cis = "#S'" in
  let chls = "#KR+" in
  let aux = String.concat "\\|" (List.map Str.quote [hls;cis;chls]) in
  let rx = Str.regexp aux in
  (
    try
      let n = Str.search_forward rx s 0 in
      let ms = Str.matched_string s in
      let start = (n+(String.length ms)) in
      let len = (String.length s)-start in
      let support =
	if ms=hls
	then Some HalfLine
	else if ms=cis
	then Some Circle
	else if ms=chls
	then Some CompactHalfLine
	else None
      in
      let ans = String.sub s start len
      in
      print_endline ans ; support,ans
    with
      | Not_found -> None,s
  )
