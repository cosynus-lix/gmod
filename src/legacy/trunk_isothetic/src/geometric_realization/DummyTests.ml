(*Some dummy tests*)

module S = BuiltIn.ODA.OverInteger.Sh
module H = BuiltIn.ODA.OverInteger.HL
module C = BuiltIn.ODA.OverInteger.Ci
module O = BuiltIn.ODA.OverInteger
module D = BuiltIn.DGROG.OverInteger
module A = BuiltIn.Area.OverInteger
module B = BuiltIn.Brick.OverInteger
module CPO = BuiltInWithoutParsers.CPODGROG
module Interval = Interval.Make(Half_line.Integer)
module Cubemaker = Cube.Make(Common)
module Cube = Cubemaker(Interval)
(*
module Areamaker = Area.Make(Common)
*)
module Area = AreaOverCube.Make(Common)(Cube)

let a_of_string s =
	let s = Printf.sprintf "\"%s\"" s in
	A.of_string s

let () = if false then Test.Circle.full_testing_string_of ()

let () = if false then Test.Circle.full_testing_to_area ()

let () = if false then
	let flag = ref false in
	let departure = S.empty in
	let arrival = S.full in
	let result = C.future_extension ~flag departure arrival in
	Printf.printf "future_extension %s %s = %s\nand flag = %b\n" (C.string_of departure) (C.string_of arrival) (C.string_of result) !flag
	
let () = if false then
	let flag = ref false in
	let departure = S.make [S.opn 2;S.opn 4] in
	let arrival = S.make [S.cls 4] in
	let result = C.future_extension ~flag departure arrival in
	Printf.printf "future_extension %s >> %s = %s\nand flag = %b\n" (C.string_of departure) (C.string_of arrival) (C.string_of result) !flag
	
let () = if false then
	let departure = S.make [S.iso 3] in
	let arrival = S.make [S.cls 0] in
	let result = H.past_extension departure arrival in
	Printf.printf "past_extension %s << %s = %s\n" (H.string_of departure) (H.string_of arrival) (S.string_of result)
	
let () = if false then
	let flag = ref false in
	let departure = S.make [S.iso 2] in
	let arrival = S.make [S.cls 0;S.cls 1;S.opn 2] in
	let result = C.future_extension ~flag departure arrival in
	Printf.printf "future_extension %s >> %s = %s\nand flag = %b\n" (C.string_of departure) (C.string_of arrival) (C.string_of result) !flag
	
let () = if false then
	let ar = O.HL.of_string "¬([3,10] ⋂ [7,11])" in
	print_endline (D.string_of (D.make ["a";"b";"c"] ["a","alpha",ar,"x"]))
	
let () = if false then
	let locus level aaa = A.levelwise_product ~level aaa in
	let _0 = A.of_string "\"a [0,1[ b ⨉ a [0,1[ b\"" in
	let _1 = A.of_string "\"a [1,2[ b ⨉ a [1,2[ b\"" in
	let _2 = A.of_string "\"a [2,3[ b ⨉ a [2,3[ b\"" in
	let _3 = A.of_string "\"a [3,4[ b ⨉ a [3,4[ b\"" in
	let f = [|_0;_1;_2;_3|] in
	let aaa = [|f;f;f|] in
	for i = 0 to 13 do
		Printf.printf "level %i\n" i;
		let a = (locus i aaa) in
		print_endline (A.string_of a)
	done

let () = if false then
	let locus level aaa = A.levelwise_product ~level aaa in
	let _1 = a_of_string "a {1} a" in
	let _2 = a_of_string "a {2} a" in
	let _3 = a_of_string "a {3} a" in
	let _4 = a_of_string "a {4} a" in
	let f = [|_1;_2;_3;_4|] in
	let aaa = [|f;f;f|] in
	for i = 0 to 13 do
		Printf.printf "level %i\n" i;
		let a = (locus i aaa) in
		print_endline (A.string_of ~hollow:false a)
	done

let () = if false then
	let x0 = a_of_string "a b ⨉ a b" in
	let x1 = a_of_string "a [3,4[ b ⨉ a [3,4[ b" in
	let () = print_endline (A.string_of x0) in
	let () = print_endline (A.string_of x1) in
	if A.is_included x0 x1 then print_endline "x0 ⊆ x1" else print_endline "¬ x0 ⊆ x1"
	
let	() = if false then
	let edge0 = ("a","ζ",[],"b",false) in
	let edge4 = ("a","ζ",H.of_string "[3,4[","b",false) in
	let graph0 = D.draw_edges_list [edge0] in
	let graph4 = D.draw_edges_list [edge4] in
	let brick1 = B.of_list [graph4;graph4] in
	let brick0 = CPO.Cp [|graph0;graph0|] in
	let () = Printf.printf "brick0 =\n%s \n" (B.string_of brick0) in
	Printf.printf "brick1 =\n%s \n" (B.string_of brick1)
