module Integer =
struct
  type t = int
  let least_regular_value = 0
  let compare = Pervasives.compare
  let min = Pervasives.min
  let max = Pervasives.max
  let string_of = string_of_int
  let of_string = int_of_string
  let shift = succ
end

module Float =
struct
  type t = float
  let least_regular_value = 0.
  let compare = Pervasives.compare
  let min = Pervasives.min
  let max = Pervasives.max
  let string_of = string_of_float
  let of_string = float_of_string
  let shift x = x +. 0.001
end

module Make(X:
  sig 
    type t
    val least_regular_value: t
    val compare: t -> t -> int
    val string_of: t -> string
    val of_string: string -> t
  end) =
struct

  include X
 
  let min x y = 
    if compare x y <= 0 then x else y

  let max x y = 
    if compare x y <= 0 then y else x

end (*Make*)

module Lexico =

struct

  module Array(HL:Sig.Bound) = Make(
    struct

      type t = HL.t array

      let least_regular_value = [||]

      let compare x y = 
        let lx = Array.length x in
        let ly = Array.length y in
        let b = Pervasives.min lx ly in
        let k = ref 0 in
        let delta = ref 0 in
        while !k < b && !delta = 0 do
          delta := HL.compare x.(!k) y.(!k);
          incr k
        done;
        if !delta <> 0
        then !delta
        else lx - ly

      let string_of t = "<"^(Array.fold_left (fun accu x -> ((HL.string_of x)^" "^accu)) "" t)^">"

      let of_string s = failwith "HalfLine.Lexico.Array.of_string NIY"

    end) (*Array*)


  module List(HL:Sig.Bound) = Make(
    struct
  
      type t = HL.t list
  
      let least_regular_value = []
  
      let rec compare x y = match x with
        | a::x' -> (
            match y with 
              | b::y' -> let delta = HL.compare a b in
                if delta <> 0 then delta else compare x' y'
              | [] -> 1)
        | [] -> if y <> [] then -1 else 0
  
      let string_of t = "<"^(List.fold_left (fun accu x -> ((HL.string_of x)^" "^accu)) "" t)^">"
  
      let of_string s = failwith "HalfLine.Lexico.List.of_string NIY"
  
    end) (*List*)


end (*Lexico*)


module Graded =

struct

  module Array(HL:Sig.Bound) = Make(
    struct
  
      type t = HL.t array
  
      let least_regular_value = [||]
  
      let compare x y =
        let lx = Array.length x in
        let delta = lx - Array.length y in
        if delta <> 0
        then delta
        else (
          let k = ref 0 in
          let delta = ref 0 in
          while !k < lx && !delta = 0 do
            delta := HL.compare x.(!k) y.(!k);
            incr k
          done;
          !delta)
  
      let string_of t = "<"^(Array.fold_left (fun accu x -> ((HL.string_of x)^" "^accu)) "" t)^">"
       
      let of_string s = failwith "HalfLine.Graded.Array.of_string NIY"
  
    end) (*Array*)

  module List(HL:Sig.Bound) = Make(
  struct

    type t = HL.t list

    let least_regular_value = []

    let compare x y =
      let grad = (List.length x) - (List.length y) in
      if grad <> 0
      then grad
      else
        let rec aux x y =
          match x with 
            | a::x' -> (
                match y with 
              | b::y' -> let delta = HL.compare a b in if delta <> 0 then delta else (aux x' y')
              | []    -> failwith "Graded.List.compare : ce cas ne devrait pas survenir.")
            | []    -> 0 in
        aux x y

    let string_of t = "<"^(List.fold_left (fun accu x -> ((HL.string_of x)^" "^accu)) "" t)^">"
     
    (* La fontction ci-dessous doit être écrite *)

    let of_string s = failwith "HalfLine.Graded.List.of_string NIY"

  end) (*List*)

end (*Graded*)

module Pair(HL:Sig.Bound) = Make(
struct
  type t = HL.t * HL.t
  let least_regular_value = HL.least_regular_value,HL.least_regular_value

  let compare (x1,y1) (x2,y2) = 
    let dx = HL.compare x1 x2 in
    if dx <> 0
    then dx
    else HL.compare y1 y2


  let string_of (x,y) = Printf.sprintf "(%s,%s)" (HL.string_of x) (HL.string_of y)

  let of_string s = failwith "Half_line.Pair.of_string is not implemented yet" 
      
end) (*Pair*)
