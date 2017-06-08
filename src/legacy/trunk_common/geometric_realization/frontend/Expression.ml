include Map.Make(String)

module ODA =
struct
  type operator =
    | Variable of string
    | Interval of bool*bool*int*int
    | Final of bool*int*bool
    | Atom of ((int list)*bool)
    | Coatom of ((int list)*bool)
    | Intersection of operator*operator
    | Union of operator*operator
    | Difference of operator*operator
    | Complement of operator
    | Closure of operator
    | Interior of operator
    | Future of operator*operator
    | Past of operator*operator
    | Compare of operator*operator
    | Less_or_equal of operator*operator
    | Greater_or_equal of operator*operator
    | Strictly_less of operator*operator
    | Strictly_greater of operator*operator
    | Circle
    | HalfLine
    | CompactHalfLine
end(*ODA*)

module DGROG =
struct
  type operator =
    | Constant of BuiltInWithoutParsers.DGROG.t
    | Variable of string
    | Intersection of operator*operator
    | Union of operator*operator
    | Difference of operator*operator
    | Complement of operator
    | Closure of operator
    | Interior of operator
    | Future of operator*operator
    | Past of operator*operator
    | FutureClosure of operator
    | PastClosure of operator
    | Less_or_equal of operator*operator
    | Greater_or_equal of operator*operator
    | Strictly_less of operator*operator
    | Strictly_greater of operator*operator
    | Compare of operator*operator
end(*DGROG*)

module Area =
struct
  type operator =
    | Constant of BuiltInWithoutParsers.Area.t
    | Variable of string
    | Int of int
    | Intersection of operator*operator
    | Union of operator*operator
    | Difference of operator*operator
    | Complement of operator
    | Closure of operator
    | Interior of operator
    | Future of operator*operator
    | Past of operator*operator
    | Less_or_equal of operator*operator
    | Greater_or_equal of operator*operator
    | Strictly_less of operator*operator
    | Strictly_greater of operator*operator
    | Compare of operator*operator
    | Product of operator*operator
    | Factorize of operator
end(*Area*)

