(** Optimized data structures involving booleans. *)

open Common

(** Vectors of booleans. *)
module Vector :
sig
  (** A boolean vector. *)
  type t

  (** Create a vector of given length. *)
  val create : int -> t
  val empty : int -> t
  val init : int -> (int -> bool) -> t

  (** [singleton n i] creates a vector of size [n] with [i] as only element. *)
  val singleton : int -> int -> t
  val full : int -> t
  val compare : t -> t -> int
  val cardinal : t -> int
  val is_empty : t -> bool
  val add : int -> t -> t
  val union : t -> t -> t
  val mem : int -> t -> bool
  val diff : t -> t -> t
  val included : t -> t -> bool
  val inter : t -> t -> t
  val remove : int -> t -> t
  val of_list : int -> int list -> t
  val enum : t -> int Enum.t
  val iter : (int -> unit) -> t -> unit
  val filter : (int -> bool) -> t -> t
  val for_all : (int -> bool) -> t -> bool
  val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a
  val to_string : t -> string
end

(** Matrices of booleans. *)
module Matrix :
sig
  (** A boolean matrix. *)
  type t

  val compare : t -> t -> int

  (** Create an empty boolean matrix of given dimensions. *)
  val create : int -> int -> t

  (** Create a boolean matrix full of ones. *)
  val full : int -> int -> t

  val dim : t -> int * int

  val is_empty : t -> bool
  val mem : int * int -> t -> bool
  val add : int * int -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val included : t -> t -> bool
  val cardinal : t -> int
  val inter : t -> t -> t
  val remove : int * int -> t -> t
  val for_all : (int * int -> bool) -> t -> bool
  val fold : (int * int -> 'a -> 'a) -> t -> 'a -> 'a
  val enum : t -> (int * int) Enum.t
  val of_enum : int -> int -> (int * int) Enum.t -> t
  val iter : (int * int -> unit) -> t -> unit
  val map : (int * int -> 'a) -> t -> 'a list
  val row : t -> int -> Vector.t
  val iter_rows : (Vector.t -> unit) -> t -> unit
  val map_rows : (Vector.t -> Vector.t) -> t -> t
  val mapi_rows : (int -> Vector.t -> Vector.t) -> t -> t
  val transpose : t -> t
  val of_list : int -> int -> (int * int) list -> t
  val to_string : t -> string
  val append_columns : t -> t -> t
end
