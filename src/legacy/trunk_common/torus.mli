(***********************************************************************)
(*                                                                     *)
(*                               ALCOOL                                *)
(*                                                                     *)
(*                              CEA LIST                               *)
(*                                                                     *)
(*                              torus.mli                              *)
(*                                                                     *)
(***********************************************************************)

(** Finite products of arcs. Gathers in a single module all the
subsets of all the n-dimensional tori for n ranging in the set of
natural numbers that are either empty or a product of non empty arcs
(see Arc). Such subsets are still called cubes and their boundaries
are still taken in a poset that can be embeded in the positive
half-line.*)

module type In =
sig
  module IntSet:
    sig 
      include Set.S
      val segment_initial: ?start:t -> int -> t
    end with type elt = int
  module Array:
  sig
    val cartesian_next_array_consumer: int array -> int -> unit
    val cartesian_next_array_consumer2: int array -> int array -> unit  
  end
end with type IntSet.elt = int

module type S =
sig
type underlying

  exception Undefined
    (** Arcs dont les produits constituent les cubes.*)
  type arc
    (** Points of the space.*)
  type point 

  type regular_value
    (** Admissible values for bounds *)

  type ints
    (** Set of integers.*)

  type t
    (** Tori *)

  val of_arc: arc -> t

  val compare_points: point -> point -> int
    (** Comparing points in order to use the functor Set.Make.*)

  val affine_dimension: t -> int

  val atom: point -> t
    (** Turn a point into a singleton that contains it. In particular
	\{p\}=\{p.(1)\}x...x\{p.(n)\} is an atom in the complete
	lattice of cubes. *)

    (** renvoie la base, dans le cas où le retour est vide, cela signifie que l'on n'a pas un "vrai" cylindre. *)
  val base_coordinates: t -> ints (*Common.IntSet.t*)
    (** Dimension de la base, dans le cas où le retour est 0, cela
	signifie que l'on n'a pas un "vrai" cylindre. *)
  val base_dimension: t -> int
    (*
      val below: int -> regular_value -> t -> t (* to be done *)
    *)
    (*
    (** Nécessaire au calcul du complémentaire d'un région cubique. *)
      val before: int -> t -> t 
    *)
    (*
    (** Utilisé pour détecter les deadlocks potentiels. *)
      val belongs_n_way_below_lub: point -> t -> bool
    *)
    (** Test whether a point belong to a block. *)
  val belongs_to: point -> t -> bool 
    (** Test whether a point belong to the boundary of a block. *)
  val belongs_to_boundary: point -> t -> bool 
    (** Arcs (a,b) (a,b( )a,b) )a,b( (a) or )a( with a distinct from b. *)
  val bounded: bool -> bool -> regular_value -> regular_value -> t 
    (** Topological closure: the least closed subset containing the block. *)
  val closure: t -> t 
    (** Required to use the functor Set.Make e.g. in the module Area. *)
  val compare: t -> t -> int 
    (** Nécessaire au calcul du complémentaire d'un tore *)
  val compare_arc: arc -> arc -> int
    (** Comparing arcs so the module built over Tore inherits from Arc. *)
  val complement: t -> (t list) 
    (** Cylindre de dimension n (1er argument), dont la projection sur
	chaque coordonnée appartenant au 2ème argument est la demi-droite
	réelle positive et dont la base est le troisième argument.*)
  val cylinder: int -> ints -> t -> t
    (** Dimension de l'espace dans lequel "vit" le cube passé en
	argument, c'est-à-dire son rang dans l'algèbre graduée des
	cubes. *)
  val dimension: t -> int
    (** Cône en-dessous d'un cube c'est-à-dire ensemble des points de
	l'espace qui sont en dessous de l'un des points du cube passé en
	argument. *)

  (*
    val downward: t -> t 
  *)

  val degenerate_coordinates: t -> ints
    (*
      val during: int -> t -> t (* Nécessaire au calcul du complémentaire d'un région cubique. *)
    *)
    (** Renvoie le cube vide de dimension n. La nécessité de distinguer
	les cubes vides de dimension différentes est due à la structure
	"d'algèbre booléenne graduée" : voir la documentation mathématique.
    *)
  val empty: int -> t

  val in_the_future_of: t -> t -> t

  val in_the_past_of: t -> t -> t

  val intersection: t -> t -> (t list)

  (*

  (** Face avant/arrière d'un cube. *)
    val face: bool -> int -> t -> t 
  (** Face avant d'un cube *)
    val front: int -> t -> t 
  (** indique si les deux cubes passés en argument se rencontrent et
    qu'aucun des deux ne se trouve dans le passé de l'autre.
  *)

  *)

  val full: int -> t 
    (** Coin inférieur du cube. *)
  val glb: t -> point

  (*

  (** Nécessaire au calcul de la région accessible. *)
    val in_the_future_of: t -> t -> t
  (** Nécessaire au calcul de la région inaccessible. *) 
    val in_the_past_of: t -> t -> t

  *)

  (** Teste si la réunion de deux tores est connexe. *)
  val in_touch: t -> t -> bool

  val initial: bool -> regular_value -> t (* [0,x] ou [0,x[ selon la valeur true/false du booléen. *)

  (** Renvoie le plus grand ouvert inclus dans le cube passé en
      argument. *)
  val interior: t -> t
    (** Teste si le premier tore est inclus dans le second. *)
  val is_included: t -> t -> bool
    (** Teste si le tore n'est pas vide. *)
  val is_empty: t -> bool 
  val is_not_empty: t -> bool 
  val is_full: t -> bool 
  val is_not_full: t -> bool 
    (*
    (** Enveloppe torique de deux tores. *)
      val join: t -> t -> t  
    *)
    (** Coin supérieur du cube. Cette fonction peut lever une exception. *)
  val lub: t -> point 
    (** Crée un tore à partir d'une liste d'arc. *)
  (* val make: (arc list) -> t *)
    (*
    (** Intersection ensembliste de deux tores : peut lever une exception. *)
      val meet: t -> t -> t 
    *)
    (* val neighbourhood: point -> t *)
    (** Forme normale d'un tore. *)
  val normalize: t -> t 
    (** Crée un cube à partir d'une liste/d'un tableau d'intervalles. *)
  val of_list: (arc list) -> t
  val of_array: (arc array) -> t
    (** Origine sur le tore. *)
  val origin: int -> point 
    (** Produit cartésien de deux tores. *)
  val product: t -> t -> t 
  val projection: ints -> t -> t
    (*  val projection: (int array) -> t -> interval *)
    (*
      val slash: int -> regular_value -> t -> t (* to be done *)
    *)
    (*  val strictly_above: int -> regular_value -> t -> t (* to be
	done *) val strictly_below: int -> regular_value -> t -> t (* to
	be done *)
    *)
  val string_of: t -> string (* Affichage *)
  val string_of_arc: arc -> string (* Affichage *)
  val terminal: bool -> regular_value -> t (* [x,-[ ou ]x,-[ selon la valeur true/false du booléen. *)
  val to_list: t -> (arc list)
  val to_array: t -> (arc array)
  val underlying: t -> underlying

(* val upward: t -> t (** Cône au-dessus d'un cube. *) *)
(* val way_below_closure: t -> t (* utilisé pour la détection de
   deadlocks potentiels. *)
*)
end

module Make(Overall:In)(A:Arc.S):
  (
    S with type regular_value = A.regular_value
	     and type point = A.regular_value array
	     and type arc = A.t
       and type ints = Overall.IntSet.t
  )
