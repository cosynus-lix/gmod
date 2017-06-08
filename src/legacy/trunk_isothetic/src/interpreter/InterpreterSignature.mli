module type Parameter = sig

  val program: AbstractSyntax.t

end (* Parameter *)

module type S = sig

  val play: (int -> int) -> unit

end (* Interpreter *)
