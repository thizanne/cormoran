module Make (D : Domain.Outer) : sig
  val analyze : Syntax.TypedProgram.t -> (int list, D.t) Hashtbl.t
  val print : (int list, D.t) Hashtbl.t -> unit
end
