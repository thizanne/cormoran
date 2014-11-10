type t

val empty : t
val init : Syntax.TypedProgram.t -> t
val transfer : t -> int -> Syntax.Typed.t -> t
val union : t -> t -> t

val print : t -> unit
