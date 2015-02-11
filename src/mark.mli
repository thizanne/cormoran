type t

val bottom : t
val init : Syntax.TypedProgram.t -> t
val transfer : t -> int -> Syntax.Typed.t -> t
val join : t -> t -> t

val satisfies : Condition.t -> t -> bool

val to_string : t -> string
