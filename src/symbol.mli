type t

val namespace : unit -> string -> t

val name : t -> string

module Table : Map.S with type key = t
