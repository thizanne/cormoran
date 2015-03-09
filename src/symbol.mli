open Batteries

type t

val namespace : unit -> string -> t

val name : t -> string

module Map : Map.S with type key = t
module Set : Set.S with type elt = t
