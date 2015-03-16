open Batteries

type t

val namespace : unit -> string -> t

val name : t -> string

val print : 'a IO.output -> t -> unit

module Ord : sig
  val compare : t -> t -> int
end

module Map : Map.S with type key = t
module Set : Set.S with type elt = t
