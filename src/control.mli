open Batteries

type thread_id = int

module Label : sig
  type t
  val initial : t
  val succ : t -> t
  val enum : initial:t -> final:t -> t Enum.t
  val compare : t -> t -> int
  val print : 'a IO.output -> t -> unit
end

module State : sig
  type t
  val empty : t
  val add_label : Label.t -> t -> t
  val from_label_list : Label.t list -> t
  val tid_label : t -> thread_id -> Label.t
  val is_initial : t -> bool
  val initial : int -> t
  val compare : t -> t -> int
  val print : 'a IO.output -> t -> unit
end
