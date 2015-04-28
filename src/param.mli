open Batteries

module Domain : sig
  type t
  val get : t -> (module Domain.Outer)
end

module Parse : sig
  val parse_filename : bool -> string -> Program.t * 'a list
end

module Output : sig
  val get_output : string option -> unit IO.output
end

module CommandTerm : sig
  val domain : Domain.t Cmdliner.Term.t
  val widening_delay : int Cmdliner.Term.t
  val use_litmus : bool Cmdliner.Term.t
  val sourcefile : string Cmdliner.Term.t
  val outputfile : string option Cmdliner.Term.t
end
