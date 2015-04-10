module CommandTerm : sig
  val domain : [> `Concrete | `Oct | `Order | `Polka | `Top ] Cmdliner.Term.t
  val widening_delay : int Cmdliner.Term.t
  val use_litmus : bool Cmdliner.Term.t
  val filename : string Cmdliner.Term.t
end

module Parse : sig
  val parse : (bool -> string -> Program.t * 'a list) Cmdliner.Term.t
end

module Domain : sig
  val get :
    ([< `Concrete | `Oct | `Order | `Polka | `Top ] -> (module Domain.Outer))
      Cmdliner.Term.t
end
