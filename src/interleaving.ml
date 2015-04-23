open Batteries

module Make (D : Domain.Outer) = struct
  module Wto = Graph.WeakTopological.Make (Cfg.G)

  module Data = struct
    type t = D.t

    type edge = Cfg.G.edge

    let equal = D.equal
    let join = D.join

    let analyze (_, op, _) d =
      D.transfer d op

    let widening = D.widening
  end

  module Fixpoint = Graph.ChaoticIteration.Make (Cfg.G) (Data)

  let analyze g widening_delay =
    let wto =
      Wto.recursive_scc g.Cfg.graph @@
      Program.Control.State.initial g.Cfg.program
    in

    let init control_state =
      if Program.Control.State.is_initial control_state
      then D.init g.Cfg.program
      else D.bottom
    in

    let widening_set =
      Graph.ChaoticIteration.FromWto
    in

    let result =
      Fixpoint.recurse
        g.Cfg.graph
        wto
        init
        widening_set
        widening_delay

    in fun state -> Fixpoint.M.find state result
end
