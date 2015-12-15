open Batteries

module Make (D : Domain.Outer) = struct
  module Wto = Graph.WeakTopological.Make (Cfg.G)

  module Data = struct
    type t = D.t

    type edge = Cfg.G.edge

    let equal = D.equal
    let join = D.join

    let analyze (_, op, _) d =
      D.transfer op d

    let widening = D.widening
  end

  module Fixpoint = Graph.ChaoticIteration.Make (Cfg.G) (Data)

  let analyze g widening_delay =
    let wto =
      Wto.recursive_scc g.Cfg.graph @@
      Control.State.initial @@ List.length g.Cfg.program.TypedAst.threads
    in

    let init control_state =
      if Control.State.is_initial control_state
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
