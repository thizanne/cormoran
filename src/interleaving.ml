open Batteries

module P = Program

module Make (D : Domain.Outer) = struct
  module Wto = Graph.WeakTopological.Make (Cfg.G)

  module Fixpoint = Graph.ChaoticIteration.Make (Cfg.G)
      (struct
        type edge = Cfg.G.edge
        type data = D.t
        let equal = D.equal
        let join = D.join
        let widening =
          D.widening
        let analyze (_, op, _) d =
          D.transfer d op
        let widening_delay = 0
      end)

  include Fixpoint

  let make_analyze g =
    let result =
      recurse
        g.Cfg.graph
        (Wto.recursive_scc g.Cfg.graph @@
         Program.initial_state g.Cfg.program)
        (fun control_state ->
           if P.is_initial control_state
           then D.init g.Cfg.program
           else D.bottom)
    in fun state -> M.find state result
  end
