module Make (D : Domain.Domain) = struct
  module Fixpoint = Graph.Fixpoint.Make (Cfg.G)
      (struct
        type vertex = Cfg.G.vertex
        type edge = Cfg.G.edge
        type g = Cfg.G.t
        type data = D.t
        let direction = Graph.Fixpoint.Forward
        let equal = D.equal
        let join = D.join
        let analyze (_, {Cfg.E.thread; ins}, _) d =
          D.transfer d thread ins
      end)

  include Fixpoint

  end
