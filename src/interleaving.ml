module Make (D : Domain.Domain) = struct
  module Fixpoint = Graph.Fixpoint.Make (Cfg.G)
      (struct
        type vertex = Cfg.G.vertex
        type edge = Cfg.G.edge
        type g = Cfg.G.t
        type data = D.t
        let direction = Graph.Fixpoint.Forward
        let equal = ( = )
        let join = D.union
        let analyze (_, {Cfg.E.thread; ins}, _) d =
          D.transfer d thread ins
      end)

  let make_result p =
    (module struct
       module Domain = D
       let data = Fixpoint.analyze (fun _ -> D.init p) (Cfg.make p)
     end : Analysis.Result)
  end
