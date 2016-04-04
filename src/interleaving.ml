open Batteries

module PS = Control.ProgramStructure

module Make (D : Domain.Outer) = struct
  module Wto = Graph.WeakTopological.Make (PS.Graph)

  module Data = struct
    type t = D.t

    type edge = PS.Graph.edge

    let equal = D.equal
    let join = D.join

    let analyze (_, (tid, op), _) d =
      D.transfer tid op d

    let widening = D.widening
  end

  module Fixpoint = Graph.ChaoticIteration.Make (PS.Graph) (Data)

  let analyze g widening_delay init =
    let wto =
      Wto.recursive_scc g.PS.graph @@
      Control.State.initial @@ List.length g.PS.labels
    in

    let init control_state =
      if Control.State.is_initial control_state
      then init
      else D.bottom
    in

    let widening_set =
      Graph.ChaoticIteration.FromWto
    in

    let result =
      Fixpoint.recurse
        g.PS.graph
        wto
        init
        widening_set
        widening_delay

    in fun state -> Fixpoint.M.find state result
end
