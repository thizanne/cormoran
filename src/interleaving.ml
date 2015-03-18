open Batteries

module P = Program

module Make (D : Domain.Outer) = struct
  module Fixpoint = Graph.Fixpoint.Make (Cfg.G)
      (struct
        type vertex = Cfg.G.vertex
        type edge = Cfg.G.edge
        type g = Cfg.G.t
        type data = D.t
        let direction = Graph.Fixpoint.Forward
        let equal = D.equal
        let join = D.join
        let analyze (_, op, _) d =
          D.transfer d op
      end)

  include Fixpoint

  let make_analyze program =
    analyze
      (fun control_state ->
         if P.is_initial control_state
         then D.init program
         else D.bottom)
      (Cfg.of_program program)
  end
