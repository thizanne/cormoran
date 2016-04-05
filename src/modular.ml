open Batteries

module C = Control
module T = TypedAst
module TS = Control.ThreadStructure

module type Lol = sig
  module Interferences : sig
    type t
    val equal : t -> t -> bool
    val init : T.program -> t
    val update : Source.thread_id -> C.Label.t -> C.Label.t -> Operation.t -> t -> t
  end

  module ThreadState : Domain.Outer

  val apply : Interferences.t -> ThreadState.t -> ThreadState.t
end

module OneThread (I : Domain.Inner) = struct
  (* TODO: better WTO for one-thread graphs *)
  module Wto = Graph.WeakTopological.Make (TS.Graph)

  module Data = struct
    type t

    type edge = TS.Graph.edge


  end
end
