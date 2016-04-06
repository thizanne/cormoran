open Batteries

module C = Control
module T = TypedAst
module TS = Control.ThreadStructure

module type Environment = sig
  type t

  val init_prog : T.program -> t
  val init_thread : T.program -> Source.thread_id -> t

  val meet : t -> t -> t
  val join : t -> t -> t
  val extend : t -> t
end

module type Main = sig
  module Env : Environment

  include Domain.Outer

  val init : Env.t -> t
end




module type Lol = sig
  module Interferences : sig
    type t
    val equal : t -> t -> bool
    val init_thread : T.program -> Source.thread_id -> t
    val update :
      Source.thread_id -> C.Label.t -> C.Label.t -> Operation.t -> t -> t
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
