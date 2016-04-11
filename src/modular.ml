open Batteries

module C = Control
module T = TypedAst
module TS = Control.ThreadStructure

module type Control = sig
  type label
  val alpha : Control.Label.t -> label
end

module type ThreadState = sig
  type 'a t constraint 'a = [< `Simple | `Extended]

  val bottom : [`Simple] t
  val is_bottom : [`Simple] t -> bool
  val equal : 'a t -> 'a t -> bool
  val init : T.program -> Source.thread_id -> [`Simple] t
  val transfer : Source.thread_id -> Operation.t -> [`Simple] t -> [`Extended] t
  val meet_cond : T.property_condition -> [`Simple] t -> [`Simple] t
  val join : [`Simple] t -> [`Simple] t -> [`Simple] t
  val widening : [`Simple] t -> [`Simple] t -> [`Simple] t
  val print : 'a IO.output -> [`Simple] t -> unit

  val extend : [`Simple] t -> [`Extended] t
  val img : [`Extended] t -> [`Simple] t
end

module type Interferences = sig
  module SourceDomain : ThreadState

  type interference

  val join : interference -> [`Extended] SourceDomain.t -> interference
  val meet : [`Extended] SourceDomain.t -> interference -> [`Extended] SourceDomain.t
end

module OneThread (I : Domain.Inner) = struct
  (* TODO: better WTO for one-thread graphs *)
  module Wto = Graph.WeakTopological.Make (TS.Graph)

  module Data = struct
    type t

    type edge = TS.Graph.edge


  end
end
