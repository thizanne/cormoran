open Batteries

module C = Control
module T = TypedAst
module TS = Control.ThreadStructure

module type Control = sig
  type label
  val alpha : Control.Label.t -> label
end

module type ThreadState = sig
  include Domain.Common

  val bottom : t
  val init : T.program -> Source.thread_id -> t
  val transfer : Operation.t -> t -> t
  val meet_cond : T.property_condition -> t -> t
  val widening : t -> t -> t
end

module type Interferences = sig
  type t
  val bottom : t
  val equal : t -> t -> bool
  val join : t -> t -> t
  val widening : t -> t -> t
end

module type ThreadAnalysis = sig
  module ControlAbstraction : Control
  module StateAbstraction : ThreadState
  module Interferences : Interferences

  val apply : StateAbstraction.t -> Interferences.t -> StateAbstraction.t
  val transfer :
    Control.Label.t ->
    Control.Label.t ->
    Operation.t ->
    StateAbstraction.t ->
    StateAbstraction.t * Interferences.t
end

module OneThread (A : ThreadAnalysis) = struct
  (* TODO: better WTO for one-thread graphs *)
  module Wto = Graph.WeakTopological.Make (TS.Graph)

  module Data = struct
    type t = A.StateAbstraction.t

    type edge = TS.Graph.edge
    let equal = A.StateAbstraction.equal

    let join = A.StateAbstraction.join

    let return_interferences, analyze =
      (* Use closures to update interferences without having an
         explicit global state *)
      let interf = ref A.Interferences.bottom in
      (fun () -> !interf),
      (fun (lbl1, op, lbl2) d ->
         let state, new_interf = A.transfer lbl1 lbl2 op d in
         interf := A.Interferences.join !interf new_interf;
         state)

    let widening = A.StateAbstraction.widening
  end

  module Fixpoint = Graph.ChaoticIteration.Make (TS.Graph) (Data)

  let analyse thread widening_delay init =
    let wto =
      Wto.recursive_scc thread.TS.graph @@
      Control.Label.initial in

    let init label =
      if Control.Label.is_initial label
      then init
      else A.StateAbstraction.bottom
    in

    let widening_set =
      Graph.ChaoticIteration.FromWto
    in

    let result =
      Fixpoint.recurse
        thread.TS.graph
        wto
        init
        widening_set
        widening_delay
    in

    Data.return_interferences (),
    fun state -> Fixpoint.M.find state result
end

module ProgramAnalysis (A : ThreadAnalysis) = struct
  module ThreadAnalysis = OneThread (A)

  let analyse threads widening_delay init =

    let initial_data =
      List.map (const @@ const A.StateAbstraction.bottom) threads in

    let rec fixpoint interf data current final itf_w_delay = function
      (* current is the id option of the thread the function is going
         to analyse. final is the id of the last thread which changed
         the interference set. That means other threads did not add
         interferences w.r.t the last analysis when the iteration
         reaches this thread again. Therefore fixpoint is reached: all
         thread data have been computed with the last interference
         set, therefore these data cannot change anymore.

         current and final are integers and not Source.thread_id: they
         represent indexes in the thread list and do not leave the
         analyse function, so it's ok.

         Interferences widening delay is now the same as
         widening_delay. TODO: change it. It is to be understood as
         the number of analysis of all the threads before doing
         a widening on interferences. *)
      | [] ->
        let itf_w_delay =
          if itf_w_delay = 0 then widening_delay
          else itf_w_delay - 1 in
        fixpoint interf data 0 final itf_w_delay threads
      | _ :: _ when Some current = final ->
        interf, data
      | t :: ts ->
        let interf', d =
          ThreadAnalysis.analyse t widening_delay init in
        let final =
          if A.Interferences.equal interf interf'
          then final
          else Some current in
        let data =
          List.modify_at current (const d) data in
        fixpoint interf' data (succ current) final itf_w_delay ts

    in fixpoint
      A.Interferences.bottom (* initial interferences *)
      initial_data (* initial data for a thread *)
      0 (* first "current analysed state" *)
      None (* Last thread updating interferences *)
      widening_delay
      threads
end
