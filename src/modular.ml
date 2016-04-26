open Batteries

module C = Control
module T = TypedAst
module TS = Control.ThreadStructure

module type ThreadAnalysis = sig
  module StateAbstraction : Domain.ThreadState
  module Interferences : Domain.Interferences

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
  (* TODO: separate widening delays for different operations *)
  module Wto = Graph.WeakTopological.Make (TS.Graph)

  let analyse thread widening_delay external_intf init =
    (* Data has to be redefined at each thread analysis because the
       analyze function should depend on the external interferences. *)
    let module Data = struct
      type t = A.StateAbstraction.t
      type edge = TS.Graph.edge
      let equal = A.StateAbstraction.equal
      let join = A.StateAbstraction.join
      let widening = A.StateAbstraction.widening

      let rec apply_interferences w_delay state =
        let result = A.apply state external_intf in
        if equal state result then result
        else if w_delay > 0 then apply_interferences (w_delay - 1) result
        else apply_interferences 0 (widening state result)

      let return_interferences, analyze =
        (* Use closures to update generated interferences without
             having an explicit global state *)
        let interf = ref A.Interferences.bottom in
        (fun () -> !interf),
        (fun (lbl1, op, lbl2) d ->
           let state, new_interf = A.transfer lbl1 lbl2 op d in
           let state = apply_interferences widening_delay state in
           interf := A.Interferences.join !interf new_interf;
           state)
    end in
    let module Fixpoint = Graph.ChaoticIteration.Make (TS.Graph) (Data) in

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

  let split_interferences thread_index =
    (* Takes the list of the interferences generated by each thread
       and returns a couple (intf_tid, intf_others) where i_tid is the
       interference set generated by thread_index and i_others is the
       join of all others *)
    List.fold_lefti
      (fun (intf_tid, intf_others) index intf_index ->
         if thread_index = index
         then (intf_index, intf_others)
         else (intf_tid, A.Interferences.join intf_others intf_index))
      (A.Interferences.bottom, A.Interferences.bottom)

  let analyse threads widening_delay init =

    let initial_data =
      List.map (const @@ const A.StateAbstraction.bottom) threads in

    let rec fixpoint interf data current final itf_w_delays = function
      (* current is the id option of the thread the function is going
         to analyse. final is the id of the last thread which changed
         its interference set. That means other threads did not add
         interferences w.r.t the last analysis when the iteration
         reaches this thread again. Therefore fixpoint is reached: all
         thread data have been computed with the last interference
         set, therefore these data cannot change anymore.

         Interferences widening delay works thread-by-thread. This
         should give a better precision. *)
      | [] -> fixpoint interf data 0 final itf_w_delays threads
      | _ :: _ when Some current = final ->
        interf, data
      | t :: ts ->
        let intf_t, intf_other = split_interferences current interf in
        let intf_t', d =
          ThreadAnalysis.analyse t widening_delay intf_other (init current) in
        let intf_t_stable = A.Interferences.equal intf_t intf_t' in
        let final =
          if intf_t_stable
          then final
          else Some current in
        let new_intf_t =
          if intf_t_stable || List.at itf_w_delays current > 0
          then intf_t'
          else A.Interferences.widening intf_t intf_t' in
        let data =
          List.modify_at current (const d) data in
        let interf =
          List.modify_at current (const new_intf_t) interf in
        let itf_w_delays =
          List.modify_at current pred itf_w_delays in
        fixpoint interf data (succ current) final itf_w_delays ts

    in fixpoint
      (List.map (const A.Interferences.bottom) threads) (* initial interferences *)
      initial_data (* initial data for a thread *)
      0 (* first "current analysed state" *)
      None (* Last thread updating interferences *)
      (List.map (const widening_delay) threads) (* widening delays *)
      threads
end
