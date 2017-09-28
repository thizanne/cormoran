open Batteries

module L = Location
module T = TypedAst
module Ty = Types
module C = Control
module PS = Control.ProgramStructure
module TS = Control.ThreadStructure

(* Program states set defined as a conjunction of thread zones.
 * Typing should check that a given thread is present at most once in
 * a zone. *)
type zone = (Source.thread_id L.loc * Sym.t L.loc) list

type t = {
  zone : zone option;
  (* None means end of the program, once flushed *)
  condition : T.property_condition L.loc;
}

let rec zone_thread_labels zone tid thread_labels thread_final =
  (* List the control labels of a thread concerned by a zone. Namely,
     if a named label of this thread is specified in the zone, only
     the corresponding control label is concerned, otherwise all
     thread control labels are concerned. *)
  match zone with
  | [] ->
    C.Label.enum ~initial:C.Label.initial ~final:thread_final
    |> List.of_enum
  | ({ L.item = tid'; _}, { L.item = lbl_sym; _ }) :: zone_tl ->
    if tid = tid'
    then [Sym.Map.find lbl_sym thread_labels]
    else zone_thread_labels zone_tl tid thread_labels thread_final

let zone_states zone labels final_state =
  List.map2i (zone_thread_labels zone) labels (final_state : C.State.t :> C.Label.t list)
  |> List.n_cartesian_product
  |> List.map C.State.from_label_list

module Make (D : Domain.ProgramState) = struct
  let full_flush g abstr =
    List.fold_lefti
      (fun abstr_acc thread_id _label ->
         D.transfer
           thread_id
           Operation.MFence
           abstr_acc)
      abstr
      g.PS.labels

  let data_satisfies condition abstr =
    let neg_condition = T.Unop (L.mkdummy T.Not, L.mkdummy condition) in
    D.is_bottom @@ D.meet_cond neg_condition abstr

  let satisfies { zone; condition } g data =
    let all_data = match zone with
      | None -> [data g.PS.final |> full_flush g]
      | Some zone -> List.map data @@ zone_states zone g.PS.labels g.PS.final
    in
    List.for_all
      (data_satisfies condition.L.item)
      all_data
end

module MakeModular
    (D : Domain.ThreadState)
    (A : Modular.Application with type state = D.t)
=
struct
  let full_flush thread_struct thread_state =
    fst @@
    A.generate
      Operation.MFence
      thread_struct.TS.final
      thread_struct.TS.final
      thread_state

  let data_satisfy condition abstrs =
    let neg_condition = T.Unop (L.mkdummy T.Not, L.mkdummy condition) in
    abstrs
    |> List.map @@ D.meet_cond neg_condition
    |> D.are_consistent
    |> ( not )

  let meet_control_data control_state thread_id thread_data =
    (* Takes a thread_data corresponding to the given thread id and
       meets the labels of the other threads accordingly with the
       control state *)
    List.fold_lefti
      (fun acc other_tid label ->
         if other_tid = thread_id
         then acc
         else D.meet_label other_tid label acc)
      thread_data
      control_state

  let meet_control control_state thread_data_list =
    (* Takes a list of thread data and meets the labels on each one
       accordingly with the control state *)
    List.mapi
      (fun thread_id thread_data ->
         meet_control_data control_state thread_id thread_data)
      thread_data_list

  let all_thread_data data control_state =
    (* Takes a control state and returns the corresponding list of
       data of the threads *)
    let lbl_list : C.State.t :> C.Label.t list = control_state in
    List.map2
      (fun thread_data label -> thread_data label (*|> meet_control lbl_list*))
      data lbl_list
    |> meet_control lbl_list

  let final_state thread_structs =
    C.State.from_label_list @@
    List.map (fun ts -> ts.TS.final) thread_structs

  let all_states zone thread_structs =
    (* Returns all control states of a zone. Basically a wrapper of
       zone_states, but with only thread_structs as a parameter. *)
    let thread_labels =
      List.map (fun ts -> ts.TS.labels) thread_structs in
    zone_states zone thread_labels (final_state thread_structs)

  let all_data zone thread_structs data =
  (* Takes a zone option and returns the sequence of all lists of thread
     data to check *)
    match zone with
    | None ->
      [
        final_state thread_structs
        |> all_thread_data data
        |> List.map2 full_flush thread_structs
      ]
    | Some zone ->
      List.map (all_thread_data data) (all_states zone thread_structs)

  let satisfies { zone; condition } thread_structs data =
    List.for_all
      (data_satisfy condition.L.item)
      (all_data zone thread_structs data)
end
