open Batteries

module L = Location
module T = TypedAst
module Ty = Types
module C = Control
module PS = Control.ProgramStructure
module TS = Control.ThreadStructure

(* A thread code portion delimited by two labels.
 * No initial label means 0.
 * No final label means the end of the thread.
*)
type interval = {
  initial : Sym.t L.loc option;
  final : Sym.t L.loc option;
}

let whole_interval = {
  initial = None;
  final = None;
}

(* A thread code portion defined as an union of intervals *)
type thread_zone = interval list

(* Program states set defined as a conjunction of thread zones.
 * Typing should check that a given thread is present at most once in
 * a zone. *)
type zone = (Source.thread_id L.loc * thread_zone) list

type t = {
  zone : zone option;
  (* None means end of the program, once flushed *)
  condition : T.property_condition L.loc;
}

let always_true = {
  zone = None;
  condition = L.mkdummy @@ T.Bool (Location.mkdummy true);
}

let always_false = {
  zone = None;
  condition = L.mkdummy @@ T.Bool (Location.mkdummy false);
}

(* Getting control states from a labelled zone *)

let enum_interval interval t_labels t_final_label =
  (* Enumerates the control labels of an interval *)
  let initial = match interval.initial with
    | None -> C.Label.initial
    | Some { L.item = label; _ } -> Sym.Map.find label t_labels
  in
  let final = match interval.final with
    | None -> t_final_label
    | Some { L.item = label; _ } -> Sym.Map.find label t_labels
  in
  C.Label.enum ~initial ~final

let enum_thread_zone t_zone t_labels t_final_label =
  (* Enumerates the control labels of a thread zone *)
  List.fold_left
    (fun enum_acc interval ->
       Enum.append
         enum_acc
         (enum_interval interval t_labels t_final_label))
    (Enum.empty ())
    t_zone

let to_tzone_list nb_threads zone =
  (* Converts a zone to the list of threaded zones of each thread that
     compose it *)
  let tzone_array =
    Array.create
      nb_threads
      [whole_interval] in
  List.iter
    (fun ({ L.item = thread_id; _ }, thread_zone) ->
       tzone_array.(thread_id) <- thread_zone)
    zone;
  Array.to_list tzone_array

let list_zone zone thread_labels thread_finals =
  (* Lists the control states of a zone *)
  zone
  |> to_tzone_list @@ List.length thread_labels
  |> List.mapi
    (fun tid t_zone ->
       enum_thread_zone t_zone (List.at thread_labels tid) @@
       C.State.tid_label thread_finals tid)
  |> List.map List.of_enum
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
      | Some zone -> List.map data @@ list_zone zone g.PS.labels g.PS.final
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
       list_zone, but with only thread_structs as a parameter. *)
    let thread_labels =
      List.map (fun ts -> ts.TS.labels) thread_structs in
    list_zone zone thread_labels (final_state thread_structs)

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
      (fun d ->
         List.print ~sep:"\n###\n" ~last:"]\n"
           D.print IO.stdout d;
         data_satisfy condition.L.item d)
      (*(data_satisfy condition.L.item) *)
    (all_data zone thread_structs data)
end
