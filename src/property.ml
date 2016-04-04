open Batteries

module L = Location
module T = TypedAst
module Ty = Types
module C = Control
module PS = Control.ProgramStructure

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

let to_tzone_list { PS.labels; _ } zone =
  (* Converts a zone to the list of threaded zones of each thread that
     compose it *)
  let tzone_array =
    Array.create
      (List.length labels)
      [whole_interval] in
  List.iter
    (fun ({ L.item = thread_id; _ }, thread_zone) ->
       tzone_array.(thread_id) <- thread_zone)
    zone;
  Array.to_list tzone_array

let list_zone zone ({ PS.labels; final; _ } as g) =
  (* Lists the control states of a zone *)
  zone
  |> to_tzone_list g
  |> List.mapi
    (fun tid t_zone ->
       enum_thread_zone t_zone (List.at labels tid) @@
       C.State.tid_label final tid)
  |> List.map List.of_enum
  |> List.n_cartesian_product
  |> List.map C.State.from_label_list

module Make (D : Domain.Outer) = struct
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
      | Some zone -> List.map data @@ list_zone zone g
    in
    List.for_all
      (data_satisfies condition.L.item)
      all_data
end
