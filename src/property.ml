open Batteries

module L = Location
module P = Program

(* A thread code portion delimited by two labels.
 * No initial label means 0.
 * No final label means the end of the thread.
*)
type interval = {
  initial : Symbol.t option;
  final : Symbol.t option;
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
type zone = (P.thread_id * thread_zone) list

type 'a t = {
  zone : zone option;
  (* None means end of the program, once flushed *)
  condition : 'a P.condition;
}

let always_true = {
  zone = None;
  condition = P.Bool (Location.mkdummy true);
}

let always_false = {
  zone = None;
  condition = P.Bool (Location.mkdummy false);
}

(* Getting control states from a labelled zone *)

let enum_interval interval t_labels t_final_label =
  (* Enumerates the control labels of an interval *)
  let initial = match interval.initial with
    | None -> P.Control.Label.initial
    | Some label -> Symbol.Map.find label t_labels
  in
  let final = match interval.final with
    | None -> t_final_label
    | Some label -> Symbol.Map.find label t_labels
  in
  P.Control.Label.enum ~initial ~final

let enum_thread_zone t_zone t_labels t_final_label =
  (* Enumerates the control labels of a thread zone *)
  List.fold_left
    (fun enum_acc interval ->
       Enum.append
         enum_acc
         (enum_interval interval t_labels t_final_label))
    (Enum.empty ())
    t_zone

let to_tzone_list { Cfg.labels; _ } zone =
  (* Converts a zone to the list of threaded zones of each thread that
     compose it *)
  let tzone_array =
    Array.create
      (Array.length labels)
      [whole_interval] in
  List.iter
    (fun (thread_id, thread_zone) ->
       tzone_array.(thread_id) <- thread_zone)
    zone;
  Array.to_list tzone_array

let list_zone zone ({ Cfg.labels; final_state; _ } as g) =
  (* Lists the control states of a zone *)
  zone
  |> to_tzone_list g
  |> List.mapi
    (fun tid t_zone ->
       enum_thread_zone t_zone labels.(tid) @@
       P.Control.State.tid_label final_state tid)
  |> List.map List.of_enum
  |> List.n_cartesian_product
  |> List.map P.Control.State.from_label_list


module Make (D : Domain.Outer) = struct
  let full_flush g abstr =
    List.fold_lefti
      (fun abstr_acc thread_id _thread ->
         D.transfer
           (Cfg.Operation.MFence thread_id)
           abstr_acc)
      abstr
      g.Cfg.program.P.threads

  let data_satisfies condition abstr =
    let neg_condition = P.LogicUnop (L.mkdummy P.Not, L.mkdummy condition) in
    D.is_bottom (D.transfer (Cfg.Operation.Filter neg_condition) abstr)

  let satisfies { zone; condition } property g data =
    let all_data = match zone with
      | None -> [data g.Cfg.final_state |> full_flush g]
      | Some zone -> List.map data @@ list_zone zone g
    in
    List.for_all
      (data_satisfies condition)
      all_data
end
