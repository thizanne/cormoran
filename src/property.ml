open Batteries
open Program.Property

let enum_interval interval t_labels t_final_label =
  (* Enumerates the control labels of an interval *)
  let initial = match interval.initial with
    | None -> Program.Control.Label.initial
    | Some label -> Symbol.Map.find label t_labels
  in
  let final = match interval.final with
    | None -> t_final_label
    | Some label -> Symbol.Map.find label t_labels
  in
  Program.Control.Label.enum ~initial ~final

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
      [Program.Property.whole_interval] in
  List.iter
    (fun { Program.thread_id; elem = thread_zone } ->
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
       Program.Control.State.tid_label final_state tid)
  |> List.map List.of_enum
  |> List.n_cartesian_product
  |> List.map Program.Control.State.from_label_list

module Make (D : Domain.Outer) = struct

  let full_flush g abstr =
    List.fold_lefti
      (fun abstr_acc thread_id _thread ->
         D.transfer
           abstr_acc
           (Program.create_threaded ~thread_id Cfg.Operation.MFence))
      abstr
      g.Cfg.program.Program.threads

  let rec satisfies g data = function
    | Condition (zone, thread_id, cond) ->
      let all_data =
        match zone with
        | None -> [data g.Cfg.final_state |> full_flush g]
        | Some zone -> List.map data @@ list_zone zone g
      in
      let thread_id = Option.default 0 thread_id in
      let threaded_cond = Program.create_threaded ~thread_id cond in
      List.for_all (D.satisfies threaded_cond) all_data
    | And (p1, p2) ->
      satisfies g data p1 && satisfies g data p2
    | Or (p1, p2) ->
      satisfies g data p1 || satisfies g data p2

end
