open Batteries

(* A thread code portion delimited by two labels.
 * No initial label means 0.
 * No final label means the end of the thread.
*)
type interval = {
  initial : Symbol.t option;
  final : Symbol.t option;
}

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

(* A thread code portion defined as an union of intervals *)
type thread_zone = interval list

let enum_thread_zone t_zone t_labels t_final_label =
  (* Enumerates the control labels of a thread zone *)
  List.fold_left
    (fun enum_acc interval ->
       Enum.append
         enum_acc
         (enum_interval interval t_labels t_final_label))
    (Enum.empty ())
    t_zone

(* Program states set defined as a conjunction of thread zones.
   Invariant: the length of a zone used to define a property over
   a given program is the number of threads of the program.
*)
type zone = thread_zone list

let list_zone (zone : zone) { Cfg.labels; final_state; _ } =
  (* Lists the control states of a zone*)
  zone
  |> List.mapi
    (fun tid t_zone ->
       enum_thread_zone t_zone labels.(tid) @@
       Program.Control.State.tid_label final_state tid)
  |> List.map List.of_enum
  |> List.n_cartesian_product
  |> List.map Program.Control.State.from_label_list

type t =
  | Condition of
      zone option * (* None means end of the program, once flushed *)
      Program.condition Program.threaded
  | And of t * t
  | Or of t * t

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
    | Condition (zone, cond) ->
      let all_data =
        match zone with
        | None -> [data g.Cfg.final_state |> full_flush g]
        | Some zone -> List.map data @@ list_zone zone g
      in
      List.for_all (D.satisfies cond) all_data
    | And (p1, p2) ->
      satisfies g data p1 && satisfies g data p2
    | Or (p1, p2) ->
      satisfies g data p1 || satisfies g data p2

end
