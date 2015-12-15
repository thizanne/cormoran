open Batteries

type t =
  | Local of Control.thread_id
  | View of Control.thread_id
  | Memory

let threaded thread_id_option origin =
  match thread_id_option, origin with
  | Some thread_id, Types.Local -> Local thread_id
  | Some thread_id, Types.Shared -> View thread_id
  | None, Types.Shared -> Memory
  | None, Types.Local -> failwith "Source.threaded"

let print output = function
  | Local thread_id -> Int.print output thread_id
  | View thread_id -> Int.print output thread_id
  | Memory -> String.print output "mem"
