open Batteries

module type Context = sig
  type 'a t
end

module Identity = struct
  type 'a t = 'a
end

module Threaded = struct
  type 'a t = {
    item : 'a;
    thread_id : Program.thread_id;
  }
end

module MaybeThreaded = struct
  type 'a t = {
    item : 'a;
    thread_id : Program.thread_id option;
  }

  let create thread_id item =
    { thread_id; item }

  let create_some thread_id item =
    { thread_id = Some thread_id; item }

  let print print_item output { thread_id; item } =
    match thread_id with
    | None ->
      Printf.fprintf output "%a:mem" print_item item
    | Some thread_id ->
      Printf.fprintf output "%a:%d" print_item item thread_id
end
