open Batteries

module type Context = sig
  type 'a t
end

module Identity = struct
  type 'a t = 'a
end

module AddSimpleContext (Ctx : sig type t end) = struct
  type context = Ctx.t

  type 'a t = {
    item : 'a;
    context : context;
  }

  let create item context =
    { item; context }
end

module Source = struct
  type t =
    | Local of Control.thread_id
    | View of Control.thread_id
    | Mem
end

module Sourced = struct
  include AddSimpleContext (Source)

  let print print_item output { item; context } =
    match context with
    | Source.Mem ->
      Printf.fprintf output "%a:mem" print_item item
    | Source.Local thread_id
    | Source.View thread_id ->
      Printf.fprintf output "%a:%d" print_item item thread_id
end

module Visibility = struct
  type t =
    | Local
    | Shared
end

module Visible = struct
  include AddSimpleContext (Visibility)

  let is_shared { context; _ } = match context with
    | Visibility.Local -> false
    | Visibility.Shared -> true

  let is_local { context; _ } = match context with
    | Visibility.Local -> true
    | Visibility.Shared -> false
end
