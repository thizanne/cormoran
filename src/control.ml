open Batteries

type thread_id = int

module Label = struct
  include Int

  let initial = 0

  let enum ~initial ~final =
    initial -- final
end

module State = struct
  type t = Label.t list

  let empty = []

  let tid_label = List.nth

  let add_label = List.cons

  let from_label_list labels = labels

  let compare = List.compare Label.compare

  let is_initial =
    List.for_all (( = ) 0)

  let initial program =
    List.map (fun _ -> 0) program.TypedAst.threads

  let print output =
    List.print Int.print ~first:"" ~last:"" output
end
