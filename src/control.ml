open Batteries

type thread_id = int

module Label = struct
  include Int

  let initial = 0

  let enum ~initial ~final =
    initial -- final

  let hash = Hashtbl.hash
end

module State = struct
  type t = Label.t list

  let equal = List.eq Label.equal

  let hash = Hashtbl.hash

  let empty = []

  let tid_label = List.nth

  let add_label = List.cons

  let from_label_list labels = labels

  let compare = List.compare Label.compare

  let is_initial =
    List.for_all (( = ) 0)

  let initial nb_threads =
    List.make nb_threads 0

  let print output =
    List.print Int.print ~first:"" ~last:"" output
end
