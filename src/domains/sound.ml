open Batteries

module Key = struct
  type source =
    | Mem
    | TopBuffer
    | BottomBuffer
    [@@ deriving ord]

  let down_source = function
    | Mem -> TopBuffer
    | TopBuffer -> BottomBuffer
    | BottomBuffer -> BottomBuffer

  let up_source = function
    | Mem -> raise @@ Invalid_argument "up_source"
    | TopBuffer -> Mem
    | BottomBuffer -> TopBuffer

  module M = Symbol.Map

  type thread_sources = source M.t

  type t = thread_sources list

  let compare =
    List.compare (Symbol.Map.compare compare_source)

  let print _output _ = failwith "Sound.Key.print not implemented"

end

module Make (Inner : Domain.Inner) = struct

  module M = Map.Make (Key)

  type t = Inner.t M.t

  let normalize =
    M.filterv (fun abstr -> not (Inner.is_bottom abstr))

  let is_bottom =
    M.for_all (fun _key d -> Inner.is_bottom d)

  let equal d1 d2 =
    M.equal Inner.equal (normalize d1) (normalize d2)

  let print output =
    M.print
      ~first:"" ~last:"" ~kvsep:":\n" ~sep:"\n────────\n"
      Key.print Inner.print output

  let join =
    M.merge
      (fun _key abstr1 abstr2 -> match (abstr1, abstr2) with
         | None, _ -> abstr2
         | _, None -> abstr1
         | Some abstr1, Some abstr2 -> Some (Inner.join abstr1 abstr2))

  let widening =
    M.merge
      (fun _key abstr1 abstr2 -> match (abstr1, abstr2) with
         | None, _ -> abstr2
         | _, None -> failwith "Abstract.widening"
         | Some abstr1, Some abstr2 ->
           Some (Inner.widening abstr1 abstr2))
end
