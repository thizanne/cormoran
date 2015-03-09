open Batteries

type t = int * string

let namespace () =
  let table = Hashtbl.create 128 in
  let n = ref (-1) in
  function name ->
    try
      Hashtbl.find table name, name
    with Not_found ->
      incr n;
      Hashtbl.add table name !n;
      !n, name

let name = snd

module Ord = struct
  type symbol = t
  type t = symbol

  let compare (n1, _sym1) (n2, _sym2) =
    Int.compare n1 n2
end

module Set = Set.Make (Ord)
module Map = Map.Make (Ord)
