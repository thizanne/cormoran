open Batteries

module TS = Control.ThreadStructure

type label = int

type t = Control.Label.t list (* Max label of each program *)

let alpha _ _ label =
  (label : Control.Label.t :> int)

let max_alpha x tid =
  (List.at x tid : Control.Label.t :> int)

let of_threads thread_structs =
  List.map (fun ts -> ts.TS.final) thread_structs

let nb_threads x = List.length x
