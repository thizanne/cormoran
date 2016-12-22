open Batteries

module Lbl = Control.Label
module TS = Control.ThreadStructure

type label = int

type t = int array array

let nb_threads c = Array.length c

let alpha c tid label =
  let label : Lbl.t :> int = label in
  c.(tid).(label)

let max_alpha c tid =
  let line = c.(tid) in
  line.(Array.length line - 1)

let make_line ts =
  let final :> int = ts.TS.final in
  let c = Array.make (final + 1) 0 in
  let current_alpha = ref 0 in
  for i = 0 to final - 1 do
    begin
      if Sym.Map.exists
          (fun _lbl_sym lbl_val -> lbl_val = !current_alpha)
          (ts.TS.labels :> int Sym.Map.t)
      then incr current_alpha
    end;
    c.(i) <- !current_alpha
  done;
  c

let of_threads tss =
  let c = Array.make (List.length tss) [||] in
  List.iteri (fun i ts -> c.(i) <- make_line ts) tss;
  c
