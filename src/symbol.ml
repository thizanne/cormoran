open Printf
open Batteries

type t = string

let print = String.print

let compare = String.compare

let fresh_label =
  let n = ref (-1) in
  function () ->
    incr n;
    sprintf "__label_%d" !n
