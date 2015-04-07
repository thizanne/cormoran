open Batteries

type t = Top

let bottom = Top

let equal Top Top = true

let init _ = Top

let transfer Top _ = Top

let join Top Top = Top

let widening Top Top = Top

let satisfies _ Top = true

let print output Top = Unit.print output ()
