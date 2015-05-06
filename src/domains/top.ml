open Batteries

type t = Top

let bottom = Top

let is_bottom Top = false

let equal Top Top = true

let init _ = Top

let transfer _ Top = Top

let join Top Top = Top

let widening Top Top = Top

let satisfies _ Top = false

let print output Top = Unit.print output ()
