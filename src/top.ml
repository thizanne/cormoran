type t = Top

let empty = Top

let init _ = Top

let transfer Top _ _ = Top

let union Top Top = Top

let satisfies _ Top = true

let to_string Top = "()"
