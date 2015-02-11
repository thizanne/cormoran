type t = Top

let bottom = Top

let init _ = Top

let transfer Top _ _ = Top

let join Top Top = Top

let satisfies _ Top = true

let to_string Top = "()"
