open Batteries

module ProgramState : Domain.ProgramState = struct
  type t = Top

  let bottom = Top

  let is_bottom Top = false

  let equal Top Top = true

  let top _ = Top

  let transfer _ _ Top = Top

  let meet_cond _ Top = Top

  let join Top Top = Top

  let widening Top Top = Top

  let print output Top = Unit.print output ()
end

module ThreadAnalysis : Modular.ThreadAnalysis = struct
  module StateAbstraction = struct
    type t = Top
    let is_bottom Top = false
    let equal Top Top = true
    let join Top Top = Top
    let print output Top = Unit.print output ()
    let bottom _ _ _ = Top
    let top _ _ _ = Top
    let meet_cond _ Top = Top
    let are_consistent _ = true
    let widening Top Top = Top
    let meet_label _ _ Top = Top
  end

  module Interferences = struct
    type t = Top
    let bottom _ _ = Top
    let equal Top Top = true
    let join Top Top = Top
    let widening Top Top = Top
    let print output Top = Unit.print output ()
  end

  module Application = struct
    type state = StateAbstraction.t

    type interference = Interferences.t

    let apply _ StateAbstraction.Top Interferences.Top =
      StateAbstraction.Top, Interferences.Top

    let generate _ _ _ StateAbstraction.Top =
      StateAbstraction.Top, Interferences.Top
    end
  end
