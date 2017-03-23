open Batteries

module PA = Param.Abstraction

module type S = sig
  type data
  type control

  val get_control : TypedAst.program -> control
  val export_graph : Param.t -> control -> data -> unit
  val analyse : Param.t -> TypedAst.program -> control -> data
  val check_property : Property.t -> control -> data -> bool
end

let get_output = function
  | None -> IO.stdnull
  | Some filename -> File.open_out filename

let get_inner param : (module Domain.Inner) =
  match param.Param.inner with
  | PA.BddPolka -> (module BddapronAdapter.Polka)
  | PA.BddOct -> (module BddapronAdapter.Oct)
  | PA.Polka -> (module ApronAdapter.Polka)
  | PA.Oct -> (module ApronAdapter.Oct)

let get_control param : (module Domain.ControlAbstraction) =
  match param.Param.control with
  | PA.Concrete -> (module ConcreteControl)
  | PA.FromLabels -> (module ControlAbstractionFromLabels)

let get_program_state param : (module Domain.ProgramState) =
  let module Inner = (val get_inner param) in
  match param.Param.outer with
  | PA.Mark -> (module Mark.Make (Inner))
  | PA.Top -> (module Top)
  | PA.MarkNoLocal ->
    Error.not_implemented_msg_error
      "mark-nolocal makes only sense in modular analysis"
  | PA.SC ->
    Error.not_implemented_msg_error
      "SC abstractions are not implemented for interleaving analysis"

let get_thread_analysis param : (module Modular.ThreadAnalysis) =
  let module Inner = (val get_inner param) in
  let module Control = (val get_control param) in
  match param.Param.outer with
  | PA.Mark -> (module MarkThread.Make (Inner) (Control))
  | PA.MarkNoLocal -> (module MarkThreadNoLocal.Make (Inner) (Control))
  | PA.SC -> (module SequentialConsistency.Make (Inner) (Control))
  | PA.Top ->
    Error.not_implemented_msg_error
      "Top domain is not yet implemented for modular analysis"

module Interleaving (D : Domain.ProgramState) : S = struct
  type data = Control.State.t -> D.t
  type control = Control.ProgramStructure.t

  module Dot = ExportCfg.Dot (D)
  module Analysis = Interleaving.Make (D)
  module Prop = Property.Make (D)

  let get_control = Control.ProgramStructure.of_program

  let export_graph param control data =
    Dot.output_graph (get_output param.Param.graph) data control

  let analyse param program control =
    Analysis.analyze program control param.Param.wdelay

  let check_property prop control data =
    Prop.satisfies prop control data
end

module Modular (TA : Modular.ThreadAnalysis) : S = struct
  type data = (Control.Label.t -> TA.StateAbstraction.t) list
  type control = Control.ThreadStructure.t list

  module Analysis = Modular.ProgramAnalysis (TA)
  module Prop = Property.MakeModular (TA.StateAbstraction) (TA.Application)

  let get_control program =
    List.map Control.ThreadStructure.of_thread program.TypedAst.threads

  let export_graph param _ _ =
    match param.Param.graph with
    | None -> ()
    | Some _ ->
      Error.not_implemented_msg_error
        "Graph export is not yet available for modular analysis"

  let analyse param program control =
    Analysis.analyse program control param.Param.wdelay param.Param.wdelay

  let check_property prop control data =
    Prop.satisfies prop control data
end

let get_analysis param : (module S) =
  match param.Param.method_ with
  | Param.Method.Interleaving ->
    (module Interleaving ((val get_program_state param)))
  | Param.Method.Modular ->
    (module Modular ((val get_thread_analysis param)))
