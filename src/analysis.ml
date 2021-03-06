open Batteries

module PA = Param.Abstraction

module type S = sig
  type data
  type control

  val get_control : TypedAst.program -> control
  val export_graph : Param.t -> control -> data -> unit
  val analyse : Param.t -> TypedAst.program -> control -> data
  val check_property : Property.t -> control -> data -> bool
  val print_result : 'a IO.output -> TypedAst.program -> control -> data -> unit
end

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
  | PA.Top -> (module Top.ProgramState)
  | PA.MarkSmart -> (module Mark.Make (Inner))
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
  | PA.MarkSmart -> (module MarkThreadSmart.Make (Inner) (Control))
  | PA.SC -> (module SequentialConsistency.Make (Inner) (Control))
  | PA.Top -> (module Top.ThreadAnalysis)

module Interleaving (D : Domain.ProgramState) : S = struct
  type data = Control.State.t -> D.t
  type control = Control.ProgramStructure.t

  module Dot = ExportCfg.Make (D)
  module Analysis = Interleaving.Make (D)
  module Prop = Property.Make (D)

  let get_control = Control.ProgramStructure.of_program

  let export_graph param control data =
    match param.Param.graph with
    | None -> ()
    | Some filename ->
      Dot.export_interleaving_graph filename control data

  let analyse param program control =
    Analysis.analyze program control param.Param.state_widening_delay

  let check_property prop control data =
    Prop.satisfies prop control data

  let print_result output _program control data =
    Control.ProgramStructure.(
      Graph.iter_vertex (
        fun v ->
          Printf.fprintf output "############%a\n\n%a\n\n%!"
            Control.State.print v
            D.print (data v)
        )
        control.graph
    )
end

module Modular (TA : Modular.ThreadAnalysis) : S = struct
  type data =
    TA.Interferences.t list *
    (Control.Label.t -> TA.StateAbstraction.t) list

  type control = Control.ThreadStructure.t list

  module Analysis = Modular.ProgramAnalysis (TA)
  module Prop = Property.MakeModular (TA.StateAbstraction) (TA.Application)
  module Dot = ExportCfg.Make (TA.StateAbstraction)

  let get_control program =
    List.map Control.ThreadStructure.of_thread program.TypedAst.threads

  let export_graph param control (_intf, data) =
    match param.Param.graph with
    | None -> ()
    | Some filename ->
      Dot.export_modular_graph filename control data

  let analyse param program control =
    Analysis.analyse param program control
      param.Param.state_widening_delay param.Param.intf_widening_delay

  let check_property prop control (_intf, data) =
    Prop.satisfies prop control data

  let print_thread_result output _program tid thread_control thread_data =
    (* TODO: do this properly with colors and stuff *)
    Printf.fprintf output " ------------------------\n";
    Printf.fprintf output "| Results for thread n°%d |\n" tid;
    Printf.fprintf output " ------------------------\n\n";
    Control.ThreadStructure.(
      Graph.iter_vertex (
        fun v ->
          Printf.fprintf output "###########\nAt label %a:\n\n%a\n\n%!"
            Control.Label.print v
            TA.StateAbstraction.print (thread_data v)
      )
        thread_control.graph
    )

  let print_thread_intf output _program tid thread_intf =
    Printf.fprintf output "\n\n";
    Printf.fprintf output " -------------------------------\n";
    Printf.fprintf output "| Interferences for thread n°%d |\n" tid;
    Printf.fprintf output " -------------------------------\n\n";
    TA.Interferences.print output thread_intf

  let print_result output program control (intf, data) =
    List.iter2i (print_thread_result output program) control data;
    List.iteri (print_thread_intf output program) intf
end

let get_analysis param : (module S) =
  match param.Param.method_ with
  | Param.Method.Interleaving ->
    (module Interleaving ((val get_program_state param)))
  | Param.Method.Modular ->
    (module Modular ((val get_thread_analysis param)))
