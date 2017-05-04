open Batteries
open Cmdliner

let enum_doc enum doc =
  Printf.sprintf "%s. $(docv) must be %s."
    doc (Arg.doc_alts_enum enum)

let cmdliner_converter_of_conv conv = Arg.conv_parser conv, Arg.conv_printer conv

type non_dir_file = string
let non_dir_file_cmdliner_converter =
  cmdliner_converter_of_conv Arg.non_dir_file

module Abstraction = struct
  type inner =
    | BddPolka
    | BddOct
    | Polka
    | Oct

  let inner_enum = [
    "bddpolka", BddPolka;
    "bddoct", BddOct;
    "polka", Polka;
    "oct", Oct;
  ]

  let inner_doc =
    enum_doc inner_enum
      "The logico-numerical domain used inside abstractions"

  type control =
    | Concrete
    | FromLabels

  let control_enum = [
    "concrete", Concrete;
    "from-labels", FromLabels;
  ]

  let control_doc =
    enum_doc control_enum
      "The abstraction used for control in modular analyses"

  type outer =
    | Mark
    | MarkNoLocal
    | SC
    | Top

  let outer_enum = [
    "top", Top;
    "mark", Mark;
    "mark-nolocal", MarkNoLocal;
    "sc", SC;
  ]

  let outer_doc =
    enum_doc outer_enum
      "The abstraction for program states and interferences"
end

module Method = struct
  type t =
    | Modular
    | Interleaving

  let method_enum = [
    "modular", Modular;
    "interleaving", Interleaving;
  ]

  let method_doc =
    enum_doc method_enum
      "The analysis method"
end

include struct
  [@@@ ocaml.warning "-39"]

  type t = {

    sourcefile : non_dir_file
        [@pos 0]
        [@docv "FILE"];
    (** The program to analyse. *)

    graph : string option
        [@aka ["g"]]
        [@docv "FILE"];
    (** Graph output file name. *)

    litmus : bool;
    (** Use litmus syntax. *)

    state_widening_delay : int
        [@aka ["w"]]
        [@default 2];
    (** Number of exact computation steps before widening when
        computing program states. Negative is equivalent to zero. *)

    intf_widening_delay : int
        [@aka ["x"]]
        [@default 2];
    (** Number of exact computation steps before widening when
        generating interferences. Negative is equivalent to zero. *)

    inner : Abstraction.inner
        [@aka ["i"]]
        [@docv "DOMAIN"]
        [@enum Abstraction.inner_enum] [@default Abstraction.BddOct]
        [@doc Abstraction.inner_doc];

    control : Abstraction.control
        [@aka ["c"]]
        [@docv "DOMAIN"]
        [@enum Abstraction.control_enum] [@default Abstraction.FromLabels]
        [@doc Abstraction.control_doc];

    outer : Abstraction.outer
        [@aka ["d"]]
        [@docv "DOMAIN"]
        [@enum Abstraction.outer_enum] [@default Abstraction.Mark]
        [@doc Abstraction.outer_doc];

    method_ : Method.t
        [@aka ["m"]]
        [@name "method"]
        [@docv "METHOD"]
        [@enum Method.method_enum] [@default Method.Interleaving]
        [@doc Method.method_doc];

  } [@@deriving cmdliner]
end
