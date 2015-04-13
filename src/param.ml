open Batteries
open Cmdliner

module Parse = struct
  let parse_litmus lexbuf =
    LexerLitmus.drop_prelude lexbuf;
    ParserLitmus.program LexerLitmus.lexer lexbuf
    |> Typing.type_program,
    []

  let parse_imp lexbuf =
    Parser.program Lexer.lexer lexbuf
    |> Typing.type_program,
    []

  let parse_lexbuf use_litmus =
    if use_litmus
    then parse_litmus
    else parse_imp

  let get_lexbuf filename =
    (* filename is expected to be valid *)
    let open Lexing in
    let lexbuf = from_channel @@ open_in filename in
    lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = filename };
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    lexbuf

  let parse_filename use_litmus filename =
    parse_lexbuf use_litmus @@ get_lexbuf filename
end

module Domain = struct
  type t =
    | Polka
    | Oct
    | Top
    | Order
    | Concrete

  let get : _ -> (module Domain.Outer) = function
    | Polka -> (module Abstract.Make (ApronAdapter.Polka))
    | Oct -> (module Abstract.Make (ApronAdapter.Oct))
    | Top -> (module Top)
    | Order -> (module Abstract.Make (InnerConcrete))
    | Concrete -> (module Concrete)
end

module CommandTerm = struct
  let domain =
    let domains = Domain.[
      "polka", Polka;
      "oct", Oct;
      "top", Top;
      "order", Order;
      "concrete", Concrete;
    ] in
    let alts = Arg.doc_alts_enum domains in
    let doc = Printf.sprintf "The domain to use. $(docv) must be %s" alts in
    Arg.(value & opt (enum domains) Domain.Polka &
         info ["d"; "domain"] ~doc ~docv:"DOMAIN")

  let widening_delay =
    let doc =
      "The number of computation steps before widening. Negative is zero." in
    Arg.(value & opt int 0 & info ["w"; "wdelay"] ~doc ~docv:"N")

  let use_litmus =
    let doc = "Use litmus syntax." in
    Arg.(value & flag & info ["litmus"] ~doc)

  let filename =
    let doc = "The program to analyse." in
    Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc ~docv:"FILE")
end
