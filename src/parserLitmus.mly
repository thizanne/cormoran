%{
  open Lexing

  module P = Program
  module L = Location

  let var_sym = Symbol.namespace ()

  let rec transform = function
    | [] -> failwith "transform"
    | [line] ->
      List.map
        (function
          | None -> L.mkdummy P.Nothing
          | Some i -> i)
        line
    | line :: lines ->
      let bodies = transform lines in
      List.map2
        (fun ins body -> match ins with
           | None -> body
           | Some ins -> P.seq ins body)
        line bodies

  let add_preserve k v map =
    Symbol.Map.modify_opt
      k (function None -> Some v | Some v' -> Some v') map

  let get_shared acc threads =
    (* Finds all shared variables of the program, including those
       which were not initialised (meaning they should be initialised
       to 0, according to litmus specification). Relies on the fact
       that litmus programs only contain sequences of
       assignations/fences (and enforces it with an exception) *)
    let rec scan_body body acc = match body with
      | P.MFence -> acc
      | P.Seq (b1, b2) ->
        acc |> scan_body b1.L.item |> scan_body b2.L.item
      | P.Assign (x, e) ->
        if P.is_shared x.L.item
        then add_preserve x.L.item.P.var_name 0 acc
        else begin
          match P.shared_in_expr e.L.item with
          | [y] -> Symbol.Map.add y 0 acc
          | _ -> failwith "ParserLitmus.globals"
        end
      | _ -> failwith "ParserLitmus.get_shared"
    in
    List.fold_left
      (fun acc thread -> scan_body thread.P.body acc)
      acc threads
%}

%token LCurly RCurly LPar RPar
%token (* Colon *) Comma Semi Pipe And Equals Mov MFence Exists
%token Eof
%token <string> Shared
%token <int> Int
%token <string> Reg
%token <int * string> ThreadedReg

%start <Program.t> program

%%

%inline loc(X) :
| x = X { L.mk x $startpos $endpos }

program :
| init = init_dec c = code prop = exists Eof {
    { P.initial = get_shared init c; threads = c }
  }
| error {
    let open Error in
    let err_loc = { L.startpos = $startpos; endpos = $endpos } in
    raise @@ Error { error = SyntaxError; err_loc; err_msg = "" }
  }

init_dec :
  (* Due to a hack with LexerLitmus.drop_prelude, LCurly is actually
     dropped *)
| LCurly? li = separated_list(Semi, init_var) RCurly {
    Symbol.Map.of_enum @@ BatList.enum @@ li
  }

init_var :
| v = Shared Equals x = Int { var_sym v, x }

code :
| lines = line+ {
    List.map
      (fun { L.item = body; _ } ->
         { P.locals = Symbol.Set.empty; body })
      (transform lines)
  }

line :
| ins = separated_nonempty_list(Pipe, loc(instruction)?) Semi { ins }

instruction :
| MFence { P.MFence }
| Mov x = loc(var) Comma e = loc(expr) { P.Assign (x, e) }

expr :
| n = loc(Int) { P.Int n }
| x = loc(var) { P.Var x }

var :
| r = Reg { P.local_var (var_sym r) }
| x = Shared { P.shared_var (var_sym x) }

exists :
| Exists LPar eqs = separated_list(And, equality) RPar { eqs }

equality :
| v = threaded_var Equals x = Int { (v, x) }

(* TODO: real property checking *)
threaded_var :
| Shared { }
| ThreadedReg { }
