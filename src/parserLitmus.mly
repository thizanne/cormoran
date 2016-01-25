%{
  open Lexing

  module Ty = Types
  module T = TypedAst
  module L = Location

  let var_sym = Sym.namespace ()

  let seq_loc b1 b2 =
    L.mk
      (T.Seq (b1, b2))
      L.(b1.loc.startpos)
      L.(b2.loc.endpos)

  let rec transform = function
    | [] -> failwith "transform"
    | [line] ->
      List.map
        (function
          | None -> L.mkdummy T.Nothing
          | Some i -> i)
        line
    | line :: lines ->
      let bodies = transform lines in
      List.map2
        (fun ins body -> match ins with
           | None -> body
           | Some ins -> seq_loc ins body)
        line bodies

  let add_preserve k v map =
    Sym.Map.modify_opt
      k (function None -> Some v | Some v' -> Some v') map

  let local_in_expr :
  type t. t T.program_expression -> Sym.t list =
    fun expression ->
    T.fold_expr
      { T.fold =
          fun var acc ->
          if T.is_local var then var.T.var_sym :: acc else acc }
      [] expression

  let shared_in_expr :
  type t. t T.program_expression -> Sym.t list =
    fun expression ->
    T.fold_expr
      { T.fold =
          fun var acc ->
          if T.is_shared var then var.T.var_sym :: acc else acc }
      [] expression

  let rec get_local { L.item = body; _ } = match body with
    | T.Nothing
    | T.Pass
    | T.MFence -> Sym.Map.empty
    | T.Seq (b1, b2) ->
       Sym.Map.merge (* Does an union *)
         (fun _key _ty_int1 _ty_int2 -> Some Env.Int)
         (get_local b1)
         (get_local b2)
    | T.Assign (x, e) ->
      if T.is_local x.L.item
      then Sym.Map.singleton x.L.item.T.var_sym Env.Int
      else begin
        match local_in_expr e.L.item with
        | [y] -> Sym.Map.singleton y Env.Int
        | _ -> Sym.Map.empty
      end
    | _ -> failwith "ParserLitmus.get_local"

  let get_shared acc threads =
    (* Finds all shared variables of the program, including those
       which were not initialised (meaning they should be initialised
       to 0, according to litmus specification). Relies on the fact
       that litmus programs only contain sequences of
       assignations/fences (and enforces it with an exception) *)
    let rec scan_body body acc = match body with
      | T.Nothing
      | T.Pass
      | T.MFence -> acc
      | T.Seq (b1, b2) ->
        acc |> scan_body b1.L.item |> scan_body b2.L.item
      | T.Assign (x, e) ->
        if T.is_shared x.L.item
        then add_preserve x.L.item.T.var_sym (T.ConstInt 0) acc
        else begin
          match shared_in_expr e.L.item with
          | [y] -> Sym.Map.add y (T.ConstInt 0) acc
          | _ -> failwith "ParserLitmus.globals"
        end
      | _ -> failwith "ParserLitmus.get_shared"
    in
    List.fold_left
      (fun acc thread -> scan_body thread.T.body.L.item acc)
      acc threads

  let var var_sym var_spec =
    { T.var_sym; var_type = Ty.Int; var_spec }

%}

%token LCurly RCurly LPar RPar
%token (* Colon *) Comma Semi Pipe And Equals Mov MFence Exists
%token Eof
%token <string> Shared
%token <int> Int
%token <string> Reg
%token <int * string> ThreadedReg

%start <TypedAst.program * Property.t list> program

%%

%inline loc(X) :
| x = X { L.mk x $startpos $endpos }

%inline shared_sym :
| x = Shared { var_sym x }

%inline register_sym :
| r = Reg { var_sym r }

program :
| init = init_dec threads = code properties = exists Eof {
    {
      T.initial = get_shared init threads;
      threads;
    },
    properties
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
    Sym.Map.of_enum @@ BatList.enum @@ li
  }

init_var :
| v = shared_sym Equals x = Int { v, T.ConstInt x }

code :
| lines = line+ {
    List.map
      (fun body ->
         {
           T.locals = get_local body;
           body;
         })
      (transform lines)
  }

line :
| ins = separated_nonempty_list(Pipe, loc(instruction)?) Semi { ins }

instruction :
| MFence { T.MFence }
| Mov x = loc(var) Comma e = loc(expr) { T.Assign (x, e) }

expr :
| n = loc(Int) { T.Int n }
| x = loc(var) { T.Var x }

var :
| r = register_sym { var r Ty.Local }
| x = shared_sym { var x Ty.Shared }

exists :
| Exists LPar condition = condition RPar {
    [{ Property.zone = None; condition }]
  }

condition :
| { L.mkdummy @@ T.Bool (L.mkdummy true) }
| eq = loc(equality) { eq }
| eq = loc(equality) And eqs = condition {
    L.mkdummy @@
    T.Binop (
      L.mkdummy T.Or,
      eq,
      eqs
    )
  }

equality :
| var = loc(sourced_var) Equals n = loc(Int) {
    (* Litmus properties are existential ones. As the verification
       works with universal properties, we negate them and check their
       opposite. Thus safe tests should have their properties verified
       if the analysis is precise, and relaxed tests should not have
       their properties verified if the analysis is sound. *)
    T.Binop (
      L.mkdummy @@ T.Neq,
      L.mkdummy @@ T.Var var,
      L.mkdummy @@ T.Int n
    )
  }

sourced_var :
| x = shared_sym { var x Source.Memory  }
| tid_var = ThreadedReg {
    let thread_id, var_name = tid_var in
    var (var_sym var_name) (Source.Local thread_id)
  }
