open Printf
open Syntax
open Error

let type_error e msg = Error [TypeError, e.startpos, e.endpos, msg]

let name_error e msg = Error [NameError, e.startpos, e.endpos, msg]

let label_already_defined e =
  name_error e (sprintf "Label %s already defined" e.item)

let not_defined e =
  name_error e (sprintf "%s not defined" e.item)

let check_var good_name good_vars bad_vars v =
  if List.mem v.item good_vars then v
  else
    if List.mem v.item bad_vars then
      raise (type_error v
        (sprintf "%s defined but wrong type: should be %s" v.item good_name))
    else
      raise (Error [
        NameError, v.startpos, v.endpos,
        sprintf "%s not defined: should be %s" v.item good_name])

let check_global globals locals = check_var "shared" globals locals
let check_local globals locals = check_var "register" locals globals

let check_label labels lbl =
  if List.mem lbl labels then lbl
  else raise (not_defined lbl)

let type_value globals locals v = match v.item with
  | Int _ -> v
  | Var r -> {v with item = Var (check_local globals locals r)}

let rec type_expr globals locals e =
  { e with item =
      match e.item with
        | Val x -> Val (type_value globals locals x)
        | Op (op, e1, e2) ->
            Op (
              op,
              type_expr globals locals e1,
              type_expr globals locals e2
            )}

let type_ins globals locals labels ins =
  let open Typed in {ins with item = match ins.item with
    | Untyped.Affect (x, e) ->
        if List.mem x.item globals
        then
          begin match e.item with
            | Val ({item = Int _; _} as n) -> Write (x, n)
            | Val {item = Var v; _} ->
                Write (x, {e with item = Var (check_local globals locals v)})
            | Op (_, _, _) -> raise (
              type_error e
                "Arithmetic operation not allowed in memory write")
          end
        else if List.mem x.item locals
        then
          begin match e.item with
            | Val {item = Var y; _} when List.mem y.item globals ->
                Read (x, y)
            | Val {item = _; _} | Op (_, _, _) ->
                RegOp (x, type_expr globals locals e)
          end
        else raise (not_defined x)
    | Untyped.Cmp (r, v1, v2) ->
        Cmp (check_local globals locals r,
             type_value globals locals v1,
             type_value globals locals v2)
    | Untyped.Mfence -> Mfence
    | Untyped.Label s -> Label s
    | Untyped.Jnz (r, s) ->
        Jnz (check_local globals locals r, (check_label labels s))
    | Untyped.Jz (r, s) ->
        Jz (check_local globals locals r, (check_label labels s))
    | Untyped.Jmp s ->
        Jmp (check_label labels s)
  }

let get_labels =
  let rec get_labels acc = function
    | [] -> acc
    | {item = Untyped.Label s} :: ins ->
        if List.mem s acc
        then raise (label_already_defined s)
        else get_labels (s :: acc) ins
    | _ :: ins -> get_labels acc ins
  in get_labels []

let type_thread globals thread =
  let open UntypedProgram in {
    TypedProgram.locals = thread.locals;
    ins = List.map
      (type_ins globals thread.locals (get_labels thread.ins)) thread.ins
  }

let type_program prog =
  let open UntypedProgram in {
    TypedProgram.initial = prog.initial;
    threads = List.map (type_thread (List.map fst prog.initial)) prog.threads;
  }
