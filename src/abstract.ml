open Apron
open Batteries
open Format
open Syntax
open Syntax.Typed

module M = Map.Make
    (struct
      type t = Var.t list list
      let compare = List.compare @@ List.compare @@ Var.compare
    end)

type t = Polka.loose Polka.t Abstract1.t M.t

let man = Polka.manager_alloc_loose ()

let var v =
  Var.of_string v.item

let binop c =
  let open Texpr0 in
  List.assoc c ['+', Add; '-', Sub; '*', Mul; '/', Div]

let texpr_val = function
  | Int n -> Texpr1.Cst (Coeff.s_of_int n.item)
  | Var v -> Texpr1.Var (Var.of_string v.item)

let texpr env expr =
  let rec aux = function
    | Val v -> texpr_val v.item
    | Op (op, e1, e2) ->
      Texpr1.Binop (
        binop op.item,
        aux e1.item, aux e2.item,
        Texpr1.Int, Texpr1.Zero
      )
  in Texpr1.of_expr env (aux expr)

let transfer d t ins =
  let env = Map.choose d |> snd |> Abstract1.env in
  match ins with
  | Pass
  | Label _
  | Jmp _ -> d
  | MFence ->
    Map.filter (fun bufs _ -> List.nth bufs t = []) d
  | RegOp (r, e) ->
    Map.map
      (fun abstr ->
         Abstract1.assign_texpr man abstr (var r)
           (texpr env e.item) None) d
  | Cmp (r0, v1, v2) ->
    let earray () = Tcons1.array_make env 1 in
    let suparray, infarray, eqarray = earray (), earray(), earray() in
    let tcons op =
      sprintf "%s %c %s"
        (string_of_value v1.item) op (string_of_value v2.item)
      |> Parser.tcons1_of_string env in
    let affect n abstr =
      Abstract1.assign_texpr man abstr (Var.of_string r0.item)
        (Texpr1.cst env (Coeff.s_of_int n)) None in
    begin
      Tcons1.array_set suparray 0 (tcons '>');
      Tcons1.array_set infarray 0 (tcons '<');
      Tcons1.array_set eqarray 0 (tcons '=');
      Map.map
        (fun abstr -> Abstract1.join_array man [|
             Abstract1.meet_tcons_array man abstr suparray
             |> affect 1;
             Abstract1.meet_tcons_array man abstr infarray
             |> affect (-1);
             Abstract1.meet_tcons_array man abstr eqarray
             |> affect 0;
           |]) d
    end
  | Read (r, x) -> failwith "TODO"
  | Write (x, v) -> failwith "TODO"
  | Jnz (r, _) -> failwith "TODO"
  | Jz (r, _) -> failwith "TODO"
