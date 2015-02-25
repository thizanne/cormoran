open Apron
open Batteries

type typ =
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge

let fun_of_typ =
  let open Int in function
  | Eq -> ( = )
  | Neq -> ( <> )
  | Lt -> ( < )
  | Le -> ( <= )
  | Gt -> ( > )
  | Ge  -> ( >= )

type t = {
  typ : typ;
  e1 : Expression.t;
  e2 : Expression.t;
}

let make typ e1 e2 =
  { typ; e1; e2 }

let to_tcons env { typ; e1; e2 } =
  let expr e e' =
    (Expression.to_texpr env (Expression.sub e e')) in
  match typ with
  | Eq -> Tcons1.make (expr e1 e2) Tcons1.EQ
  | Neq -> Tcons1.make (expr e1 e2) Tcons1.DISEQ
  | Lt -> Tcons1.make (expr e2 e1) Tcons1.SUP
  | Le -> Tcons1.make (expr e2 e1) Tcons1.SUPEQ
  | Gt -> Tcons1.make (expr e1 e2) Tcons1.SUP
  | Ge -> Tcons1.make (expr e1 e2) Tcons1.SUPEQ
