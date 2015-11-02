open Batteries

type arith_one =
  | Neg

let arith_one_fun = function
  | Neg -> ( ~- )

type logic_one =
  | Not

let logic_one_fun = function
  | Not -> ( not )

type arith_two =
  | Add
  | Sub
  | Mul
  | Div

let arith_two_fun = function
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div -> ( / )

type logic_arith_two =
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge

let logic_arith_two_fun =
  let open Int in
  function
  | Eq -> ( = )
  | Neq -> ( <> )
  | Lt -> ( < )
  | Gt -> ( > )
  | Le -> ( <= )
  | Ge  -> ( >= )

type logic_two =
  | And
  | Or

let logic_two_fun = function
  | And -> ( && )
  | Or -> ( || )
