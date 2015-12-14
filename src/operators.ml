open Batteries

type _ one =
  | Neg : (int -> int) one
  | Not : (bool -> bool) one

let one_fun : type a b. (a -> b) one -> a -> b =
  function
  | Neg -> ( ~- )
  | Not -> ( not )

type _ two =
  | Add : (int -> int -> int) two
  | Sub : (int -> int -> int) two
  | Mul : (int -> int -> int) two
  | Div : (int -> int -> int) two
  | Eq : (int -> int -> bool) two
  | Neq : (int -> int -> bool) two
  | Lt : (int -> int -> bool) two
  | Gt : (int -> int -> bool) two
  | Le : (int -> int -> bool) two
  | Ge : (int -> int -> bool) two
  | And : (bool -> bool -> bool) two
  | Or : (bool -> bool -> bool) two

let two_fun : type a b c. (a -> b -> c) two -> a -> b -> c =
  let open Int in
  function
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div -> ( / )
  | Eq -> ( = )
  | Neq -> ( <> )
  | Lt -> ( < )
  | Gt -> ( > )
  | Le -> ( <= )
  | Ge  -> ( >= )
  | And -> ( && )
  | Or -> ( || )
