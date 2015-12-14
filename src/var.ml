open Batteries

module Ty = Types

type 't t = {
  var_type : 't Ty.t;
  var_sym : Sym.t;
}
