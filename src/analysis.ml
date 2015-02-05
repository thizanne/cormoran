module type Result = sig
  module Domain : Domain.Domain
  val data : Syntax.position -> Domain.t
end
