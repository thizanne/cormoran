module type Result = sig
  module Domain : Domain.Domain
  val data : Syntax.position -> Domain.t
end

module EmptyResult (D : Domain.Domain) : Result = struct
  module Domain = D
  let data _ = D.empty
end
