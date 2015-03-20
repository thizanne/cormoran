open Batteries

module type Result = sig
  module Domain : Domain.Outer
  val data : Program.control_state -> Domain.t
end

module EmptyResult (D : Domain.Outer) : Result = struct
  module Domain = D
  let data _ = D.bottom
end
