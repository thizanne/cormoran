open Batteries

module type Result = sig
  module Domain : Domain.Outer
  val data : Program.Control.State.t -> Domain.t
end

module EmptyResult (D : Domain.Outer) : Result = struct
  module Domain = D
  let data _ = D.bottom
end
