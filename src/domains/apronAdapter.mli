module type Numerical = sig
  type t
  val manager : t Apron.Manager.t
end

module Make : functor (N : Numerical) -> Domain.Inner

module Polka : Domain.Inner

module Oct : Domain.Inner
