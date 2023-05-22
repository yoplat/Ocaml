open Comparable
open IntervalI

module Interval (Endpoint : Comparable) :
  IntervalI with type endpoint = Endpoint.t = struct
  type endpoint = Endpoint.t
  type interval = Interval of Endpoint.t * Endpoint.t | Empty

  exception WrongInterval
end
