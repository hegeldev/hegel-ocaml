module Gen = Gen
module Cbor = Cbor

exception Assume_rejected = Gen.Assume_rejected

let run = Runner.run

let assume condition =
  if not condition then raise Assume_rejected

let note = Gen.note
let target = Gen.target
