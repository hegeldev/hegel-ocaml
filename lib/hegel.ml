module Gen = Gen
module Cbor = Cbor
module Protocol = Protocol
module Crc32 = Crc32
module State = State

exception Assume_rejected = Gen.Assume_rejected

let run = Runner.run
let find_hegel_path = Runner.find_hegel_path
let extract_channel_id = Runner.extract_channel_id

let assume condition =
  if not condition then raise Assume_rejected

let note = Gen.note
let target = Gen.target
