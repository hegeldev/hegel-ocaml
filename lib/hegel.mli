(** Hegel - Property-based testing for OCaml.

    Hegel is a universal property-based testing framework. This SDK communicates
    with the Hegel server (powered by Hypothesis) to generate test data and
    perform shrinking. *)

module Gen = Gen
module Cbor = Cbor
module Protocol = Protocol
module Crc32 = Crc32
module State = State

val run : ?test_cases:int -> ?hegel_path:string -> (unit -> unit) -> unit
(** Run a property-based test. The test function is called repeatedly with
    generated data. If any invocation raises an exception, Hegel will shrink
    the inputs and report the minimal failing case. *)

val find_hegel_path : unit -> string option
(** Search PATH for the hegel binary. Returns [None] if not found. *)

val extract_channel_id : Cbor.t -> int
(** Extract the channel ID from a CBOR event map. Raises [Failure] if missing or invalid. *)

val assume : bool -> unit
(** [assume cond] rejects the current test input if [cond] is false.
    This does not count as a test failure; it simply skips the input. *)

val note : string -> unit
(** [note msg] records a message that will be displayed with any failing
    test case. Only prints on the final replay. *)

val target : ?label:string -> float -> unit
(** [target value ~label] guides the test engine toward higher values
    of a numeric metric. The engine will try to maximize the target value,
    which can help find edge cases. *)

exception Assume_rejected
(** Raised internally when [assume false] is called. *)
