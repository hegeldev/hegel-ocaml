(** Hegel - Property-based testing for OCaml.

    Hegel is a universal property-based testing framework. This SDK communicates
    with the Hegel server (powered by Hypothesis) to generate test data and
    perform shrinking. *)

module Gen = Gen
module Cbor = Cbor

val run : ?test_cases:int -> ?hegel_path:string -> (unit -> unit) -> unit
(** Run a property-based test. The test function is called repeatedly with
    generated data. If any invocation raises an exception, Hegel will shrink
    the inputs and report the minimal failing case. *)

val assume : bool -> unit
(** [assume cond] rejects the current test input if [cond] is false.
    This does not count as a test failure; it simply skips the input. *)

val note : string -> unit
(** [note msg] records a message that will be displayed with any failing
    test case. Only prints on the final replay. *)

exception Assume_rejected
(** Raised internally when [assume false] is called. *)
