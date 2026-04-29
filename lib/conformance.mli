(** Conformance test helper library for Hegel.

    This module provides utilities for conformance test binaries. *)

val parse_test_cases : string option -> int
(** [parse_test_cases s] parses an optional string to an integer test count,
    defaulting to 50 if [s] is [None] or not a valid integer. *)

val get_test_cases : unit -> int
(** [get_test_cases ()] returns the number of test cases to run. Reads
    [CONFORMANCE_TEST_CASES] env var; defaults to [50]. *)

val format_metrics : (string * string) list -> string
(** [format_metrics pairs] formats a list of [(key, value)] pairs as a JSON
    object line (including newline). Values must already be serialized as JSON.
*)

val write_metrics_to : string -> (string * string) list -> unit
(** [write_metrics_to filename pairs] appends a JSON metrics line to [filename].
*)

val write_metrics : (string * string) list -> unit
(** [write_metrics metrics] appends a JSON line to the file indicated by the
    [CONFORMANCE_METRICS_FILE] env var. [metrics] is a list of [(key, value)]
    pairs where values are already serialized JSON strings. Raises [Failure] if
    the env var is not set. *)
