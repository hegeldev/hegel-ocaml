(** Conformance test helper library for Hegel.

    This module provides utilities for conformance test binaries:
    - {!get_test_cases}: reads [CONFORMANCE_TEST_CASES] env var
    - {!write_metrics}: appends a JSON line to [CONFORMANCE_METRICS_FILE] *)

open! Core

(** [parse_test_cases s] parses an optional string to an integer test count,
    defaulting to 50 if [s] is [None] or not a valid integer. *)
let parse_test_cases = function
  | None -> 50
  | Some s -> Option.value ~default:50 (Int.of_string_opt s)

(** [get_test_cases ()] returns the number of test cases to run. Reads
    [CONFORMANCE_TEST_CASES] env var; defaults to [50]. *)
let get_test_cases () = parse_test_cases (Sys.getenv "CONFORMANCE_TEST_CASES")

(** [format_metrics pairs] formats a list of [(key, value)] pairs as a JSON
    object line (including newline). Values must already be serialized as JSON
    (e.g. ["true"], ["42"], ["-3.14"]). *)
let format_metrics pairs =
  let json_body =
    String.concat ~sep:", "
      (List.map pairs ~f:(fun (k, v) -> sprintf "%S: %s" k v))
  in
  sprintf "{%s}\n" json_body

(** [write_metrics_to filename pairs] appends a JSON metrics line to [filename].
*)
let write_metrics_to filename pairs =
  let oc =
    Stdlib.open_out_gen [ Stdlib.Open_append; Stdlib.Open_creat ] 0o644 filename
  in
  Exn.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    ~f:(fun () -> Stdlib.output_string oc (format_metrics pairs))

(** [write_metrics metrics] appends a JSON line to the file indicated by the
    [CONFORMANCE_METRICS_FILE] env var. [metrics] is a list of [(key, value)]
    pairs where values are already serialized JSON strings (including quotes for
    strings, no quotes for numbers/booleans/null). Raises [Failure] if the env
    var is not set. *)
let write_metrics pairs =
  match Sys.getenv "CONFORMANCE_METRICS_FILE" with
  | None -> failwith "CONFORMANCE_METRICS_FILE env var not set"
  | Some filename -> write_metrics_to filename pairs

(** [with_metrics body] runs [body ()] and writes its result as a single metrics
    line. If [body] raises, an empty metrics line is written and the exception
    is re-raised.

    The hegel server writes one metric line per test case it dispatches
    (including invalid and overrun cases). To keep the client and server metric
    counts aligned, conformance binaries should wrap their per-test body with
    this helper. *)
let with_metrics body =
  match body () with
  | metrics -> write_metrics metrics
  | exception exn ->
      write_metrics [];
      raise exn
