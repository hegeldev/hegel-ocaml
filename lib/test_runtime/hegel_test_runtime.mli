(** Runtime registry and runner for [let%hegel_test] tests.

    The [ppx_hegel_test] PPX emits a registration call for every
    [let%hegel_test] definition. When a library opts in to the dune
    [(inline_tests (backend ppx_hegel_test))] stanza, dune synthesises an
    executable whose entry point calls {!test_main}, which iterates the
    registry, runs each test, prints pass/fail status, and exits non-zero
    if any test failed. *)

(** Metadata captured by the PPX for a single registered test. *)
type test =
  { name : string
  ; file : string
  ; line : int
  ; run : unit -> unit
  }

(** [register ~name ~file ~line run] adds a test to the global registry. The
    PPX emits one call per [let%hegel_test] at module-init time; tests are
    ordered by registration. *)
val register : name:string -> file:string -> line:int -> (unit -> unit) -> unit

(** [registered ()] returns the list of currently registered tests in source
    order. *)
val registered : unit -> test list

(** [run_all ()] runs every registered test, prints a per-test status line,
    and returns the number of failures. Exceptions raised by a test are
    caught and counted; subsequent tests still run. *)
val run_all : unit -> int

(** [test_main ()] runs every registered test via {!run_all} and terminates
    the process with exit code [0] on success or [1] on any failure. This is
    the entry point that dune's generated inline-tests runner invokes. *)
val test_main : unit -> 'a
