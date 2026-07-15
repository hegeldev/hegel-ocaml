(** Runtime registry and runner.

    The [ppx_hegel_test] PPX emits a registration call for every
    [let%hegel_test] definition. When a library opts in to the dune
    [(inline_tests (backend ppx_hegel_test))] stanza, dune synthesises an
    executable whose entry point calls [test_main], which iterates the
    registry, runs each test, prints pass/fail status, and exits non-zero
    if any test failed. You can also register and run tests manually. *)

(** Metadata captured by the PPX for a single registered test. *)
type test =
  { name : string
  ; file : string
  ; line : int
  ; run : unit -> unit
  }

(** [register ~name ~file ~line run] adds a test to the global registry. The
    PPX emits one call per [let%hegel_test] at module-init time. Tests are
    ordered by registration.

    You can register tests manually without the PPX. [run] is any [unit -> unit]
    that raises on failure, normally a wrapped {!Hegel.run_hegel_test}:

    {[
      let () =
        Hegel_test_runtime.register ~name:"addition_commutes" ~file:__FILE__ ~line:__LINE__
          (fun () ->
             Hegel.run_hegel_test (fun tc ->
               let a = Hegel.draw tc (Hegel.Generators.integers ()) in
               let b = Hegel.draw tc (Hegel.Generators.integers ()) in
               assert (a + b = b + a)))
    ]} *)
val register : name:string -> file:string -> line:int -> (unit -> unit) -> unit

(** [registered ()] returns the list of currently registered tests in registration
    order.

    {[
      List.iter
        (fun (t : Hegel_test_runtime.test) ->
           Printf.printf "%s (%s:%d)\n" t.name t.file t.line)
        (Hegel_test_runtime.registered ())
    ]} *)
val registered : unit -> test list

(** [run_all ()] runs every registered test, prints a per-test status line,
    and returns the number of failures.

    {[
      match Hegel_test_runtime.run_all () with
      | 0 -> print_endline "all tests passed"
      | n -> Printf.eprintf "%d test(s) failed\n" n
    ]} *)
val run_all : unit -> int

(** [test_main ()] runs every registered test via {!run_all} and terminates
    the process with exit code [0] on success or [1] on any failure. This is
    the entry point that dune's inline-tests runner invokes:

    {[
      let () = Hegel_test_runtime.test_main ()
    ]} *)
val test_main : unit -> 'a
