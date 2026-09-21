@@ portable

(**
{2 Introduction} 
Runs Hegel's concurrent state machines on Jane Street's [Concurrent] library.
This sublibrary is available on OxCaml only.

The caller passes a wrapped capability to {!Hegel.Stateful.run_concurrent} or
to the [run] of a [module%hegel_concurrent_state_machine]:

{[
let%hegel_test counter tc =
  Concurrent_in_thread.with_blocking Await.Terminator.unkillable ~f:(fun concurrent ->
    Counter.run
      tc
      ~concurrency:(Hegel_jane_concurrent.of_concurrent concurrent)
      ~init:(Atomic.make 0)
      ~max_concurrency:4 [@nontail])
;;
]}

{2 Interface}
*)

(** [of_concurrent c] runs each round's workers as tasks of [c]. *)
val of_concurrent : _ Concurrent.t @ local -> Hegel.Concurrency.t @ local
