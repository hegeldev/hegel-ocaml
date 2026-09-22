@@ portable

(**
{2 Introduction}
Runs Hegel's concurrent state machines on Jane Street's [Concurrent] library.
This sublibrary is available on OxCaml only.

{!of_concurrent} wraps a [Concurrent.t] as a {!Hegel.Concurrency.t}. The caller
passes it to {!Hegel.Stateful.run_concurrent} or to the [run] of a
[module%hegel_concurrent_state_machine]:

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

A rule takes a {!ctx} containing the scheduler's per-task context and a
[Concurrent.t]. A machine declares the context type as [ctx]. 
For example, with a [Parallel] scheduler it is [Parallel_kernel.t]:

{[
module%hegel_concurrent_state_machine Counters = struct
  type ctx = Parallel_kernel.t Hegel_jane_concurrent.ctx

  let bump_twice _tc (ctx : ctx @ local) (n : int Atomic.t @ contended) =
    let #((), ()) =
      Parallel_kernel.fork_join2 ctx.context (fun _ -> Atomic.incr n) (fun _ -> Atomic.incr n)
    in
    ()
  [@@rule]
  ;;
end

let scheduler = Parallel_scheduler.scheduler ()

let%hegel_test counters tc =
  Parallel_scheduler.parallel scheduler (fun _kernel concurrent ->
    Counters.run
      tc
      ~concurrency:(Hegel_jane_concurrent.of_concurrent concurrent)
      ~init:(Atomic.make 0)
      ~max_concurrency:4 [@nontail])
;;
]}

{2 Interface}
*)

(** The context containing the scheduler's per-task value and a [Concurrent.t].
    Both are local to the step. *)
type 'a ctx =
  { context : 'a
  ; concurrent : 'a Concurrent.t
  }

(** [of_concurrent c] runs each round's workers as tasks of [c]. *)
val of_concurrent : 'a Concurrent.t @ local -> 'a ctx Hegel.Concurrency.t @ local
