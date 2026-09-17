(** How a concurrent stateful test runs its workers.

    A concurrent state machine (see [Stateful.run_concurrent]) runs each round
    by passing one body per worker. [spawn_join_n] must run all of them at the
    same time and return their outcomes once every body has finished. The
    library comes with {!threads} by default. *)

(** The result of one worker body: [None] on success, or the exception it
    raised with its backtrace. *)
type outcome = (exn * Printexc.raw_backtrace) option

(** A concurrency capability. *)
type t =
  { spawn_join_n : n:int -> f:(int -> outcome) -> outcome list
    (** [spawn_join_n ~n ~f] runs [f 0] … [f (n - 1)] as concurrent tasks and
        returns their outcomes once all of them have finished. A worker may block
        until another worker runs. If [f i] raises, the exception is recorded
        as task [i]'s outcome. *)
  }

(** Runs each body on its own systhread. *)
val threads : t
