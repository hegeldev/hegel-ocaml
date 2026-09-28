@@ portable

(** Runs Hegel's concurrent state machines on Jane Street's [Concurrent]
    library. See {{!page-hegel_jane_concurrent}Hegel_jane_concurrent} for a
    guide, examples, and the interface. *)

type 'a ctx =
  { context : 'a
  ; concurrent : 'a Concurrent.t
  }

val of_concurrent : 'a Concurrent.t @ local -> 'a ctx Hegel.Concurrency.t @ local
