(** How hegel waits on a body that returns ['a t]. *)
module type S = sig
  type 'a t

  (** A test body, rule step, or invariant, already applied to its
      arguments. *)
  type body = unit -> unit t

  (** Runs a body and waits for it to finish. *)
  type wait = body -> unit

  (** [run_loop loop] runs the engine loop where it can block, passing it
      [wait]. *)
  val run_loop : (wait -> unit) -> unit t
end

(** [Make (B)] is {!Hegel.run_hegel_test} and the sequential {!Hegel.Stateful}
    API for bodies that return [unit B.t]. *)
module Make (B : S) : sig
  (** Same as {!Hegel.run_hegel_test}, for a body returning [unit B.t]. *)
  val run_hegel_test
    :  ?settings:Settings.t
    -> ?test_location:Internal.test_location
    -> ?database_key:string
    -> ?failure_blobs:string list
    -> (Internal.test_case -> unit B.t)
    -> unit B.t

  (**/**)

  (** Same as {!Hegel.run_hegel_test_ppx}. The [let%hegel_test] PPX targets
      it. *)
  val run_hegel_test_ppx
    :  ?settings:Settings.t
    -> ?test_location:Internal.test_location
    -> ?database_key:string
    -> ?failure_blobs:string list
    -> (Internal.test_case -> unit B.t)
    -> unit B.t

  (**/**)

  (** The sequential {!Hegel.Stateful} API, for rules and invariants returning
      [unit B.t]. *)
  module Stateful : sig
    (** Same as {!Hegel.Stateful.Pool}. *)
    module Pool = Stateful.Pool

    (** One possible action in a sequential stateful test. See
        {!Hegel.Stateful.Rule}. *)
    module Rule : sig
      type 'state t

      (** Same as {!Hegel.Stateful.Rule.create}, for a [step] returning
          [unit B.t]. *)
      val create
        :  name:string
        -> ?weight:float
        -> step:(Internal.test_case -> 'state -> unit B.t)
        -> unit
        -> 'state t

      (** Returns the name of the rule. *)
      val name : _ t -> string

      (** Returns the weight of the rule. *)
      val weight : _ t -> float
    end

    (** A property that must always be true in a stateful test. See
        {!Hegel.Stateful.Invariant}. *)
    module Invariant : sig
      type 'state t

      (** Same as {!Hegel.Stateful.Invariant.create}, for an [inv] returning
          [unit B.t]. *)
      val create
        :  name:string
        -> inv:(Internal.test_case -> 'state -> unit B.t)
        -> ?always_check:bool
        -> unit
        -> 'state t

      (** Returns the name of the invariant. *)
      val name : _ t -> string
    end

    (** A sequential state machine. *)
    module type State_machine = sig
      type state

      val rules : state Rule.t list
      val invariants : state Invariant.t list
    end

    (** Same as {!Hegel.Stateful.run}. Each rule and invariant finishes before
        the next one starts. *)
    val run
      :  ?step_count:int
      -> ?sexp_of_state:('state -> Sexplib0.Sexp.t)
      -> Internal.test_case
      -> (module State_machine with type state = 'state)
      -> init:'state
      -> unit B.t

    (**/**)

    (** Same as {!Hegel.Stateful.run_internal}. *)
    val run_internal
      :  init:'state
      -> rules:'state Rule.t list
      -> invariants:'state Invariant.t list
      -> ?sexp_of_state:('state -> Sexplib0.Sexp.t)
      -> ?step_count:int
      -> Internal.test_case
      -> unit B.t

    (**/**)
  end
end
