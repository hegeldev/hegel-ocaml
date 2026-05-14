(** A stateful test exercises a system through a sequence of randomly chosen
    actions ("rules") applied to a state. Rules are constructed with
    {!Rule.create} from a [name] and a [step] function that performs one
    application of the rule — drawing any arguments it needs from the test case
    and returning the new state. Invariants are ['state -> unit] functions
    evaluated after every successful step.

    To run a state machine, call {!run} inside a Hegel test.

    Example: an integer stack.

    {[
    open Hegel

    let push =
      Stateful.Rule.create ~name:"push" ~step:(fun tc stack ->
          let n =
            draw tc (Generators.integers ~min_value:0 ~max_value:100 ())
          in
          n :: stack)

    let pop =
      Stateful.Rule.create ~name:"pop" ~step:(fun tc stack ->
          Client.assume tc (not (List.is_empty stack));
          List.tl stack)

    let test_integer_stack () =
      run_hegel_test (fun tc -> Stateful.run ~init:[] ~rules:[ push; pop ] tc)
    ]} *)

module Variables : sig
  type 'a t
  (** A typed handle for a per-test case pool of variables. *)

  val create : Client.test_case -> 'a t
  (** Creates an empty {!Variables.t}. Variables are tied to a test case; do not
      reuse one across test cases. *)

  val add : 'a t -> 'a -> unit
  (** Records [value] in [variables] for later draws. *)

  val size : _ t -> int
  (** Returns the number of variables in the pool. *)

  val is_empty : _ t -> bool
  (** Returns [true] if no variables are in the pool *)

  val draw : 'a t -> 'a
  (** Draws a variable from the [variables] without removing it. Calls
      [assume false] if the [variables] is empty. *)

  val consume : 'a t -> 'a
  (** Removes and returns a variable from the [variables]. Calls [assume false]
      if the [variables] is empty. *)
end

module Rule : sig
  type 'state t
  (** A rule is one possible action in a stateful test. *)

  val create :
    name:string -> step:(Client.test_case -> 'state -> 'state) -> 'state t
  (** Declares a rule.

      - [name] is printed in the final output when the rule is run
      - [step tc state] performs one application of the rule, drawing any
        arguments it needs from [tc] and returning the new state. *)

  val name : _ t -> string
end

module Settings : sig
  type t = { max_steps : int  (** default: 50 *) }

  val default : t
end

val run :
  ?settings:Settings.t ->
  init:'state ->
  rules:'state Rule.t list ->
  ?invariants:('state -> unit) list ->
  Client.test_case ->
  unit
(** Executes a stateful test by repeatedly applying random rules and checking
    invariants. *)
