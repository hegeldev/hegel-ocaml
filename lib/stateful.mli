(** A stateful test exercises a system through a sequence of randomly chosen
    actions ("rules") applied to a state. Rules are constructed with
    {!Rule.create} from a [name] and a [step] function that performs one
    application of the rule — drawing any arguments it needs from the test case
    and returning the new state. Invariants are ['state -> unit] functions
    evaluated before any step is run and after every successful step.

    To run a state machine, call {!run} inside a Hegel test. Examples in this
    documentation assume [open Hegel].

    Example: an integer stack.

    {[
    let push =
      Stateful.Rule.create ~name:"push" ~step:(fun tc stack ->
          let n =
            draw tc (Generators.integers ~min_value:0 ~max_value:100 ())
          in
          n :: stack)

    let pop =
      Stateful.Rule.create ~name:"pop" ~step:(fun tc stack ->
          assume tc (not (List.is_empty stack));
          List.tl stack)

    let%hegel_test test_integer_stack tc =
      Stateful.run ~init:[] ~rules:[ push; pop ] tc
    ]} *)

module Pool : sig
  (** A typed handle for a per-test case pool of variables. *)
  type 'a t

  (** Creates an empty {!Pool.t}. Pools are tied to a test case; do not
      reuse one across test cases. *)
  val create : Internal.test_case -> 'a t

  (** Records [value] in [variables] for later draws.

      {[
      let n = draw tc (Generators.integers ~min_value:0 ~max_value:100 ()) in
      Stateful.Pool.add pool n
      ]} *)
  val add : 'a t -> 'a -> unit

  (** Returns the number of variables in the pool.

      {[
      assume tc (Stateful.Pool.size pool > 0)
      ]} *)
  val size : _ t -> int

  (** Create an unprintable generator that returns a variable from the [pool] without removing it.
      Calls [assume false] if the [pool] is empty.

      {[
      let existing = draw_silent tc (Stateful.Pool.values_reusable pool)
      ]} *)
  val values_reusable : 'a t -> ('a, Generators.unprintable) Generators.generator

  (** Create an unprintable generator that removes and returns a variable from the [pool].
      Calls [assume false] if the [pool] is empty.

      {[
      let taken = draw_silent tc (Stateful.Pool.values_consumed pool)
      ]} *)
  val values_consumed : 'a t -> ('a, Generators.unprintable) Generators.generator
end

module Rule : sig
  (** A rule is one possible action in a stateful test. *)
  type 'state t

  (** Declares a rule.

      - [name] is printed in the final output when the rule is run
      - [step tc state] performs one application of the rule, drawing any
        arguments it needs from [tc] and returning the new state.

      {[
      let push =
        Stateful.Rule.create ~name:"push" ~step:(fun tc stack ->
            let n = draw tc (Generators.integers ~min_value:0 ~max_value:100 ()) in
            n :: stack)
      ]} *)
  val create : name:string -> step:(Internal.test_case -> 'state -> 'state) -> 'state t

  (** Returns the name of the rule.

      {[
      let label = Stateful.Rule.name push
      ]} *)
  val name : _ t -> string
end

(** Executes a stateful test by repeatedly applying randomly chosen [rules] to a
    state threaded from [init], checking each of the [invariants] before the
    first step and after every successful step. Raises [Invalid_argument] if
    [rules] is empty. *)
val run
  :  init:'state
  -> rules:'state Rule.t list
  -> ?invariants:('state -> unit) list
  -> Internal.test_case
  -> unit
