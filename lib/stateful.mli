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

    let%hegel_test test_integer_stack tc =
      Stateful.run ~init:[] ~rules:[ push; pop ] tc
    ]} *)

module Pool : sig
  (** A typed handle for a per-test case pool of variables. *)
  type 'a t

  (** Creates an empty {!Pool.t}. Pools are tied to a test case; do not
      reuse one across test cases. When [sexp_of] is given, each drawn/consumed
      variable is printed as [v<id> = <sexp>] on the final replay of a failing
      test; without it, variable picks print nothing. *)
  val create : Client.test_case -> 'a t

  (** Records [value] in [variables] for later draws. *)
  val add : 'a t -> 'a -> unit

  (** Returns the number of variables in the pool. *)
  val size : _ t -> int

  (** Returns [true] if no variables are in the pool *)
  val is_empty : _ t -> bool

  (** Create an unprintable generator that returns a variable from the [pool] without removing it. 
      Calls [assume false] if the [pool] is empty. *)
  val values_reusable : 'a t -> ('a, Generators.unprintable) Generators.generator

  (** Create an unprintable generator that removes and returns a variable from the [pool]. 
      Calls [assume false] if the [pool] is empty. *)
  val values_consumed : 'a t -> ('a, Generators.unprintable) Generators.generator
end

module Rule : sig
  (** A rule is one possible action in a stateful test. *)
  type 'state t

  (** Declares a rule.

      - [name] is printed in the final output when the rule is run
      - [step tc state] performs one application of the rule, drawing any
        arguments it needs from [tc] and returning the new state. *)
  val create : name:string -> step:(Client.test_case -> 'state -> 'state) -> 'state t

  val name : _ t -> string
end

(** Executes a stateful test by repeatedly applying random rules and checking
    invariants. *)
val run
  :  init:'state
  -> rules:'state Rule.t list
  -> ?invariants:('state -> unit) list
  -> Client.test_case
  -> unit
