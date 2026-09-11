(** {2 Introduction}
    A stateful test applies a random sequence of rules to a state. A rule is a
    [step] function that takes the test case and the current state, draws
    whatever data it needs, and returns the new state. An invariant is a property
    that must always hold after each step.

    With the [ppx_hegel_test] PPX, a state machine is a module written as
    [module%hegel_state_machine M = struct … end]. Mark rules with [[@@rule]]
    and invariants with [[@@invariant]] or [[@@invariant always_check]].
    The PPX generates the [run] function for the state machine. If the module
    defines [sexp_of_state] (e.g. [type state = … [@@deriving sexp_of]]) [run]
    uses it to print the state after each step.

    Without the PPX, create rules with {!Rule.create} and the invariants
    with {!Invariant.create}, put them in a module of type {!State_machine},
    and pass that to {!run}.

    Every invariant is checked on the initial and final states. Between steps,
    invariants are sampled unless marked [always_check].

    Examples in this documentation assume [open Hegel].

    Example: an integer stack.

    {[
    module%hegel_state_machine Stack = struct
      type state = int list [@@deriving sexp_of]

      let push tc stack =
        let n = draw tc (integers ~min_value:0 ~max_value:100 ()) in
        n :: stack
      [@@rule]
      ;;

      let pop tc stack =
        assume tc (not (List.is_empty stack));
        List.tl stack
      [@@rule]
      ;;

      let short tc stack =
        note tc (Printf.sprintf "%d elements" (List.length stack));
        assert (List.length stack < 10)
      [@@invariant always_check]
      ;;
    end

    let%hegel_test integer_stack tc = Stack.run tc ~init:[]
    ]} *)

(** {2 Submodules} *)

module Pool : sig
  (** A pool of previously generated values. They are populated with the results
      of rules and may be used as arguments to later rules. A pool lets data
      flow from one rule to another, so a rule can act on a handle or identifier
      that an earlier rule produced rather than on a freshly drawn value.

      Create one with {!create} and populate it with {!add}. To draw from the
      pool, use the following generators:
      - {!values_reusable}: drawing from it returns a value in the pool without
        removing it.
      - {!values_consumed}: drawing from it removes a value from the pool and
        returns it.

      Example: a resource allocator. The [alloc] rule creates a fresh handle and
      deposits it in the pool. The [free] rule draws one of those handles back
      out and releases it. Without a pool, [free] would have no way to name a
      handle that a previous [alloc] actually created. It could only draw an
      arbitrary integer, most of which name no live resource.

      {[
      type state =
        { live : Int.Set.t
        ; handles : int Stateful.Pool.t
        }

      let alloc =
        Stateful.Rule.create ~name:"alloc" ~step:(fun _tc state ->
          let h = fresh_handle () in
          Stateful.Pool.add state.handles h;
          { state with live = Set.add state.live h })
      ;;

      let free =
        Stateful.Rule.create ~name:"free" ~step:(fun tc state ->
          (* draws a handle a prior [alloc] put in the pool *)
          let h = draw_silent tc (Stateful.Pool.values_consumed state.handles) in
          release h;
          { state with live = Set.remove state.live h })
      ;;
      ]} *)
  type 'a t

  (** Creates an empty {!Pool.t}. Pools are tied to a test case. Do not reuse
      one across test cases. *)
  val create : Internal.test_case -> 'a t

  (** Records [value] in [variables] for later draws.

      {[
      let n = draw tc (integers ~min_value:0 ~max_value:100 ()) in
      Stateful.Pool.add pool n
      ]} *)
  val add : 'a t -> 'a -> unit

  (** Returns the number of variables in the pool.

      {[
      assume tc (Stateful.Pool.size pool > 0)
      ]} *)
  val size : _ t -> int

  (** Create an unprintable generator that returns a variable from the [pool]
      without removing it. Calls [assume false] if the [pool] is empty.

      {[
      let existing = draw_silent tc (Stateful.Pool.values_reusable pool)
      ]} *)
  val values_reusable : 'a t -> ('a, Generators.unprintable) Generators.generator

  (** Create an unprintable generator that removes and returns a variable from
      the [pool]. Calls [assume false] if the [pool] is empty.

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

      To trace the state a rule produces on a failing replay, pass
      [?sexp_of_state] to {!run}.

      {[
      let push =
        Stateful.Rule.create ~name:"push" ~step:(fun tc stack ->
          let n = draw tc (integers ~min_value:0 ~max_value:100 ()) in
          n :: stack)
      ;;
      ]} *)
  val create : name:string -> step:(Internal.test_case -> 'state -> 'state) -> 'state t

  (** Returns the name of the rule.

      {[
      let label = Stateful.Rule.name push
      ]} *)
  val name : _ t -> string
end

module Invariant : sig
  (** An invariant is a property that must always be true in a stateful test. *)
  type 'state t

  (** Declares an invariant.

      - [name] is printed in the final output if the test fails on an invariant
      - [inv tc state] checks the invariant on [state]. Anything it draws or
        notes through [tc] prints indented under the line before it.
      - [always_check] defaults to [false]. When [true], the invariant is
        checked after every step. Otherwise, it is sampled.

      Every invariant is checked on the initial and final states.

      {[
      let short =
        Stateful.Invariant.create
          ~name:"short"
          ~inv:(fun _tc stack -> assert (List.length stack < 10))
          ()
      ;;
      ]} *)
  val create
    :  name:string
    -> inv:(Internal.test_case -> 'state -> unit)
    -> ?always_check:bool
    -> unit
    -> 'state t

  (** Returns the name of the invariant. *)
  val name : _ t -> string
end

(** {2 Running stateful tests} *)

(** A state machine.

    {[
    module Counter : Stateful.State_machine with type state = int = struct
      type state = int

      let add tc n = n + draw ~label:"by" tc (integers ~min_value:1 ~max_value:10 ())
      let rules = [ Stateful.Rule.create ~name:"add" ~step:add ]

      let invariants =
        [ Stateful.Invariant.create ~name:"small" ~inv:(fun _tc n -> assert (n < 100)) ()
        ]
      ;;
    end
    ]} *)
module type State_machine = sig
  type state

  val rules : state Rule.t list
  val invariants : state Invariant.t list
end

(** [run tc (module M) ~init] executes a stateful test by repeatedly applying
    randomly chosen rules of [M] starting from the [init] state. Every
    invariant is checked on the initial and the final state. After a step,
    invariants are randomly sampled unless they were created with
    [always_check:true]. Raises [Hegel.Usage_error] if [M] has no rules or
    [step_count] is below 1. [step_count] defaults to 50. Each case runs at
    least one step and at most [step_count].

    {[
    let%hegel_test counter tc = Stateful.run tc (module Counter) ~init:0 ~step_count:200
    ]}

    A [module%hegel_state_machine M] has an [M.run tc ~init], which
    calls this function on [M].

    On a failing replay, each applied rule prints as [Step N: <name>], with the
    printed draws and notes nested under it. When [sexp_of_state] is supplied,
    the model state is also printed after the initial state and after every step.

    {v
      state = 0
      Checking invariants on the initial state.
      Step 1: add
        n = 3
      state = 3
      Step 2: add
        n = 7
      state = 10
      Invariant my_invariant violated after step 2.
    v} *)
val run
  :  Internal.test_case
  -> ?step_count:int
  -> ?sexp_of_state:('state -> Sexplib0.Sexp.t)
  -> (module State_machine with type state = 'state)
  -> init:'state
  -> unit

(**/**)

(** [run_internal ~init ~rules ~invariants tc] is {!run} with the rules and
    invariants passed as lists. The [run] that a [module%hegel_state_machine]
    generates calls this. *)
val run_internal
  :  init:'state
  -> rules:'state Rule.t list
  -> invariants:'state Invariant.t list
  -> ?sexp_of_state:('state -> Sexplib0.Sexp.t)
  -> ?step_count:int
  -> Internal.test_case
  -> unit

(**/**)
