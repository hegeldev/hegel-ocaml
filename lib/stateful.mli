(** {2 Introduction}
    A stateful test exercises a system through a sequence of randomly chosen
    actions ("rules") applied to a state. Rules are constructed with
    {!Rule.create} from a [name] and a [step] function that performs one
    application of the rule, drawing any arguments it needs from the test case
    and returning the new state. Invariants are ['state -> unit] functions
    evaluated before any step is run and after every successful step.

    To run a state machine, call {!run} inside a Hegel test. Examples in this
    documentation assume [open Hegel].

    Example: an integer stack.

    {[
    let push =
      Stateful.Rule.create ~name:"push" ~step:(fun tc stack ->
          let n = draw tc (integers ~min_value:0 ~max_value:100 ()) in
          n :: stack)

    let pop =
      Stateful.Rule.create ~name:"pop" ~step:(fun tc stack ->
          assume tc (not (List.is_empty stack));
          List.tl stack)

    let%hegel_test integer_stack tc =
      Stateful.run
        ~init:[]
        ~rules:[ push; pop ]
        ~sexp_of_state:[%sexp_of: int list]
        tc
    ]}

    Passing [?sexp_of_state] makes a failing sequence print the model state after
    each step, so you can see how it evolved; see {!run}. *)

(** {2 Submodules} *)

module Pool : sig
  (** A pool of previously generated values. They are populated with the results
      of rules and may be used as arguments to later rules. A pool lets data flow
      from one rule to another, so a rule can act on a handle or identifier that
      an earlier rule produced rather than on a freshly drawn value.

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

      let free =
        Stateful.Rule.create ~name:"free" ~step:(fun tc state ->
            (* draws a handle a prior [alloc] put in the pool *)
            let h = draw_silent tc (Stateful.Pool.values_consumed state.handles) in
            release h;
            { state with live = Set.remove state.live h })
      ]}
  *)
  type 'a t

  (** Creates an empty {!Pool.t}. Pools are tied to a test case; do not
      reuse one across test cases. *)
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

      To trace the state a rule produces on a failing replay, pass
      [?sexp_of_state] to {!run}.

      {[
      let push =
        Stateful.Rule.create ~name:"push" ~step:(fun tc stack ->
            let n = draw tc (integers ~min_value:0 ~max_value:100 ()) in
            n :: stack)
      ]} *)
  val create : name:string -> step:(Internal.test_case -> 'state -> 'state) -> 'state t

  (** Returns the name of the rule.

      {[
      let label = Stateful.Rule.name push
      ]} *)
  val name : _ t -> string
end

(** {2 Running stateful tests} *)

(** Executes a stateful test by repeatedly applying randomly chosen [rules] to a
    state threaded from [init]. Every invariant is checked on the initial and the
    final state. After a step, invariants are randomly sampled. Raises 
    [Hegel.Usage_error] if [rules] is empty.

    On a failing replay, each applied rule prints as [Step N: <name>], with the
    values the rule draws nested under it. When [sexp_of_state] is supplied, the
    model state also prints as [state = <value>] after the initial state and
    after every step. An invariant that is violated prints 
    [Invariant N violated after step M], [... in the initial state], or
    [... in the final state], where [N] is the invariant's index in
    [invariants].

    {v
      state = 0
      Step 1: add
        draw_1 = 3
      state = 3
      Step 2: add
        draw_2 = 7
      state = 10
      Invariant 0 violated after step 2.
    v} *)
val run
  :  init:'state
  -> rules:'state Rule.t list
  -> ?invariants:('state -> unit) list
  -> ?sexp_of_state:('state -> Sexplib0.Sexp.t)
  -> Internal.test_case
  -> unit
