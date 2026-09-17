(** {2 Introduction}
    A stateful test applies a random sequence of rules to a state. A rule is a
    [step] function that takes the test case and the current state, draws
    whatever data it needs, and returns the new state. Each step runs one rule.
    An invariant is a property that must always hold after each step.

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

(** {2 Concurrent stateful testing}

    A [module%hegel_concurrent_state_machine] concurrently runs rules on worker
    threads. Concurrent rules belong to a group. Only rules in the same group may run
    concurrently. The number of workers is in [[min_concurrency, max_concurrency]]
    (see {!run_concurrent}).

    A round is a step of the concurrent test. Each round selects one rule group,
    and each worker runs a sequence of rules from that group. Invariants run after
    all workers finish the round.

    The example store below has a bug. The store locks individual reads and writes,
    but releases the lock between reading a counter and writing its incremented value.
    Two workers can therefore overwrite each other's updates.

    {[
    module Store = struct
      type t =
        { lock : Mutex.t
        ; values : (int, int) Hashtbl.t
        }

      let create () = { lock = Mutex.create (); values = Hashtbl.create 4 }

      let get store key =
        Mutex.protect store.lock (fun () -> Hashtbl.find_opt store.values key)
      ;;

      let put store key value =
        Mutex.protect store.lock (fun () -> Hashtbl.replace store.values key value)
      ;;

      let put_if_absent store key =
        Mutex.protect store.lock (fun () ->
          if Hashtbl.mem store.values key
          then false
          else (
            Hashtbl.add store.values key 0;
            true))
      ;;

      let increment store key =
        let value = Option.value (get store key) ~default:0 in
        Thread.yield ();
        (* Make the lost-update race easier to observe. *)
        put store key (value + 1)
      ;;

      let snapshot store = Mutex.protect store.lock (fun () -> Hashtbl.copy store.values)
    end

    module%hegel_concurrent_state_machine Key_value_store = struct
      type state =
        { store : Store.t
        ; keys : int Stateful.Concurrent_pool.t
        ; increments : int Atomic.t
        }

      let register tc state =
        let key = draw tc (integers ~min_value:0 ~max_value:3 ()) in
        if Store.put_if_absent state.store key
        then Stateful.Concurrent_pool.add state.keys tc key
      [@@rule "operations"]
      ;;

      let increment tc state =
        let key = draw_silent tc (Stateful.Concurrent_pool.values_reusable state.keys) in
        Store.increment state.store key;
        Atomic.incr state.increments
      [@@rule "operations"]
      ;;

      let read tc state =
        let key = draw_silent tc (Stateful.Concurrent_pool.values_reusable state.keys) in
        match Store.get state.store key with
        | Some value -> note tc (Printf.sprintf "read %d -> %d" key value)
        | None -> note tc (Printf.sprintf "key %d is absent" key)
      [@@rule "operations"]
      ;;

      let snapshot tc state =
        let count = Hashtbl.length (Store.snapshot state.store) in
        note tc (Printf.sprintf "snapshot holds %d keys" count)
      [@@rule "snapshot"]
      ;;

      let no_lost_updates _tc state =
        let stored =
          Hashtbl.fold (fun _ value total -> total + value) (Store.snapshot state.store) 0
        in
        let performed = Atomic.get state.increments in
        if stored <> performed
        then
          failwith
            (Printf.sprintf
               "increments were lost: store sums to %d after %d increments"
               stored
               performed)
      [@@invariant always_check]
      ;;
    end

    let%hegel_test concurrent_store tc =
      let init : Key_value_store.state =
        { store = Store.create ()
        ; keys = Stateful.Concurrent_pool.create tc
        ; increments = Atomic.make 0
        }
      in
      Key_value_store.run tc ~init ~min_concurrency:1 ~max_concurrency:4
    ;;
    ]}

    The [operations] group allows registration, increments, and reads to overlap.
    The [snapshot] group runs separately. [Concurrent_pool] is the thread-safe
    version of [Pool]. The invariant compares stored values with an atomic count of
    completed increments.

    With [max_concurrency > 1], failures are reported without shrinking, replay,
    database persistence, or reproduction blobs.

    In the failure output, each rule execution is labeled with its worker and the
    time in milliseconds since the test case began to aid debugging.

    {v
    --- Failure: concurrent_store (...) -------------------------

    Concurrency level: 4
    Checking invariants on the initial state.
    ---------------- Round 1: group "operations" ----------------
    [worker 2 +0.238ms] Rule: register
    [worker 2 +0.246ms]   key = 0
    ...
    [worker 2 +0.261ms] Rule: read
    [worker 2 +0.263ms]   read 2 -> 0
    ...
    [worker 3 +0.223ms] Rule: increment
    ...
    ---------------- Round 2: group "snapshot" ------------------
    [worker 0 +0.363ms] Rule: snapshot
    [worker 0 +0.365ms]   snapshot holds 3 keys
    ...
    ---------------- Round 3: group "operations" ----------------
    [worker 0 +0.486ms] Rule: increment
    ...
    [worker 1 +0.464ms] Rule: register
    [worker 1 +0.468ms]   key = 1
    ...
    [worker 2 +0.509ms] Rule: increment
    ...
    [worker 3 +0.479ms] Rule: increment
    Invariant no_lost_updates violated after round 3.

    Exception: Failure("increments were lost: store sums to 11 after 12 increments")
    v} *)

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
      one across test cases. Drawn mutable values are shared by default.
      Pass a copying function for independent mutable values. Consumed draws
      return the stored value directly. [clone] must not call back into the
      same pool. *)
  val create : ?clone:('a -> 'a) -> Internal.test_case -> 'a t

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

  (** Draws a value without removing it. [draw] on an empty pool rejects the current
      rule.

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

module Concurrent_pool : sig
  (** A thread-safe pool of values shared by concurrent stateful workers. *)
  type 'a t

  (** Creates an empty concurrent pool. Pools are tied to a test case. Do not reuse
      one across test cases. Drawn mutable values are shared by default.
      Pass a copying function for independent mutable values. Consumed draws
      return the stored value directly. [clone] must not call back into the
      same pool. *)
  val create : ?clone:('a -> 'a) -> Internal.test_case -> 'a t

  (** [add pool tc value] records [value] using the calling worker's test-case
      clone. *)
  val add : 'a t -> Internal.test_case -> 'a -> unit

  val is_empty : _ t -> bool
  val size : _ t -> int

  (** Draws a value without removing it. [draw] on an empty pool rejects the current
      rule. *)
  val values_reusable : 'a t -> ('a, Generators.unprintable) Generators.generator

  (** Draws and atomically removes a value. An empty-pool draw rejects the
      current rule. *)
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

module Concurrent_rule : sig
  (** A rule applied to shared state by concurrent worker threads. The caller is
      responsible for synchronizing mutable state accessed by rule bodies. *)
  type 'state t

  (** [create ~name ?group ~step] declares a concurrent rule. Only rules with the
      same [group] may run concurrently. Rules without an explicit group are in
      one anonymous group. *)
  val create
    :  ?group:string
    -> name:string
    -> step:(Internal.test_case -> 'state -> unit)
    -> unit
    -> 'state t

  val name : _ t -> string
  val group : _ t -> string
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
  :  ?step_count:int
  -> ?sexp_of_state:('state -> Sexplib0.Sexp.t)
  -> Internal.test_case
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

val run_concurrent_internal
  :  init:'state
  -> rules:'state Concurrent_rule.t list
  -> invariants:'state Invariant.t list
  -> ?concurrency:Concurrency.t
  -> ?sexp_of_state:('state -> Sexplib0.Sexp.t)
  -> ?step_count:int
  -> min_concurrency:int
  -> max_concurrency:int
  -> Internal.test_case
  -> unit

(**/**)

(** A state machine whose rules may run concurrently. *)
module type Concurrent_state_machine = sig
  type state

  val rules : state Concurrent_rule.t list
  val invariants : state Invariant.t list
end

(** [run_concurrent ?step_count ?sexp_of_state tc (module M) ~init ~min_concurrency ~max_concurrency]
    executes a state machine using N worker threads, where N is in [[min_concurrency, max_concurrency]].
    In a round, all workers receive zero or more rules from one concurrency group.

    Invariants are checked on the initial and final state and sampled between
    rounds, unless they were created with [always_check:true].

    If [sexp_of_state] is provided, the state is printed before the first round
    and after each completed round.

    [step_count] defaults to 50 and bounds the number of rounds per test case.
    Each worker may execute multiple rules in a round.

    A [max_concurrency] greater than one makes the run nondeterministic. libhegel
    consequently reports a failure without replaying, shrinking or producing a failure
    blob.

    [concurrency] is how each round's workers are run. See {!Concurrency}. It
    defaults to {!Concurrency.threads}. *)
val run_concurrent
  :  ?concurrency:Concurrency.t
  -> ?step_count:int
  -> ?sexp_of_state:('state -> Sexplib0.Sexp.t)
  -> Internal.test_case
  -> (module Concurrent_state_machine with type state = 'state)
  -> init:'state
  -> min_concurrency:int
  -> max_concurrency:int
  -> unit
