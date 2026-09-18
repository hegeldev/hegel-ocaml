# Concurrent stateful testing: capability-based workers

Status: Phases 1 and 2 done on 2026-09-17 (Phase 2 uncommitted). Phase 3
not started. Branch `concurrent-stateful-testing`.

Phase 2 as built (differences from the plan below): handles are
`type handle = H of unit ptr [@@unboxed]` with a ctypes view and the unsafe
crossing attribute (the attribute is not allowed on an alias); every `Ffi`
export is asserted portable at the end of `ffi.ml.in` (laundering the
`Ctypes` module does not work: its data values stay contended); `Locked`
(dune `select`, capsule0 under OxCaml) replaces the shared `Mutex`-guarded
tables; `Make_pool (Tbl)` became `Int_pool`; pool element types, `just`, and
`sampled_from` carry `value mod portable contended` kind constraints on
OxCaml (sequential pools included); `Concurrent_rule.t` has its own record;
the worker body captures a list of clones and a list of rules. Verified:
`just check-tests` 100% upstream; the full suite (210 tests), the PPX tests,
the deriver tests, the jane tests, and the examples build and pass under
5.2.0+ox. See `.claude/CLAUDE.md`, "OxCaml portability".

## Goal

Hegel spawns nothing. `run_concurrent` takes a concurrency capability and
uses it once per round to run the workers. Rules do not receive it and cannot
spawn: a task body cannot capture the outer capability (it is local to the
caller's region), nested work could not be attributed to a worker, and its
output could not be printed under the right clone. Hegel ships no implementation of the
capability in the `hegel` package and its documentation never describes
spawning domains or threads. Jane Street users get an adapter for their
`Concurrent` library in an optional sublibrary. On OxCaml, rule bodies are
portable, so the mode checker rejects data races in test code.

## Constraints

- Upstream OCaml keeps building. Every OxCaml-only construct goes behind cppo
  `#ifdef OXCAML`.
- `core` never becomes a required dependency of the `hegel` package, directly
  or transitively. Anything that depends on `concurrent`, `await`, `parallel`,
  `capsule`, or `portable` lives in an optional sublibrary.
- No public or documented way to spawn domains. The only implementation hegel
  owns is test-only, under `test/`.
- 100% coverage on `lib/` and `lib/jane/` stays enforced.

## Facts established by probes in the local 5.2.0+ox switch

- `Domain.Safe.spawn` needs a portable closure. The current worker closure
  captures the mutable control record and user rule closures, so it is
  rejected. Threads give no parallelism.
- Jane Street's `Concurrent` is a real opam package from
  `github.com/janestreet/concurrent`. It depends on `core`, `async`, `await`,
  `portable`, and requires `oxcaml-compiler`. Its interface uses mode syntax
  that upstream 5.2.0 and 5.4.1 reject at the first token. There is no
  upstream release. `capsule` depends on `base`. `capsule0` depends only on
  `basement` and `sexp_type`. `basement` has no dependencies.
- ctypes 0.24.0+ox carries no mode annotations in any interface. Every `Ffi`
  binding is nonportable until asserted otherwise.
- A hegel-shaped round over a hegel-defined `spawn_join_n` signature ran on
  `Concurrent_in_thread.with_blocking` (4 workers live at once, 4 domains) and
  `Parallel_scheduler.with_concurrent ~max_workers:2` (not all live), with no
  `Obj.magic` in the round code. Per-round data used only mode-crossing
  fields: `nativeint` handles, `Atomic.t`, immutable arrays.
- A module-level `@@ portable` at the top of an `.mli` applies to every `val`
  and failures are reported per value. Inferred portability crosses
  compilation units for modules without an `.mli`.
- A module-level generator obtained through a signature is nonportable unless
  the core type's closure fields carry `@@ portable` modalities. With the
  modality, `map` with a closure over a `ref` fails at the `map` call.
- `exn` and `Printexc.raw_backtrace` cross portability. `Ctypes.ptr` does not.
- A closure capturing a local capability cannot be a tail-call argument;
  `[@nontail]` fixes it.
- After any domain has been spawned, `Unix.fork` fails for the rest of the
  process, even after the domains are joined. This matters for the test
  binary only: `test/test_loader.ml`'s cold-cache download test forks.
- The OxCaml stdlib has no `Iarray` module; `Iarray` is Base.

## Decisions to settle before starting

1. Decided: `Hegel.Concurrent` would shadow Jane Street's
   `Concurrent` under `open Hegel`. Use `Hegel.Concurrency.t`.
2. Representation: a record with one closure field `spawn_join_n`, not a
   first-class module. It threads through generated code with one type and
   passes as a local value on OxCaml. The field is monomorphic in hegel's
   `outcome` type. Result type is `outcome list`, not `array`: a contended
   array cannot be read (`Array.iter` needs uncontended), a contended list of
   outcomes can be read and its exceptions re-raised (verified). On OxCaml
   the field type is
   `n:int -> (f:(int -> outcome) @ portable -> outcome list @ contended) @ local`;
   the inner `@ local` is required because a partial application of a local
   closure is itself local, and without it the adapter fails with "escape
   their region when it is partially applied" (verified).
3. Shared mutable tables in `Internal` (draw-name counters, `owned` record).
   They are shared across clones behind a mutex. On OxCaml use `capsule0`
   (`Capsule_prim.Data` plus `Capsule_blocking_sync.Mutex`) if `sexp_type` is
   core-free; verify first. Fallback: one `Obj.magic_uncontended` at the clone
   hand-off, with the ownership invariant documented.
4. Handle types. Try `[@@unsafe_allow_any_mode_crossing]` on `Ffi.test_case`
   and the other handle types, which keeps the assertion at the FFI level.
   Verify the attribute exists in 5.2.0+ox. Fallback: `nativeint` handles
   rebuilt with `Ctypes.ptr_of_raw_address` per call.
5. Decided: `Hegel.spawn` and `join` are removed, not ported, and the
   "Concurrency and parallelism" section of `lib/hegel.mli.in` is deleted,
   not rewritten (Phase 1, steps 4 and 5).

6. Decided: default. `Concurrency.threads`, a systhreads implementation in
   the core library on both compilers, is the default of a plain optional
   argument: `run_concurrent ?(concurrency = Concurrency.threads)` and the
   generated `run ?concurrency`. Existing call sites compile unchanged and
   behave as today: interleaving under the runtime lock, no parallelism,
   same worker attribution, same failure precedence. Users who want
   parallelism pass `~concurrency` built on the abstraction of their choice;
   the docs state the contract and do not show how to build one. Jane Street
   users pass `~concurrency:(Hegel_jane_concurrent.of_concurrent c)`. No
   registration hook. Decided 2026-09-17: `Concurrency.domains`, upstream
   only (`#ifndef OXCAML`), not the default: a pool of
   `Domain.recommended_domain_count () - 1` domains created on first use and
   kept until exit, each job on its own systhread inside a pool domain so
   bodies stay live however few domains there are. Reason: domains per
   round measured 684 ms against 56 ms for a pool and 133 ms for threads on
   100 cases x 20 rounds x 4 workers. Condition: a systhreads default must be acceptable in
   Jane Street processes; `Thread.create` carries no alert on OxCaml.
   Verified: OxCaml accepts a local optional argument with a global default.
   Interface syntax is `?concurrency:Concurrency.t @ local -> ...` (no
   parentheses; the parenthesized form is a syntax error); the
   implementation is a plain `?(concurrency = Concurrency.threads)`. Without
   the `@ local` in the interface, passing the adapter's value fails with
   "This value is local but is expected to be global". So the PPX emits the
   same `?concurrency` on both compilers and `run_concurrent` has one
   signature.

7. Decided (after Phase 2): rules mutate their state in place, as in
   hegel-rust; nothing returns a new state. Two rule types stay, because a
   concurrent step must be `@@ portable` with a `@ contended` state and
   OxCaml has no mode polymorphism to make one step serve both: `Rule`
   (`step : test_case -> 'state -> unit`, no modes, no group) for the
   sequential `run`, and `Concurrent_rule` (`?group`, portable step) for
   `run_concurrent ?concurrency ?min_concurrency ?max_concurrency` (defaults
   1 and `min_concurrency`). `Concurrent_pool` is gone: one thread-safe
   `Pool` with `add pool tc value` serves both. `stateful_seq.ml.in` and
   `stateful_concurrent.ml.in` are merged into `stateful.ml.in`, where the
   two runners share `run_machine` and the `run_rules` loop; the concurrent
   runner never runs inline, even at one worker. The PPX keeps
   `module%hegel_state_machine` and `module%hegel_concurrent_state_machine`.
   Rules returning a new state are a breaking change; the release note for
   these phases is `minor`.

## Phase 1: capability refactor, no modes, both compilers build

1. Add `lib/concurrency.ml`: the record type, an `outcome` type, and the
   `threads` implementation: N `Thread.create` per round, joined, outcomes
   collected into a list. Re-export as `Hegel.Concurrency`. Unit tests in
   `test/test_concurrency.ml`. Added later: `domains`, upstream only, behind
   cppo (`concurrency.ml.in`, `concurrency.mli.in`,
   `test/test_concurrency.ml.in`).
2. Rewrite `lib/stateful_concurrent.ml`. Delete the thread pool, `Mutex`,
   `Condition`, and `worker_loop`. Per round: one clone per worker, one
   `spawn_join_n`, then the existing `reraise_worker_failure` over the
   returned array. Group loop and invariant checks stay.
3. `Concurrent_rule.step` stays `test_case -> 'state -> unit`.
   `run_concurrent` and `run_concurrent_internal` take `?concurrency`,
   defaulting to `Concurrency.threads`.
4. Remove `Hegel.spawn`, `Hegel.join`, and `Hegel.worker`: the definitions in
   `lib/internal.ml.in` and `lib/internal.mli`, the re-exports in
   `lib/hegel.ml` and `lib/hegel.mli.in`, the two tests in
   `test/test_clone.ml`, and the use in
   `ppx/test/expect_tests/test_note_verbosity.ml`. They are the last thread
   creation in `lib/`, they are unstructured, and they contradict the
   no-spawn rule. `threads.posix` stays in `lib/dune` for
   `Concurrency.threads`. No replacement helper.
5. Delete the "Concurrency and parallelism" section of `lib/hegel.mli.in`
   entirely, including its Threads, Domainslib, and Eio guidance, and delete
   the `Thread.create` example in the `clone` doc comment. `clone` keeps a
   two-sentence doc: one handle is driven by one task at a time, so give each
   concurrent task its own clone.
6. PPX: the generated `run` for `module%hegel_concurrent_state_machine`
   gains `?concurrency` and forwards it. Existing call sites keep working.
   Rule bodies are unchanged.
7. Tests.
   - Add a test-only capability in `test/test_helpers.ml`. It exists to run
     the suite with real parallelism and is not exported or documented.
   - Add a second, sequential test-only capability to prove agnosticism and to
     cover the path where workers are not live together.
   - Run the concurrent tests in `test/test_stateful.ml` under the default,
     and additionally under the test-only parallel capability where the
     assertion depends on workers being live at once.
   - Run the fork-based loader test before the stateful suite in
     `test/test_hegel.ml`, or port it to `create_process`.
8. Docs. Rewrite the concurrent section of `lib/stateful.mli`: the capability
   contract only (`spawn_join_n` runs all bodies at the same time and returns
   when all have finished; bodies never raise; liveness is the
   implementation's property). State that the default is concurrent, not
   parallel, and that parallelism comes from a user-supplied capability
   built on the abstraction of their choice. Point Jane Street users at the
   adapter. Do not show how to build an implementation. Drop `Thread.yield`
   from the example.
   Add the `RELEASE.md` entry, listing the removal of `spawn`, `join`, and
   the concurrency section as breaking changes.
9. Verify: `just check` on 5.2.0 including coverage; build and run the suite
   in the ox switch with `--build-dir _build_ox`; `dune build @fmt`.

## Phase 2: OxCaml portability behind cppo

1. Convert `internal.mli`, `stateful.mli`, `settings.mli`, `ffi/ffi.mli` to
   `.mli.in`, and `ffi/ffi.ml`, `generators_core.ml`, `stateful_seq.ml`,
   `stateful_concurrent.ml`, `concurrency.ml` to `.ml.in` (the last for the
   field modes only). Add cppo rules;
   `lib/ffi/dune` needs its own copy of the flags rule.
2. `ffi.ml.in`: `Obj.magic_portable` on the memoized closure in `foreign`,
   once. Handle types per decision 4.
3. Put `#ifdef OXCAML` / `@@ portable` / `#endif` at the top of every
   `.mli.in`. Compile. The compiler names every value that fails. Fix each:
   `diff_renderer` becomes an `Atomic`; `test_aborted` and `draw_depth`
   become `Atomic`; the two shared tables per decision 3; anything
   intentionally main-thread-only gets `@@ nonportable`.
4. `@@ portable` modalities on the closure fields of `Generators_core.core`
   and on `Concurrent_rule.t.step`. Parameter modes on `map`, `flat_map`,
   `filter`, `composite`, `with_printer`, and `functions` follow by inference
   from the stored field; annotate them in the `.mli.in` only.
5. `'state @ contended` on the rule step. `Concurrent_pool` onto `capsule0`.
   The capability field becomes
   `n:int -> (f:(int -> outcome) @ portable -> outcome list @ contended) @ local`
   and `run_concurrent` declares `?concurrency:Concurrency.t @ local`.
6. Wire `capsule0` as an OxCaml-only dependency with a dune `select`; install
   it in the ox CI job. Confirm the dependency closure of the `hegel` package
   contains no `core`.
9. Verify: the ox CI job builds and runs the suite. Upstream `just check` is
   unchanged by construction.

10. Done after the unification: `lib/jane/hegel_jane.mli` is a cppo `.in`
    with the portable default, so `Hegel_jane.Derive` works from portable
    code (the jane deriver test compiled on OxCaml only after this). Core's
    functions are portable in the `5.2.0+ox` switch; the implementation
    needed no annotations.

## Phase 3: optional adapter for Jane Street's `Concurrent`

Done. `lib/jane/concurrent/hegel_jane_concurrent.ml` (`hegel.jane.concurrent`, `concurrent`
as a depopt) with `of_concurrent` only: the caller opens the scope
(`Concurrent_in_thread.with_blocking Await.Terminator.unkillable`) and
passes `of_concurrent concurrent` to the run, `[@nontail]`. `with_default`
and a global `threads` value (which did type-check) were dropped by
decision. The test runs a machine under `Concurrent_in_thread` and under
`Parallel_scheduler.parallel (Parallel_scheduler.scheduler ())`, and checks a
worker exception reaches the caller. It is opt-in through
`HEGEL_CONCURRENT_TESTS=1` (set in the ox CI job, which also installs
`concurrent` and `parallel`): dune allows only environment variables in an
executable's `enabled_if`, and an `(optional)` executable is still requested
by the default alias, so a library-availability gate does not work there.

1. `lib/jane/concurrent/hegel_jane_concurrent.ml` as an `(optional)` sublibrary
   depending on `concurrent`, listed as a depopt. Two functions.
   `with_default f` runs `f` under
   `Concurrent_in_thread.with_blocking Await.Terminator.unkillable` and hands
   it the wrapped capability. `of_concurrent` wraps `_ Concurrent.t @ local` into
   `Hegel.Concurrency.t @ local`. It exists because the core library cannot
   name `Concurrent.t` without depending on `core`. Its body is one call:
   `Concurrent.spawn_join_n c () ~n ~f:(fun _scope _ctx _inner i -> f i)`,
   which fixes the scope context to `()`, discards the per-task scope,
   context, and nested capability that hegel does not use, and converts the
   `Iarray` result with `Iarray.to_list`. The record captures the local
   `c`, so `of_concurrent` returns it with `exclave_` (verified end to end
   against `Concurrent_in_thread.with_blocking`).
2. Test: a concurrent machine under `Concurrent_in_thread.with_blocking` and
   under `Parallel_scheduler.with_concurrent`. Gate like the jane suite;
   install `concurrent` in the ox CI job only.
3. Docs: usage example with `[@nontail]`; state that preemptive
   implementations are the ones to use for concurrency testing.

## Risks

- Coverage under the test-only parallel capability has not been run yet.
- `.in` files are outside `dune fmt`; the formatted surface shrinks.
- The ox CI job needs `capsule0` and `concurrent` installed.
- Liveness is the caller's property. A cooperative implementation with fewer
  workers than the drawn concurrency cannot make all workers live at once, so
  a rule that blocks on another worker's action can starve there.
