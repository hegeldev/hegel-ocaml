# Hegel for OCaml

## Build Commands

```bash
# No setup step: libhegel is located (or downloaded + cached) at runtime.
just test          # Run tests with 100% coverage enforcement
just format        # Auto-format code with ocamlformat
just check-format  # Check formatting (fails if unformatted)
just docs          # Build API documentation with odoc
just check         # Run check-format + check-docs + check-tests (the full CI check)
```

## Tooling

- **OCaml**: 5.2.1
- **Build system**: Dune 3.21.1
- **Test framework**: Alcotest 1.9.1
- **Code coverage**: bisect_ppx 2.8.3 (enforced at 100% via scripts/check-coverage.py)
- **Formatter**: OCamlFormat 0.29.0 (version pinned in .ocamlformat)
- **Documentation**: odoc 3.1.0
- **Package manager**: opam 2.1.5
- **PPX derivation**: ppxlib 0.35.0 (for `[@@deriving hegel_generator]`)

## Project Structure

```
lib/                         # Library source
  dune                       # Library build config (bisect_ppx instrumented)
  hegel.ml / hegel.mli.in    # Main module — re-exports the public API
                             #   (the constructors via Generators.Public).
                             #   (.in files are cppo-preprocessed by dune rules
                             #   — `#ifdef OXCAML` compiler compat — into the
                             #   .ml/.mli the library builds from. Most .mli and
                             #   several .ml are .in now: see OxCaml portability)
  locked.mli.in              # Locked: mutex-guarded shared data. dune `select`
    locked.mutex.ml          #   picks locked.capsule.ml (capsule0, OxCaml) or
    locked.capsule.ml        #   locked.mutex.ml (Mutex + value). Core-free.
                             #   locked.capsule.ml is OxCaml syntax and listed in
                             #   .ocamlformat-ignore
  ffi/                       # ctypes bindings to native libhegel (NOT instrumented)
    ffi.ml                   # dlopen + 1:1 C-ABI wrappers; settings/run/test_case
                             #   handles; typed draws + string-generator handles;
                             #   events; the pretty-printer document (printer_*,
                             #   test_case_printer, note)
    loader.ml                # locate/download libhegel at runtime (env > site >
                             #   sibling ../hegel-rust build (libhegel_c.<ext>) > release)
  settings.ml / settings.mli # Hegel.Settings: the settings record (type t), its
                             #   verbosity/database/phase/health_check/backend enums,
                             #   default ()/from_profile/create materialized from
                             #   the engine's settings profiles (to_ffi/of_ffi)
  internal.ml.in             # Test runner + run lifecycle + typed-draw wrappers on
                             #   top of Hegel_ffi.Ffi; note/print_line/render_sexp +
                             #   flush_document (engine-side output, see Pretty
                             #   printing); events (cppo → internal.ml)
  generators.ml.in           # Re-export shim: include the four generators_* modules
                             #   (cppo → generators.ml; generators.mli.in likewise)
  generators_core.ml         # generator type; draw/draw_silent, map/flat_map/filter,
                             #   composite, span labels — the discriminated union
  generators_primitives.ml   # integers, booleans, floats, text, binary, just, formats
  generators_collections.ml  # lists, assoc_lists, hash_tables (+ the table-agnostic
                             #   make_hash_tables), and the collection protocol
  generators_combinators.ml  # sampled_from, one_of, tuples2/3/4
  generators_functions.ml    # functions/functions2/functions3: memoized
                             #   function generators (Claessen's show/shrink,
                             #   but no trie — the engine shrinks results)
  derive.ml                  # Hegel.Derive: scope-resolved names derived code
                             #   refers to (hegel_generator_int/…/char/list/
                             #   option + the Sexplib0 sexp_of_* converters)
  stateful.ml.in             # Stateful testing: Pool (shared by both machine kinds),
                             #   Rule (sequential, plain mutating step), Concurrent_rule
                             #   (?group, portable step taking the worker's ctx),
                             #   Invariant, State_machine + run tc (module M) ~init,
                             #   Concurrent_state_machine (type ctx) +
                             #   run_concurrent ~concurrency ?min/max_concurrency.
                             #   Both runners share run_machine (engine machine,
                             #   initial/final checks, free) and run_rules (one
                             #   worker's rules for a round); the sequential one runs
                             #   inline on tc inside a span, the concurrent one clones
                             #   per worker through Concurrency.spawn_join_n and
                             #   reraise_worker_failure. Spawns nothing itself (see
                             #   Concurrent stateful testing). The doc-hidden
                             #   run_internal/run_concurrent_internal take the lists
                             #   directly and are what the PPX-generated run calls.
                             #   Rule and invariant bodies run on indent-2 block handles
  concurrency.ml.in/.mli.in  # Hegel.Concurrency: the capability record
                             #   (spawn_join_n), threads (the default) and, upstream
                             #   only (#ifndef OXCAML), the pooled domains
  jane/                      # Optional hegel.jane sublibrary ((optional) in dune).
    hegel_jane.ml/.mli.in    #   Core.Hashtbl hash_tables + pool helpers and the
    test/                    #   sexp_diff require_equal renderer (set_sexp_diff);
                             #   (.mli.in: cppo, the OxCaml portable default)
                             #   instrumented + coverage-gated like lib/ (its own
                             #   test/ dir, gated behind HEGEL_SKIP_JANE_TESTS in
                             #   check-tests-no-coverage since it needs the core/
                             #   sexp_diff opam depopts — see justfile)
    concurrent/              # Optional hegel.jane.concurrent sublibrary (OxCaml
      hegel_jane_concurrent  #   only: depends on Jane Street's concurrent; a
        .ml/.mli, test/      #   sibling of hegel.jane, which must keep building
                             #   upstream). of_concurrent wraps a local Concurrent.t
                             #   as a local Hegel.Concurrency.t; the caller opens the
                             #   scope (Concurrent_in_thread.with_blocking, a Parallel
                             #   scheduler). Both files are OxCaml
                             #   syntax (.ocamlformat-ignore). Its test is opt-in
                             #   through HEGEL_CONCURRENT_TESTS=1 (set by the ox CI
                             #   job): dune only allows env variables in an
                             #   executable's enabled_if, and an (optional)
                             #   executable is still requested by the default alias

template/                    # hegel.template ppx: links ppx_template on
                             #   OxCaml; upstream, hegel_template.upstream.ml
                             #   removes the %template markers; a clear error
                             #   on OxCaml without ppx_template. NOT instrumented

ppx/                         # PPX rewriters and derivers
  dune                       # PPX library build configs; a rule generates
                             #   ppx_compat.ml from one variant below
  ppx_hegel_generator.ml     # Deriver: reads type decls, emits generator functions
  ppx_hegel_test.ml          # Expander: rewrites [let%hegel_test name tc = body]
                             # into a plain callable function (no registration,
                             # no runtime library — see Inline Test Integration below)
                             # and [module%hegel_state_machine M = struct .. end]
                             # into that module plus generated rules/invariants/run
                             # from its [@@rule]/[@@invariant] bindings
  ppx_compat_pre-53.ml       # AST compat shim for ppxlib < 0.36 (OCaml < 5.3)
  ppx_compat_post-53.ml      # AST compat shim for ppxlib >= 0.36 (OCaml >= 5.3)
  ppx_compat_oxcaml.ml       # AST compat shim for the OxCaml compiler
  test/                      # PPX E2E tests, package-attributed so opam-repo-ci runs them
    test_ppx_derive.ml       # PPX deriver E2E tests (package ppx_hegel_generator)
    test_ppx_derive_jane.ml  # Deriver + Hegel_jane.Derive tests ((optional)
                             #   executable; run via the justfile jane blocks)
    test_ppx_hegel_test.ml   # ppx_hegel_test expander E2E tests (package ppx_hegel_test)
    expect_tests/            # ppx_expect tests (dev-only, disabled in release profile)
                             # (one of the three is copied to ppx_compat.ml = ppx_hegel_compat lib)

test/                        # hegel's own test suite (one executable: test_hegel,
  dune                       #   Alcotest, package hegel — runs under `-p hegel`;
                             #   no PPX preprocessing beyond the ppx_js_style linter)
  test_hegel.ml              # Top-level Alcotest runner
  test_helpers.ml            # Shared test utilities (+ the test-only parallel and
                             #   sequential Concurrency capabilities)
  test_concurrency.ml.in     # Concurrency tests (cppo: the domains tests exist
                             #   upstream only)
  test_client.ml             # Internal config + run lifecycle tests (real engine)
  test_generators_*.ml       # Generator core / primitives / collections / combinators
  test_stateful.ml           # Stateful testing tests

docs/                        # Tutorial and guide documents
  getting-started.md         # Getting Started tutorial (OCaml translation)

examples/                    # Example programs demonstrating the library
  dune                       # Example executables build config
  basic_properties.ml        # Primitive generators: integers, booleans, floats
  collections.ml             # Collections and combinators: lists, filter, map
  real_world.ml              # Real-world scenario: sorted-merge property test
  derived_types.ml           # Derived generators via [@@deriving hegel_generator]
  higher_order.ml            # Function generators: functions/functions2/functions3

scripts/
  check-coverage.py          # Parses bisect-ppx-report, enforces 100%

README.md                    # Project overview, install, quick-start
```

## Architecture Overview

### Native backend (lib/ffi/ffi.ml)

There is no subprocess, socket, or wire protocol. The engine is the native
`libhegel` C library (from hegel-rust, header `hegel-c/include/hegel.h`), called
in-process via ctypes. `Hegel_ffi.Loader` resolves the shared library at runtime
(mirroring hegel-go): `$HEGEL_LIBHEGEL_PATH`, then a sibling
`../hegel-rust/target/{release,debug}/libhegel_c.<ext>` checkout (the name
`cargo build -p hegeltest-c` gives the cdylib; dune sandboxes run tests from
`_build/.sandbox/...`, so that relative path does not resolve under
`dune runtest` — set `HEGEL_LIBHEGEL_PATH` there), then a SHA-256-verified
download from the hegel-rust GitHub release cached under
`~/.cache/hegel-ocaml/libhegel/<version>/` (opt out with
`HEGEL_LIBHEGEL_NO_DOWNLOAD=1`). `Hegel_ffi.Ffi` `dlopen`s that path and exposes
thin 1:1 wrappers: settings handles, the run lifecycle (`run_start`,
`next_test_case`, `run_result`, `run_free`), and per-test-case primitives — the
typed draws (`generate_integer`, `generate_boolean`, `generate_float`,
`generate_bytes`, `generate_string` + the `string_generator_*` handle
constructors, `generate_date`/`time`/`datetime`, `generate_ipv4`/`ipv6`), spans,
collections, pools, state machines (`new_state_machine` takes the per-machine
`~step_count` since libhegel 0.38.0; there is no settings-level step count),
`target`, `event`/`event_value`, derived handles (`test_case_clone`,
`test_case_block`), the pretty-printer document (`printer_*`,
`test_case_printer`, `note` — see Pretty printing below), `mark_complete`. There is no CBOR: each value is
drawn by a dedicated typed call rather than a schema round-trip (this replaced the
removed `hegel_generate`/CBOR-schema path in libhegel 0.26.0). There is no
engine thread (removed in libhegel 0.30.1): `hegel_next_test_case` runs all
engine work between test cases on the calling thread, so its binding releases
the OCaml runtime lock for the call's duration. The `ffi` library is
deliberately NOT bisect_ppx-instrumented, keeping its
mechanical marshalling out of the 100%-coverage gate (no `[@coverage off]`).

`lib/protocol.ml`, `lib/connection.ml`, `lib/cbor/`, `lib/cbor_helpers.ml`, and
the old Python-subprocess install flow were removed in the native-backend and
typed-draw migrations.

### Dependencies: core-free main library + optional hegel.jane

The `hegel` library depends on the stdlib plus `sexplib0` (printer type
`'a -> Sexplib0.Sexp.t`, the same type as `Core.Sexp.t`), `unix` (isatty for
color detection), `threads.posix`, ctypes/ipaddr/dune-site (`yojson` is a test-only dependency of
the PPX test that parses the engine's `sdk.jsonl`). `core`,
`core_unix`, and `sexp_diff` are NOT dependencies of the library: `core` and
`sexp_diff` are opam depopts that gate the `(optional)` sublibrary
`hegel.jane` (`lib/jane/`, module `Hegel_jane`). Anywhere the library needs a
container or renderer a Jane Street type used to provide, the dependency is
refunctionalized — the code takes the operations as closures/parameters, and
each side instantiates them:
- pools: `Int_pool` (an `Int_table` of values, no lock: a sequential machine runs its rules one at a time) and `Int_pool_concurrent` (the same table behind a `Locked`) are the client-side pools; `resolve_pool_draw` (find/remove closures) is the shared id-resolution step
- hash tables: `make_hash_tables ~of_pairs ~sexp_of_t` ← `hash_tables` (Stdlib.Hashtbl) / `Hegel_jane.hash_tables` (Hashtbl.Poly)
- dates/times: `make_dates ~of_date`/`make_times ~of_time`/`make_datetimes ~of_datetime` (+ `~sexp_of`) (+ `?min_date`/`?min_time`/`?min_datetime` and `max_*` bounds) ← `dates`/`times`/`datetimes` (ISO 8601 strings) / `Hegel_jane.dates`/`ofdays` (Core values)
- chars: `make_characters ~of_char ~sexp_of` ← `chars` / `Hegel_jane.chars`. `Core.Char.t = char`, so both sides draw the same value and only the printer differs (`sexp_of_char` vs `Core.Char.sexp_of_t`) — unlike the other refunctionalized pairs, `of_char` is `Fun.id` on both sides, kept only for symmetry with `~of_date`
- require_equal diff: `Internal.set_diff_renderer` hook ← default prints both values (`-`/`+`, red/green); `Hegel_jane.set_sexp_diff ()` installs the `sexp_diff` two-column renderer

The test suite still links `core`/`core_unix` (test-only dependencies; users
never install them). `core`/`sexp_diff` being opam depopts is about the
published `hegel` package's dependency footprint for its *users* — a hegel
*developer* running `just check` is still expected to have them installed:
`lib/jane/` is bisect_ppx-instrumented and 100%-coverage-gated like `lib/`
(unlike `ffi`/the PPXes, which stay excluded), with its own `lib/jane/test/`
suite (`test_hegel_jane.ml` Alcotest, `test_require_jane.ml` a
[sexp_diff] snapshot). `just check-tests` (the coverage-enforcing recipe)
always runs it; `just check-tests-no-coverage` (the `compat`/`oxcaml` CI jobs,
which don't install `core`/`sexp_diff`) skips it via `HEGEL_SKIP_JANE_TESTS=1`
— see the justfile.

### Generator System (generators_core.ml + generators_{primitives,collections,combinators}.ml)

The generator type and combinators (`draw`, `map`, `flat_map`, `composite`, …)
live in `generators_core.ml`; the primitives, collections, and combinators are
split across the sibling `generators_*.ml` files. `generators.ml` is a thin shim
that `include`s all five so they surface as one `Hegel.Generators` module.
The `generators_*` files have no interfaces, so the constructor signatures
(`booleans` … `filter`, plus the `date`/`time` types) and their docs live once,
in the doc-hidden submodule `Generators.Public` (`generators.mli.in`; in
`generators.ml.in` it includes the five files, the top level does `include
Public`, and the signature narrows `Public` to the constructors). `Generators` does `include module type of struct include Public
end`, so odoc lists the constructors on the `Generators` page and never shows
`Public`. `hegel.mli`/`hegel.ml` re-export them unqualified with the same
`include` (inside `hegel.mli`'s doc-hidden block, so they aren't listed
twice), next to the `generator`/`printable`/`unprintable` types, so
`open Hegel` alone is enough — `integers ()` and `Generators.integers ()`
name the same value, and the templated portable instances come along. A new
constructor goes in `Public` only; `Generators` stays the documented reference. Project code prefers the unqualified form
wherever `open Hegel` is already in scope.

Generators are a discriminated union:
- **Leaf** — holds a `draw : test_case -> 'a` closure that performs a single typed engine draw (via one of the `Internal.generate_*` primitives). Calling `map` on a Leaf composes the closure in place (no extra span), since the engine already wraps every primitive draw in its own span.
- **Mapped** — wraps source + transform function (adds a `mapped` span).
- **FlatMapped** — wraps source + a function returning a generator. Evaluated recursively inside a `flat_map` span.
- **Filtered** — wraps source + predicate. Up to `max_filter_attempts` retries before `assume false`.
- **CompositeList** — lists of any element core. Uses the collection protocol (with_collection / collection_more) to generate elements one at a time.
- **Composite** — a `generate_fn` thunk run inside a labeled span; used by tuples, one_of, `lists ~unique`, and hash tables (all of which now always drive the collection protocol / draw sub-values directly — there is no schema fast path).
- **Values** — the engine-pool core behind `Stateful.Pool` and `Concurrent_pool`: `{ pool; select : test_case -> 'a }`, where `select` draws an id from the engine pool and resolves it against the client table. `Generators.Int_pool_concurrent` does that under the pool's `Locked`, so the table never disagrees with the engine about which ids exist while workers share it; the sequential `Generators.Int_pool` needs no lock. Both are doc-hidden client sides over `Int_table`; `resolve_pool_draw` is the shared id-resolution step. `hash_tables` is refunctionalized at the API level: `make_hash_tables ~of_pairs ~sexp_of_t` is table-agnostic, `hash_tables` closes it over `Stdlib.Hashtbl`, `Hegel_jane.hash_tables` over `Core.Hashtbl.Poly`.
- **Span labels** (libhegel 0.39.0) — a label is an opaque `uint64_t` (OCaml `int64`) identifying the generator that opened a span; the engine treats two spans with the same label as coming from the same generator when it shrinks and mutates, and does nothing else with it. There are no predefined label constants in the ABI any more. `Generators_core.Labels.from_name`/`combine` compute the engine's own hashes (64-bit FNV-1a over the name's bytes / over the labels' little-endian bytes in order — `hegel_label_from_name`/`hegel_label_combine`, pinned equal by `test_labels_match_engine` through the `Ffi.label_*` bindings) so no context is needed at generator construction. Every core stores its `label`, fixed at construction: a `Leaf` from its primitive's name (`leaf ~name:"integers"` → `hegel_ocaml.integers`), and everything built from other generators as `combine [own kind; components' labels…]` (`lists (integers ())` ≠ `lists (text ())`; `map` on a leaf stays a leaf but combines `Labels.mapped` in; `with_printer` leaves the label alone; `Values` is the constant `Labels.pool`). `label_of_core`/`Ppx_internal.label_of` read it back. The deriver emits `combine [fixed_dict|enum_variant; from_name "<type name>"]` so two derived types of the same shape stay distinct. Names are prefixed `hegel_ocaml.` to keep clear of libhegel's own `hegel.<kind>` spans.
- **Function** — a generated function (`functions`/`functions2`/`functions3`). `build ~name` returns a fresh per-test-case memoized function that draws each result from `returns` on first application (memoized on the argument via structural hash/equality — a polymorphic `Stdlib.Hashtbl` — so `sexp_of_arg` is display-only and an omitted one shows `<opaque>` without collapsing the key) and shows applied pairs as `name arg = result` in the print region on the final replay. Only *top-level* applications print — a pair applied at draw depth > 0 (inside a span) is suppressed, like a nested draw. A distinct core so `draw_silent_named` / `draw_named` can thread the draw-site binding name into the function (see the PPX note below); the name threads even when the function is drawn nested. Result draws are wrapped in a span labelled `combine [Labels.function_result; label of returns]`.

### Inline Test Integration (ppx/ppx_hegel_test.ml)

The `ppx_hegel_test` PPX rewrites `let%hegel_test name tc = body` into a
single top-level item: `let name = fun () -> Hegel.run_hegel_test ... (fun tc
-> body)`. That's it — `name` is an ordinary `unit -> unit` value with no
registration, no runtime library, and no side effect at module init. The same
rewriter also handles `module%hegel_state_machine M = struct … end`, the
analogue of hegel-rust's `#[hegel::state_machine] impl`. At expansion time it
collects the bindings marked `[@@rule]` or `[@@invariant]` into appended
`rules` and `invariants` lists (`Rule.create ~name:"<binding>" ~weight
~step:<binding> ()` and the same for `Invariant.create`) plus a
`run ?step_count ?sexp_of_state tc ~init`. A marker's options are a *record*
payload and nothing else — `[@@rule { weight = 2.5 }]`,
`[@@invariant { always_check = true }]`, bare `[@@rule]` for the defaults.
`rule_attribute` spells out one `Ast_pattern` alternative per field
combination, so the fields take either order and either may be omitted, and a
repeated or unknown field is a compile error. A weight is carried
as the text of a float literal (an int payload gains a `.`) and defaults to
`1.0`. `module%hegel_concurrent_state_machine` does the same with
`Concurrent_rule.create ?group ~weight ~name ~step ()`, whose `[@@rule]` record
also takes `group` (which the sequential form rejects). Its rule bodies take
the worker's context between the test case and the state (`tc ctx state`).
If the module declares `type ctx`, the generated `run ~concurrency
?min_concurrency ?max_concurrency ?step_count ?sexp_of_state tc ~init` makes
the capability required, since no default can supply that context; otherwise
the PPX appends `type ctx = unit` and the `run` defaults `?concurrency` to
`Hegel.Concurrency.threads` (`declares_type`). Both call
`Stateful.run_concurrent_internal`. There
is no registry and no runtime discovery. The generated `run` calls the
doc-hidden `Stateful.run_internal`, which takes the lists directly, because
the expanded module need not declare `type state` and the public
`Stateful.run` takes a `State_machine` module. `sexp_of_state` defaults to
the module's own when it binds one or derives it on `type state`
(`defines_sexp_of_state`). The markers are stripped from the emitted items,
every other item is kept, and a module with no `[@@rule]` is a compile error.
Marked bodies get the same draw-name injection as a test body, judged at
depth 0. A rule or invariant body runs in its own naming scope each step (see
Pretty printing), so `let n = draw tc g` prints as `n`, not `n_1`. Invariants
take the test case (`inv : test_case -> 'state -> unit`) so they can draw and
note. Hegel
has no test runner of its own and no `(inline_tests (backend ...))` stanza:
the project's own tests wire each `let%hegel_test`-produced function into
whatever test framework the project already uses (see `examples/*.ml`, which
each end with a plain `let () = test_foo (); test_bar (); ...`, and
`ppx/test/test_ppx_derive.ml` / `test_ppx_hegel_test.ml`, which build an
`Alcotest.test_case` list by hand). `dune runtest` then works exactly like it
does for any other test executable in that framework — there is nothing
hegel-specific to integrate.

Within the body the PPX also injects binding names into draws: `let x = draw tc g`
becomes `draw_named ~label:"x" ~repeatable:.. tc g`, and `let x = draw_silent tc g`
becomes `draw_silent_named ~name:"x" tc g`. Both target hidden entry points, keeping
`~repeatable`/`~name` off the public `draw`/`draw_silent` (the `draw`→`draw_named`
precedent). The `~name` is only meaningful for a function generator (`Function` core),
where it labels the shown `x arg = result` pairs; it is ignored for every other
generator, and attaches at the draw site (so it works through an intermediate
`let g = functions ..; let f = draw_silent tc g` binding). Precedence: an explicit
`?name` on `functions` always wins, else the draw-site binding name, else `"function"`.
A function made printable (via `with_printer`) is drawn with
`draw`; `draw_named` threads the label the same way (even when nested) and prints
the usual `x = value` line — the function renders as `<fun>` through its printer —
only at the top level, suppressing it when nested like any other draw.

Because the PPX only produces a callable and never calls it, a
`let%hegel_test` composes with any test framework: drop the produced
function into `Alcotest.test_case "name" `Quick name`, an OUnit test, a
`let%expect_test` body (see `ppx/test/expect_tests/`), or just call it
directly from `let () = ...`. Nothing about `let%hegel_test` opts a library
into an inline-tests backend or auto-discovery — the user always writes the
`dune runtest`-facing entry point themselves, exactly as they would for a
handwritten property test built on `Hegel.run_hegel_test` directly.

### Type-Directed Derivation (ppx/ + lib/derive.ml)

The `ppx_hegel_generator` PPX deriver synthesizes a printable generator from
type declarations annotated with `[@@deriving hegel_generator]`. It follows
the base_quickcheck conventions:

1. **Naming**: type `t` derives `hegel_generator`; any other type `foo`
   derives `hegel_generator_foo`. `Hegel.draw tc My_module.hegel_generator`
   reads naturally.
2. **Scope resolution**: generated code refers to every type constructor by
   name with the same mangling — `int` → `hegel_generator_int`, `M.t` →
   `M.hegel_generator`, and a parameterized type applies its argument
   generators (`int list` → `hegel_generator_list hegel_generator_int`). The
   PPX holds no primitive table. `Hegel.Derive` supplies the built-in names
   (int, bool, float, string, char, list, option), and `Hegel` includes it,
   so `open Hegel` is enough in a deriving file. A module opened later can
   shadow the names — that is how `Hegel_jane.Derive` swaps in Core flavors.
3. **Always printable**: the deriver also emits `sexp_of_<t>` (it calls
   `Ppx_sexp_conv_expander.Sexp_of.str_type_decl`; ppx_sexp_conv is a build
   dependency of the PPX package, not of user projects) and wraps the
   generator in `with_printer sexp_of_<t>`. Draw with `draw` to print on a
   failing replay, or `draw_silent` to stay silent. Deriving `sexp`/`sexp_of`
   alongside stays legal: the identical `sexp_of_<t>` definitions shadow.
   `[@sexp.opaque]` on a field type is the escape hatch for un-sexpable
   fields. `Hegel.Derive` re-exports the `Sexplib0.Sexp_conv` primitive
   converters because ppx_sexp_conv resolves builtins by unqualified name.
4. **Attributes**: `[@hegel.generator EXPR]` on any type occurrence (record
   field, constructor argument, tuple component) replaces that occurrence's
   generator with `EXPR` — the quickcheck idiom for ranges and custom
   generators. `[@hegel.do_not_generate]` on a variant constructor excludes
   it from generation: its argument types need no generator, the derived
   printer renders them opaque (the deriver injects `[@sexp.opaque]` before
   invoking the sexp expander), and the deriver emits one
   `let _ = fun … -> C …` item per excluded constructor to suppress
   warning 37 (nothing else constructs it).
5. **Type shapes**: records emit a `test_case -> t` thunk wrapped with
   `Generators.composite` (fields drawn in declaration order via nested
   lets); variants pick a constructor index via `sampled_from` (all-nullary
   enums are a bare `sampled_from`; data-carrying variants wrap the index and
   argument draws in an `enum_variant` span); inline-record constructors
   (`Pcstr_record`) construct the record literal directly inside the
   constructor — `C expr` is illegal syntax for an inline record; aliases
   reuse the aliased type's generator expression directly. The
   `Ppx_compat.extract_constr_args`/`map_constr_arg_types` helpers abstract
   the constructor-argument representation across the three toolchains.
6. **`~portable`**: `[@@deriving hegel_generator ~portable]` wraps every
   combinator the deriver emits (`composite`, `composite_with_label`,
   `sampled_from`, `with_printer`, and each applied parameterized generator
   such as `hegel_generator_list`) in `[@mode portable]`, and marks the
   generated `val` portable in a signature
   (`Ppx_compat.portable_value_description`, a no-op off OxCaml). The value
   keeps its name: a portable value also serves nonportable code. A component
   (`M.hegel_generator`, an override) that is not portable is a compile error
   at the use site. A parameterized generator used under `~portable` must be a
   template, as `Hegel.Derive`'s `list`/`option` are. Opt-in, so a user who
   derives without it meets no modes. On upstream OCaml it has no effect:
   the attributes it emits are ignored.
7. **Jane**: `Hegel_jane.Derive` includes `Hegel.Derive`, swaps the char pair
   to the `Core.Char` flavor, and adds wrapper modules (`Date`, `Time_ns`,
   `Time_ns.Span`) that include their Core counterparts plus a
   `hegel_generator`. A field must be typed with the wrapper path (`Date.t`,
   not `Core.Date.t`) — the deriver mangles the path as written, and
   `Core.Date.hegel_generator` does not exist. One
   `open Hegel_jane.Derive` replaces `open Hegel.Derive`.

See `GAPS.md` for the deriver's remaining gaps versus base_quickcheck
(recursion, type parameters, polymorphic variants, …). Note `lib/derive.ml`
previously held qualified runtime helpers (`generate_option`/`generate_list`);
that module was deleted and the filename now hosts `Hegel.Derive`, the
scope-resolution module described above.

**Usage example:**

```ocaml
(* In your dune file, add:
     (preprocess (pps ppx_hegel_generator ppx_hegel_test)) *)

open Hegel

type point = { x : int; y : int } [@@deriving hegel_generator]
type color = Red | Green | Blue [@@deriving hegel_generator]

type entity =
  { name : string
  ; initial : char
  ; level : (int[@hegel.generator integers ~min_value:3 ~max_value:5 ()])
  ; tag : int option
  }
[@@deriving hegel_generator]

(* Derived generators are printable: draw with [draw] to print the value on
   a failing replay, or [draw_silent] to stay silent. *)
let%hegel_test derived_types_smoke tc =
  let p = Hegel.draw tc hegel_generator_point in
  let c = Hegel.draw_silent tc hegel_generator_color in
  let e = Hegel.draw tc hegel_generator_entity in
  ignore (p, c, e)
;;
```

**Supported field types:**
- `int` — the full native `int` range (same default as `integers ()`)
- `bool`, `float` (finite: no NaN, no infinity), `string`, `char` (Latin-1)
- `t list` — engine-driven length via the collection protocol, as `lists`
- `t option` — `Some v` or `None`, as `optional`
- Named types `t` / `M.t` — resolves `hegel_generator_t` / `M.hegel_generator`
  (must be in scope)
- Tuples `(t1 * t2 * ...)` — generates each component in order


### Collection Protocol

`lists` (both plain and `~unique`), `assoc_lists`, and `hash_tables` draw their
elements one at a time through an *engine-managed collection* — libhegel decides
how many elements to produce (there is no whole-collection schema draw):
1. `hegel_new_collection` (min/max size bounds) → a `hegel_collection_t *`
2. `hegel_collection_more` → loop while it returns true, drawing one element per
   true result
3. `hegel_collection_reject` → mark the last element invalid (used to reject
   duplicates under `~unique` and by `assoc_lists`' key check)
4. `hegel_collection_free` → release the handle, exactly once

### Caller-owned handles (libhegel 0.31.0)

Collections, *variable pools*, and *state machines* are opaque caller-owned
handles (`hegel_collection_t *`, `hegel_pool_t *`, `hegel_state_machine_t *`),
not the `int64_t` ids they were before 0.31.0. Each has a matching destructor —
`hegel_collection_free` / `hegel_pool_free` / `hegel_state_machine_free` — and
must be freed exactly once; freeing twice is undefined behaviour. Freeing is
order-independent with respect to the test case and run, and NULL is a safe
no-op. On `HEGEL_E_STOP_TEST` the constructor leaves the out-parameter NULL, so
the `Ffi` wrappers `check_rc` before reading it.

Who owns what in hegel-ocaml:
- collections → `Generators_core.with_collection` (`Fun.protect`, so a
  `Stop_test` mid-draw still frees)
- state machines → `Stateful.run`, the same way
- blocks → `Internal.with_block` (freed with its context when the body
  returns or raises; a hand-written match, not `Fun.protect`, because the
  callback may be a local closure), mirroring hegel-rust's lexically scoped
  `TestCase::child`. So the `tc` a rule or invariant body receives is valid
  only for that step (documented on `Rule.create`/`Invariant.create`).
- variable pools and clones → the test case. `Stateful.Pool.create` and
  `Hegel.clone` are public and have no lexical scope, and user code may
  capture them, so `Internal.new_pool` / `Internal.clone` add the handle to
  the test case's `owned` record (pools, plus `(context, handle)` pairs for
  clones, behind a mutex; a clone or block shares the record with the test
  case it was derived from, like `draw_state`), and `run_test_case` calls
  `free_owned` once the case is complete — matching the order in hegel-rust's
  own `hegel-c/tests/c_abi_inprocess.rs`, which frees everything before
  `hegel_mark_complete`. `Stateful.Pool.add` takes the calling rule's test
  case and the pool handle is owned by the test case, so a pool created
  inside a rule body keeps working after that step's block is freed. Clones
  and blocks used to be freed by a `Gc.finalise_last` finaliser
  instead; that was a use-after-free, because the compiler treats a record as
  dead after its last field read, so the finaliser could run in the middle of
  an engine call that had just read `tc.context` (a `note` from an invariant
  body segfaulted under a small minor heap).

Note: the published reference at <https://hegel.dev/reference/libhegel> is
**stale on this point** — it still documents the pre-0.31.0 `int64_t` ids and
lists no destructors for these three. The authority is
`hegel-c/include/hegel.h` at the pinned tag (and `nm` on the downloaded
`libhegel`, which exports all four of the functions above).

### Pretty printing (engine-side layout)

All user-visible test output — notes, drawn values, stateful traces, the
require_equal diff — is assembled in libhegel's per-test-case *document*
(`hegel_test_case_printer` / `hegel_note`), not streamed to stderr as it
happens. `Internal.note` appends verbatim (pre-indented, possibly multi-line)
lines; `Internal.print_line` assembles a draw line from `Text`/`Value`
segments, laying each sexp out engine-side with `Internal.render_sexp`: an
atom is its `Sexp.to_string` escaping, a list is a group — `(`, children
separated by breakable spaces, `)` — so a value that fits the line prints
inline (byte-identical to `Sexp.to_string_hum`; pinned by a test) and one
that doesn't breaks all-or-nothing, one child per line (unlike `pp_hum`'s
fill style), at the document's default width 79. After `mark_complete`,
`run_test_case` reads the document back (`flush_document`:
`printer_resolve` — required when clone regions exist, an argument error
otherwise, hence the catch — then `printer_value`) and prints it to stderr
inside the client-drawn failure frame; `printed_output` is "the document was
non-empty". Verbosity gating stays client-side (`should_print`): under Quiet,
or a non-final case at Normal, nothing is appended and the read is skipped.
Clones write into their own region, anchored where the clone was made, so
concurrent output assembles deterministically regardless of scheduling.
Indentation is engine-side too (libhegel 0.37.10 block handles):
`Internal.with_block tc ~indent f` runs `f` on a handle onto the *same* choice
stream whose print region is a block nested in `tc`'s at the current position,
every line `indent` columns further in, ending with the block; the block is
freed when `f` returns. `Stateful.run` runs each rule's `step` and each invariant body with
`with_block tc ~indent:2` so their draws nest under the `Step N: name` note.
The rule loop is `Stateful.run_rules`, shared by both runners: it pulls rule
indices from the engine until the worker's round ends, notes the heading
(`Step N:` sequentially, `Rule:` concurrently), runs the body on the block,
and on `Assume_rejected` reports the rejection to the engine. It only creates the
block when `should_print tc` holds; a non-printing case runs the body on `tc` itself, since
a block per step costs a native handle and a context, which
measured as 50% more wall time and 20x the major collections on a
200-case x 500-step machine. And
`final_replay` runs the body via `run_test_case ~indent:2` so it sits inside
the client-drawn failure frame (`flush_document ~framed` adds the blank line
after the frame header). The client prepends no spaces anywhere. Mirroring
hegel-rust's `TestCase::child`, a block starts at span depth 0 and opens a
fresh `draw_state` (a rule's draw names are scoped to that one invocation: a
`[@@rule]` body's `let n = draw ..` prints as `n` in every step, and a
closure defined inside a `let%hegel_test` body — flagged repeatable by the
PPX — as `n_1` in every step), while sharing `owned_pools`; a clone instead
copies the span depth and shares the parent's `draw_state`.
Unlike a clone, a block must not be driven concurrently with its parent.
`Internal.set_worker_index` binds `hegel_test_case_set_worker`:
`run_concurrent` tags each worker's per-round clone so the engine stamps its
lines `[worker N +X.XXXms]`; the sequential `run` runs on `tc` itself and
nothing is stamped.
Flipping the `should_print` gate to always-append (so the engine sees every
case's representation) is the intended future Tyche switch.

### Entry point

The engine runs in-process, so there is no subprocess or session to manage.
The public entry point is `Hegel.run_hegel_test ?settings ?test_location
test_fn` — `Internal.run_hegel_test`, which is `Internal.run_test` with [settings]
defaulting to `Settings.default ()`. The `let%hegel_test` PPX targets the
doc-hidden `Hegel.run_hegel_test_ppx` — a thin wrapper that sets `~from_ppx:true`
on `Internal.run_hegel_test` — so the PPX-vs-plain signal never appears on the
public `run_hegel_test`. The `[@@failure_blobs ...]` record/replay workflow is
supported: the PPX forwards the listed blobs as `~failure_blobs`, which replays
the first blob as a standalone deterministic case (pair it with
`print_blob = false` to suppress the `rerun with:` line that failing runs
print by default). `from_ppx` selects that line's syntax: a
`[@@failure_blobs [...]]` attribute under the PPX, a `~failure_blobs:[...]`
argument for a plain `run_hegel_test` caller. For persisting and replaying
failing examples across runs, use `database` / `database_key`.

### Test location and Antithesis reporting (libhegel 0.41.1)

The Antithesis integration lives in libhegel, not in hegel-ocaml (the former
`lib/antithesis.ml`, which wrote `sdk.jsonl` itself, was deleted when
libhegel 0.41.1 centralized it). `Hegel.test_location` is
`Internal.test_location` (`function_name`/`file`/`begin_line`, built by the
`let%hegel_test` PPX from the binding's source location). `run_test` hands it
to `build_ffi_settings`, which after `Settings.to_ffi` calls
`Ffi.settings_test_location` (`hegel_settings_set_test_location(file,
begin_line, class_name, function)`) on the handle. The engine's `class_name`
is the "class, module or package enclosing the test"; hegel-ocaml passes
the file path without its extension (`Filename.remove_extension loc.file`)
(`tests/list_tests.ml` → `tests/list_tests`), since the PPX records the file,
not the module path. The directory is kept because two stanzas may each
define a `my_module.ml`, and the engine keys the assertion on this name. Inside Antithesis (`ANTITHESIS_OUTPUT_DIR`) the engine then
appends the SDK-format declaration + verdict lines for every run started from
that handle and for the final blob replay (so a failing run writes two
failing verdicts), identified as `<path>::<function> passes properties`; a
run-level error counts as a failure. Without a location, or outside
Antithesis, nothing is written. The engine re-reads the environment per run,
so `test_client.ml` exercises it in-process with a tempdir (no child process,
unlike `hegel.toml`). Invalid UTF-8 in any string is `HEGEL_E_INVALID_ARG` →
`Usage_error`. The location is per-test identity, not a setting:
`Settings.register_profile` (`to_ffi ~database_key:None`) never sets one, and
`Settings.t` has no field for it.

### Settings (lib/settings.ml)

`Hegel.Settings` is a plain record (`Settings.t`) in the base_quickcheck
`Test.Config.t` style: `Settings.default ()` is the engine's resolved
`default` settings profile, `Settings.create ?test_cases ?seed ()` layers the
two most common overrides (taking `seed` as an `int`), and every other field
is set with OCaml's record update syntax —
`{ (Settings.create ~seed:0 ()) with verbosity = Settings.Verbose }`.
There are deliberately no `with_*` builder functions. The enums (`verbosity`,
`database`, `phase`, `health_check`) live in the same module, so their
constructors are written qualified (`Settings.Disabled`) rather than relying on
type-directed disambiguation. `Internal` does `open Settings` for its own
pattern matches.

Defaults are the engine's (libhegel 0.40.0 settings profiles): there is no
OCaml-side CI detection any more. `Settings.default ()` /
`Settings.from_profile name` call `hegel_settings_new_for_profile` (with the
reserved name `default` for the former) on a throwaway context and read the resolved
handle back field by field through the `hegel_settings_get_*` getters
(`Settings.of_ffi`), the way hegel-rust's `Settings::new` does; that is what
makes `hegel.toml`, `HEGEL_DEFAULT_PROFILE`, `HEGEL_CONFIG`, and the shipped
`ci`/`workload` profiles apply to OCaml runs. `Settings.register_profile` and
`Settings.set_default_profile` wrap the matching engine calls. Going the other
way, `Settings.to_ffi` (what `Internal.build_ffi_settings` calls) sets *every*
field on a fresh handle — including `database` with `NULL` for `Unset` and the
health-check mask even when empty — so the record, not the profile the fresh
handle was resolved from, is authoritative for the run. That includes
`backend` (`Default`/`Urandom`): `default ()` reports the profile's choice
(`urandom` under `workload`) and `to_ffi` writes it back. `print_blob` is a
plain field too: its base value is `true` since libhegel 0.42.0 (before that
the engine's base had it off and hegel-ocaml forced it on client-side), and
the client does the printing (`print_failure_body` gates on
`settings.print_blob`). `hegel_settings_new`
can now fail (an unknown default profile, a malformed `hegel.toml`): it raises
`Usage_error` with the engine's diagnostic. `hegel.toml` is loaded once per
process, so `test_client.ml` exercises it in a child process
(`HEGEL_TEST_CONFIG_CHILD`, dispatched at the top of `test_hegel.ml`).

### Test Runner (lib/internal.ml.in)

`run_hegel_test` builds an `Ffi.settings` from the OCaml settings, calls
`Ffi.run_start`, then loops on `Ffi.next_test_case` until it returns `None`. Each
test case handle is wrapped in a `test_case` record and passed to the user's function.
The setters in `build_ffi_settings` (`Settings.to_ffi`) cannot fail: every
`hegel_settings_set_*` returns `HEGEL_OK` (the step count, formerly the only
setting the engine could reject, is now a `Stateful.run ?step_count` argument
validated by `hegel_new_state_machine`); only the initial `hegel_settings_new`
can, on a bad profile configuration, raising `Usage_error`.
The client controls when a final run occurs. Exceptions map to
`Ffi.mark_complete` statuses: VALID, INVALID (`Assume_rejected`/`Flaky_strategy`),
OVERRUN (`Stop_test`, the engine's stop signal during a primitive), INTERESTING
(any other exception, with a location-derived origin from `extract_origin`).
Interesting exceptions are captured by origin so the final-replay exception is
re-raised; after the loop, `Ffi.run_result` failures are raised (single) or
aggregated into a "Multiple failures" report. `run`/`settings` handles are freed
in an `Exn.protect ~finally`.

### Concurrent stateful testing (lib/stateful.ml.in, lib/concurrency.ml.in)

Two runners share `run_machine` and `run_rules`, mirroring hegel-rust's
`Rule`/`ConcurrentRule` split. Both kinds of rule mutate their state in
place; nothing returns a new state. A sequential `step` is
`test_case -> 'state -> unit`; a concurrent one is
`test_case -> 'ctx -> 'state -> unit`, where `'ctx` is the per-worker
context the capability passes to each body: `Concurrency.t` is `'ctx t` with
`f : 'ctx -> int -> outcome`, `threads` and `domains` are `unit t` passing
`()`, `Concurrent_state_machine` declares `type ctx`, and `run_concurrent`
takes `~concurrency` as required because a `threads` default would pin
`ctx = unit`. The context exists so the system under test can run its own
tasks on the scheduler hegel runs on: `Hegel_jane_concurrent.of_concurrent`
passes each rule `{ context; concurrent }`, the task's scheduler value (a
`Parallel_kernel.t` under a `Parallel` scheduler) and its fresh nested
`Concurrent.t`. Rules never see the round's `Scope.t`: a task detached onto
it that fails raises out of `spawn_join_n` and loses every sibling's
outcome, while nested work through `ctx.concurrent` fails inside its rule's
own outcome (findings from the standalone probe, 2026-09-22).
The sequential `run` (`Rule`, no groups, concurrency fixed at 1) runs each
round inline on `tc` inside a `stateful_rule` span (`stop_span
~discard:rejected`), deterministic and shrinkable, with `Step N: name`
headings and no worker stamps; its rule bodies are ordinary functions, so on
OxCaml they see the state uncontended and plain refs work. The concurrent
`run_concurrent` (`Concurrent_rule` with `?group`, `?min_concurrency` default
1, `?max_concurrency` default `min_concurrency`; the engine draws the worker
count when it creates the machine) never runs a rule inline, even at one
worker: each round notes a `Round N: group` header, clones the test case once
per worker, tags the clone with `set_worker_index`, and makes one call to
`concurrency.spawn_join_n ~n ~f`, where `f i` runs `run_rules` for worker
`i` on clone `i` (heading `Rule: name`), returning an `outcome` (`None`, or
the exception with its backtrace; bodies never raise into the capability).
`reraise_worker_failure` picks the highest-precedence failure (usage/internal
error, then overrun, then invalidation, then a test failure; lowest worker
index first), then the invariants run on the main thread. The runner spawns
no threads or domains itself. `Hegel.Concurrency.t` is a record with that one
field; `run_concurrent` takes it as `~concurrency` (required), and the
generated `run` defaults it to `Concurrency.threads` (one systhread per body
per round: interleaving, no parallelism) only when the module declares no
`type ctx`. One `Pool` serves both kinds; `add` takes the
calling rule's test case because a pool add is a draw on that handle. The
optional `hegel.jane.concurrent` sublibrary (`lib/jane/concurrent/`, OxCaml only) is
the adapter for Jane Street's `Concurrent`: `of_concurrent c` is one
`Concurrent.spawn_join_n c () ~n ~f` call plus `Base.Iarray.to_list`,
returning the record with `exclave_`. Every `Concurrent.t` is handed out
`@ local` and the record captures it, so the capability is local and the run
that receives it is `[@nontail]`; the caller opens the scope
(`Concurrent_in_thread.with_blocking Await.Terminator.unkillable`, or a
`Parallel_scheduler`). A global `Concurrency.t` that opened a
`with_blocking` scope per round did type-check but was dropped by decision:
the caller owns the scope. `concurrent` is an opam depopt. `Concurrency.domains` is upstream-only (`#ifndef OXCAML`): a
pool of `recommended_domain_count - 1` domains created on first use and joined
at exit, one job queue per domain, jobs dealt round-robin, each job on its own
systhread inside its domain so bodies stay live however few domains there
are. Results go back as an `outcome list`, not an array, because on OxCaml a
contended array cannot be read. Once any domain has been spawned `Unix.fork`
fails for the rest of the process, so `test_hegel.ml` runs the forking
`loader` suite first. Jane Street's `Concurrent` library is OxCaml-only and
depends on `core`, which is why its adapter is the optional
`hegel.jane.concurrent` sublibrary. See `plan.md` for the OxCaml portability
phase.

### OxCaml portability (cppo, modes, the trust boundary)

Under OxCaml (`#ifdef OXCAML`, set by the `cppo-flags` rule in `lib/dune`
and `lib/ffi/dune`) the library is mode-checked so a concurrent rule body can
be `portable`. Upstream OCaml builds the same sources with the annotations
stripped; every file carrying mode syntax is a cppo `.in`. Macros:
`PORTABLE` = `@@ portable` (field modality), `MODE(m)` = `@ m` (a mode, or a
template's mode variable: `MODE(portable)`, `MODE(m)`), `KIND(k)` = `: k` (on a
type variable or type: `('a KIND(value mod c))`, `t KIND(…)`), `ABSTRACT(a, k)`
= `(type (a : k))` / `(type a)` (a kind-annotated locally abstract type; a
template's instances share one `let … and …` group, so a named `'a` there
would leak the portable instance's kind into the plain one). Each expands to
nothing (or the plain form) upstream, so one copy of the code serves both
compilers; `#ifdef OXCAML … #else` is kept for code that really differs
(`[%call_pos]`, `Concurrency.domains`, the ipaddr launder, `Hashtbl.MakePortable`), `CROSSING` = `: value mod
contended` in the generator files.

Portability is opt-in through `ppx_template` (`let%template`/`val%template`
with `[@@mode m = (nonportable, portable)]`): each combinator that takes a
caller's closure or generator is written once and compiled at both modes, and
a caller picks the portable instance with `(map [@mode portable])`.
`just`/`sampled_from` template the element kind as well
(`[@@mode (m, c) = ((nonportable, uncontended), (portable, contended))]`),
so the plain instance takes any type. ppx_template names the portable
instance `f__portable` (the kind axis adds nothing), so references inside
`(m, c)` templates use `[@mode m]`. The primitives are not templated: they
return `MODE(portable)`. `ppx_template` exists only for OxCaml, and dune allows no
variables in `pps` library names, so the `hegel.template` ppx (`template/`)
`select`s it when installed and otherwise links the generated
`hegel_template.fallback.ml`. Upstream, that is a copy of
`template/hegel_template.upstream.ml`, a rewriter that keeps each `let%template`/`val%template` item and removes the
marker (single items only, not `[%%template]` blocks), and the compiler ignores
the remaining `[@mode]` attributes. On OxCaml, reaching the fallback means
`ppx_template` is missing (it is only a depopt), so the file is an
`[%%ocaml.error]` telling the user to install it. `lib/`, `lib/jane/`, `test/`, and both hegel PPXes list
`hegel.template`, the PPXes so users get `[@mode portable]` without adding
`ppx_template` themselves. cppo still
guards the syntax upstream cannot parse (`@ m`, kind annotations).

- **Interfaces.** `hegel.mli.in`, `generators.mli.in`, `internal.mli.in`,
  `settings.mli.in`, `derive.mli.in`, `ffi.mli.in`, and
  `jane/hegel_jane.mli.in` start with a module-level `@@ portable` default:
  every `val` is portable and the compiler checks each implementation. Core's
  own functions are portable in the `5.2.0+ox` switch, so `hegel_jane.ml`
  needs no annotations of its own, and a type derived under
  `Hegel_jane.Derive` gets a portable printer. `stateful.mli.in` instead puts `sig @@ portable` on the
  `Pool`/`Rule`/`Invariant`/`Concurrent_rule` submodules (`PORTABLE`); the
  runners (`run`, `run_concurrent`, …) stay nonportable since they reference
  `Concurrency.threads`, which uses `Thread.create`.
  `Int_pool_concurrent.t` / `Concurrent_pool.t` constrain their element type
  to `value mod portable contended` and take a portable `clone`; the
  sequential `Int_pool`/`Pool` constrain nothing.
- **Types that cross.** `Generators_core.core`/`generator` cross contention
  only: a generator holds no mutable state, so a rule body can read one it
  captured, but whether it is portable depends on the closures it was built
  from, which the template instance records. A record built from portable
  fields is portable, so each portable instance is compiler-checked with no
  cast. `Internal.test_case` crosses portability and contention: handles cross (see
  below), `test_aborted`/`draw_depth` are `Atomic.t`, the draw-name table and
  the `owned` record are `Locked.t` (declared `value mod portable contended`
  in `locked.mli.in`; `protect`'s result type must cross since it leaves the
  lock). `Concurrent_rule.t.step` is
  `(test_case -> 'ctx @ local -> 'state @ contended -> unit) @@ portable`, so a
  concurrent rule body is portable, sees its state contended, and gets its
  context local (the adapter's record captures a task-local `Concurrent.t`);
  a rule written as a named function marks that parameter `(ctx @ local)`
  unless the context type crosses locality, as `unit` does. A sequential
  `Rule.t.step` has no modes, which is why the two rule types exist (OxCaml
  has no mode polymorphism to make one step serve both). `run_concurrent`
  takes `init:'state @ portable` and `concurrency:'ctx Concurrency.t @ local`;
  `Concurrency.t`'s field is
  `n:int -> (f:('ctx @ local -> int -> outcome) @ portable -> outcome list @ contended) @ local`
  (a contended array is unreadable, a contended list is not).
- **The trust boundary is `Ffi`, in two places.** (1) Every libhegel handle
  is `type handle = H of unit ptr [@@unboxed]`, declared
  `value mod portable contended` with `[@@unsafe_allow_any_mode_crossing]`
  under OxCaml (the attribute is not allowed on a type alias, hence the
  constructor); `handle_t` is a ctypes `view` so every `ptr void` in a C
  signature converts at the boundary and no wrapper call site changed.
  libhegel's threading contract (hegel.h "Threading") is what makes it true.
  (2) ctypes 0.24.0+ox has no mode annotations, so every binding is
  nonportable; the block at the end of `ffi.ml.in` asserts each exported
  value portable with `Obj.magic_portable`. Laundering `Ctypes` as a module
  does not work: its data values (`Ctypes.int` …) stay contended inside
  portable code. The only other launder is three `ipaddr` functions in
  `generators_combinators.ml.in`.
- **Runner shape.** `run_rules` is the shared rule loop (next rule from the
  engine, heading, block, rejection report), parameterized by a heading
  function, a `~ctx @ local` (`()` sequentially), and a
  `rule : int -> string * (test_case -> 'ctx @ local -> unit)` lookup so it
  serves both rule types; the body closure it hands `with_block` captures the
  local context, which is why `with_block` takes its callback `@ local` and
  frees with a match instead of `Fun.protect`. The `work` function in `run_concurrent_internal`
  is the portable worker body; it captures `rules : Concurrent_rule.t list`
  (a list, not an array: `Array.get` needs an uncontended array), the state
  machine handle, and the `@ portable` state; `dispatch_round` reads its
  clone from a `test_case list`. The capability is passed down to `loop`,
  not captured, so it may be local (`dispatch_round`'s parameter is
  annotated `@ local`, `LOCAL_CONCURRENCY`); `run_machine` does not use
  `Fun.protect` for the same reason, and the `run_machine` call in
  `run_concurrent_internal` is `[@nontail]` (`NONTAIL`) because its callback
  argument captures the local capability.
- **`Locked` under OxCaml** is `Capsule_prim.Data` + `Capsule_blocking_sync.Mutex`
  (`capsule0`, deps `basement` and `sexp_type` only). `protect` goes through
  `Data.iter` and an atomic cell because `Data.extract` wants a unique result,
  and it carries exceptions out before re-raising because raising under
  `with_lock` poisons the mutex (a pool must survive a failing `clone`).
- **Writing portable test/user code.** Inference order matters: a captured
  variable whose type is still a variable when the closure is checked is
  rejected, so annotate (`fun (n : int) -> …`, `fun tc (value : int Atomic.t)`,
  optional parameters `?(max_size : int option)`). Bookkeeping in rule bodies
  is `Atomic`, never `ref`. `Thread.yield`/`Thread.delay` are nonportable;
  use `Domain.cpu_relax`/`Caml_unix.sleepf`. Under `open Core`, `Atomic` is
  Core's and a different type: `module Atomic = Stdlib.Atomic`. A printer
  application is `(printer gen) v`. `Alcotest.fail` inside a rule body is
  nonportable; use `failwith`.
- **Building locally.** `dune build --build-dir _build_ox …` in the
  `5.2.0+ox` switch (delete `_build_ox` afterwards). The ox CI job installs
  `capsule0`, `concurrent`, and `parallel` from `.github/oxcaml-ci.opam`, in
  the same `opam install` as the with-test deps (a second solver call can
  drop packages the first one installed). ocamlformat cannot parse OxCaml
  syntax: `.in` files are outside `dune fmt`, and `lib/locked.capsule.ml` is
  in `.ocamlformat-ignore`.

## Key Patterns and Conventions

### Documentation

- All public types, functions, exceptions, and constants use `(** ... *)` doc comments for odoc
- `just docs` must build with zero warnings — this is enforced in CI
- Parameter descriptions live inline in the first sentence when names are self-explanatory

### Testing

- Every lib module has a corresponding `test/test_<module>.ml`
- All tests run against the real engine: libhegel is dlopen'd in-process (and
  downloaded on demand), so there are no fake engines. Some test names carry a
  historical `_e2e` suffix; it no longer signals a different harness
- `test/` must build under `-p hegel` (opam-repo-ci runs it): plain Alcotest
  functions calling `Hegel.run_hegel_test`, no `let%hegel_test`, no PPX beyond
  the `ppx_js_style` linter. White-box tests use the doc-hidden `(**/**)`
  re-export `Hegel.Internal`. Only `Generators`,
  `Stateful`, and the values/types directly under `Hegel` are documented API
- PPX E2E tests live under `ppx/test/`, attributed via `(package ...)` to
  `ppx_hegel_generator` (`test_ppx_derive.ml`) and `ppx_hegel_test`
  (`test_ppx_hegel_test.ml`) so `dune runtest -p <pkg>` runs them; each builds
  its own `Alcotest.test_case` list from the PPX-produced functions, same as
  any other consumer would. `ppx/test/expect_tests/` stays
  package-less and dev-only (`enabled_if (<> %{profile} release)`)
- 100% branch and line coverage is mandatory — no exceptions, no `[@coverage off]`

### Error Handling

- `Internal.Assume_rejected` — raised by `assume false`; mapped to `mark_complete INVALID`
- `Internal.Stop_test` — raised when the engine signals StopTest (choice exhaustion); mapped to `mark_complete OVERRUN`
- `Hegel_ffi.Ffi.Usage_error` (re-exported as `Hegel.Usage_error`) — raised by `check_rc` on `HEGEL_E_INVALID_ARG`; `run_test_case` re-raises it untouched (no `mark_complete`, no shrinking), mirroring hegel-rust's `InvalidArgument` unwind. Generators therefore don't duplicate engine-side argument validation

### Typed Draws (no schema)

There is no CBOR schema layer. Each generator draws its value through a dedicated
typed FFI call (`Hegel_ffi.Ffi` / `Internal.generate_*`):
- `integers` → `generate_integer ~min_value ~max_value` (i64 bounds; OCaml native int fits)
- `booleans` → `generate_boolean 0.5 None`
- `floats` → `generate_float ~min_value ~max_value ~allow_nan ~allow_infinity ~exclude_min ~exclude_max ~smallest_nonzero_magnitude` (width 64; unbounded ends are ±infinity)
- `binary` → `generate_bytes ~min_size ~max_size`
- `text` / `characters` → build a text `string_generator` handle (codec / codepoint bounds / categories / include-exclude chars) then `generate_string`; surrogates auto-excluded
- `chars` → `generate_text` fixed to `min_size:max_size:1`, codepoints 0-0xFF (Latin-1), decoded via `String.get_utf_8_uchar` (a codepoint above 127 encodes to 2 UTF-8 bytes, so this can't just index byte 0) into a native `char` via `make_characters ~of_char ~sexp_of`
- `from_regex` / `emails` / `urls` / `domains` → the matching `string_generator_*` handle + `generate_string`
- `dates` / `times` / `datetimes` → `generate_date`/`time`/`datetime` structs, bounded by the caller's `?min_date`/`?max_date`, `?min_time`/`?max_time`, `?min_datetime`/`?max_datetime` (validated by the engine: a bad bound is `HEGEL_E_INVALID_ARG`, which `check_rc` raises as `Usage_error` and the runner propagates unshrunk). The parts are the `date`/`time` records (`hegel_time_t` carries nanoseconds since libhegel 0.36.0) and feed the refunctionalized builders `make_dates`/`make_times`/`make_datetimes` (`~of_date`/`~of_time`/`~of_datetime` constructor + `~sexp_of` printer); the public `dates`/`times`/`datetimes` close them over ISO 8601 strings (`YYYY-MM-DD`, `HH:MM:SS.fffffffff` with the fraction always printed, joined by `T`). A typed date library plugs in its own `~of_date` (no string parsing round-trip)
- `ip_addresses` → `generate_ipv4`/`generate_ipv6` raw bytes, rendered to strings by the `ipaddr` library (`Ipaddr.V4/V6.{of_octets_exn, to_string}`; RFC 5952 for v6)
- `sampled_from` → `generate_integer 0 (n-1)` then index into the values array
- `just` → a Leaf whose `draw` ignores the engine and returns the constant
- `one_of` / `optional` / tuples / `lists` / `hashmaps` → `Composite`/`CompositeList` cores that draw an index or drive the collection protocol, calling sub-generators' draws directly

String-generator handles are context-bound: built from `tc.context`, used for the
draw, and always freed (`Internal.with_string_generator`).

### Coverage Rules

- 100% line coverage is mandatory on library code
- `scripts/check-coverage.py` parses `bisect-ppx-report summary` output
- Unreachable engine-contract violations use `failwith "..."` (tested via unit tests on the transform)
- `[@coverage off]` annotations are never used
- Only the instrumented `hegel` library and the `hegel.jane` sublibrary are measured; the `hegel_ffi` bindings, examples, and PPX code are not

## Lessons Learned

### PPX Deriver Implementation

1. **PPX generates printable generators**: The deriver emits `sexp_of_<t>`
   (through ppx_sexp_conv's expander library) and wraps the generator in
   `with_printer sexp_of_<t>`, yielding a `(t, printable) generator` named by
   the quickcheck convention (`hegel_generator` for `t`,
   `hegel_generator_foo` otherwise). A field whose type cannot sexp needs
   `[@sexp.opaque]`; that is the one way bare deriving can fail to compile.
   (Earlier revisions emitted unprintable `<t>_generator` values drawn only
   with `draw_silent`.)

2. **PPX tests need a separate executable**: Because the PPX needs
   `(preprocess (pps ppx_hegel_generator))`, the test file using `[@@deriving hegel_generator]`
   must be in a separate `(test ...)` stanza from the main test suite. Both test
   executables are run by `dune runtest`.

3. **ppxlib.metaquot is essential**: The PPX uses `[%expr ...]` and `[%stri ...]`
   metaquot syntax for readable AST construction. This requires
   `(preprocess (pps ppxlib.metaquot))` in the PPX's own dune file.

4. **Scope resolution replaced runtime helpers**: Generated code resolves
   `list`/`option` (and every primitive) by unqualified name from
   `Hegel.Derive`, so the PPX needs no type table and `Hegel_jane.Derive` can
   shadow the defaults. (The name `Derive` previously held qualified runtime
   helpers; those were deleted once generated code stopped calling them, and
   the name was reused for the scope-resolution module.)

5. **Floats default to finite**: The PPX generates `floats ~allow_nan:false ~allow_infinity:false ()`
   to avoid NaN/infinity in derived types, which would cause issues in most user code.

### Documentation and Polish Stage

7. **Zero odoc warnings is enforced by fatal warnings in the dev profile**: the root
   `dune` file sets `(env (dev (odoc (warnings fatal))))`, so `dune build @doc` (and
   `just check-docs`) fails outright on any odoc warning (e.g. a bad reference), on cold
   and warm builds alike. (By default odoc warnings don't fail the build and dune's cache
   hides them on rebuilds; the recipe used to force a cold build by deleting
   `_build/default/_doc` and failing on any output, but that delete corrupted dune's
   incremental odoc state whenever sources had changed since the last doc build.) All lib
   modules must have
   `(** ... *)` doc comments on every public type, function, constant, and exception.
   References to non-public modules (e.g. `Internal`) must be code spans (`[Internal.note]`),
   not `{!...}` links — the target isn't in the doc tree, so the link can't resolve.

8. **odoc module-level comment must come first**: The module-level `(** ... *)` comment must appear
   before any `open` statements or definitions. odoc picks up only the first doc comment as the
   module doc. Comments placed after the first definition are treated as item-level docs.

9. **README under 200 lines**: The full API reference belongs in odoc comments, not the README.
   README should cover: what it is, installation, a quick-start example, a generator/combinator
   table, mention of the PPX deriver, project layout, and build commands. All API detail goes
   in `just docs` output.

10. **Getting Started tutorial in `docs/getting-started.md`**: Plain Markdown, not an odoc page.
    Reference it from README.md. Translate all Python library examples to idiomatic OCaml, adding
    short notes where the OCaml API differs (no decorator, no `.generate()` method, etc.).

11. **Five example programs cover the full surface area**: `basic_properties.ml` (primitives,
    assume, note), `collections.ml` (lists, map, flat_map, filter, sampled_from, hashmaps),
    `real_world.ml` (sorted-merge property test), `derived_types.ml` (PPX deriver),
    `higher_order.ml` (function generators). Each has a standalone `main`; derived_types needs
    a separate dune stanza with PPX preprocessing.

12. **opam not on PATH in shell spawned by `just`**: The `just` tool starts a fresh shell that
    does not source `.bashrc` or `.profile`. Fix: add
    `export PATH := env("HOME") + "/.opam/5.2.1/bin:" + env("PATH")` at the top of the
    justfile, and use `eval $(opam env)` inside recipes that need the full opam environment.
    The `export PATH` line in justfile is evaluated by `just` itself, not the shell.

### Coverage and bisect_ppx

- **Raise usage errors with a literal `raise`, not a helper.** bisect_ppx marks
  a call in tail position only after it returns (to preserve tail calls), so a
  helper like `let usage_error msg = raise (Ffi.Usage_error msg)` leaves every
  call site uncovered even when tests hit it. `raise` itself is special-cased,
  so generators write `raise (Internal.Usage_error msg)`.

### Good-Taste Audit

13. **OCamlFormat is the authority on doc comment placement**: OCaml has two valid placements for
    documentation comments — before an item (`(** doc *) type t = ...`) or after it
    (`type t = ... (** doc *)`). Both are accepted by odoc. However, **OCamlFormat enforces the
    trailing form** for `type` and `exception` declarations — it will revert any "before" placement
    on format. Trust the formatter; do not fight it. Trailing doc comments after type/exception
    declarations are the OCamlFormat-canonical style.

14. **`_foo` naming convention**: In OCaml, a leading `_` on a name signals "intentionally unused"
    and suppresses the unused-variable warning. Using `_foo` for a module-level binding that IS
    used (e.g. `let _session = ...` that is referenced throughout the module) is misleading and
    confusing. Only use `_foo` or `_` for genuinely unused bindings. The exception is ppxlib's
    `let _deriver = Deriving.add ...` pattern, where the value IS intentionally unused (the
    registration side-effect is what matters). Avoid `_my_foo` — `_deriver` or `_` are cleaner.

15. **`_foo` used immediately after binding is a genuine bad taste**: `let _msg = ...` then
    `let pairs = extract_dict _msg` — where the `_msg` binding is immediately used — is wrong.
    The `_` prefix should only appear on bindings that are structurally required but whose value
    is intentionally discarded. If you access the value, drop the underscore prefix.

16. **Trailing `(** ... *)` docs after `and` declarations in mutually recursive types**: For
    `type t = ... and u = ...`, OCamlFormat places the trailing doc after each `and` clause, not
    before. This is consistent with the single-type case. Do not attempt to restructure these.

### Code Review (Greybeard Pass)

17. **`Option.fold` is idiomatic OCaml for option-with-default-accumulator**: The pattern
    `Option.fold ~none:acc ~some:(fun x -> Some x) opt` is the standard OCaml way to say
    "if Some, replace the accumulator; if None, keep it." Don't replace it with a match — bisect_ppx
    treats `Option.fold` as a single coverage point, but a match creates two branches, one of which
    may be hard to cover in tests.

18. **Shared test helpers belong in `test/test_helpers.ml`**: Any utility function used across
    multiple test modules (e.g. `contains_substring`) should live in a shared helper module listed
    in the dune `(modules ...)` stanza. This avoids copy-paste and ensures consistent behavior.

19. **Or-patterns in match arms for deduplication**: When two match arms do the same thing with
    minor variation, use `(Some (Dead _) | None) as entry -> ...` and dispatch on the bound
    variable inside the arm body. This is cleaner than duplicating the entire block.

20. **`dune-project` license must match the actual LICENSE file**: The `(license ...)` field
    in `dune-project` is propagated to the generated `.opam` file. If these disagree with
    the actual `LICENSE` file, downstream tooling (opam, GitHub license detection) will show
    conflicting information. Always check that the declared license matches the file.
