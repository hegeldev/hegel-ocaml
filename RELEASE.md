RELEASE_TYPE: minor

This release changes the public API for running Hegel tests to a new `let%hegel_test` PPX extension.

```ocaml
(* before *)
let my_invariant =
  Hegel.run_hegel_test (fun tc -> ...)

(* after *)
let%hegel_test my_invariant tc = ...
```

The top-level `Hegel.run_hegel_test` has been removed from the public API. 
Existing callers should migrate to `let%hegel_test`

Add the PPX to your library's dune stanza:

```
(library
 (name my_tests)
 (libraries hegel)
 (inline_tests (backend ppx_hegel_test))
 (preprocess (pps ppx_hegel_test)))
```

After that, `dune runtest` discovers and runs every `let%hegel_test` in the library. 
Tests remain directly callable as ordinary `unit -> unit` functions, so they continue 
to work with Alcotest or other harnesses.

Per-test settings can be supplied with the `[@@settings ...]` attribute:

```ocaml
let%hegel_test my_invariant tc = ...
[@@settings { (Hegel.default_settings ()) with max_examples = 1000 }]
```

This release also adds integration with Antithesis. Each `let%hegel_test` emits an 
always assertion to recording whether the property test passed or failed. 
Outside Antithesis, the integration is a no-op.
