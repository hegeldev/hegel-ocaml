# Printing improvements

Working list for the `print-improvements` branch. Based on a survey of
hegel-ocaml's actual failure output (see `scratch_demo/` — run scenarios with
`dune exec scratch_demo/demo.exe -- <name>`) and of QCheck2, Haskell
QuickCheck, Hedgehog, falsify, qcheck-stm, and the research linked from
tybug.dev/property-testing (ICSE'24 "PBT in Practice", Tyche/OpenPBTStats).

Engine constraint: libhegel only explores. Per failure the C API returns just
an origin string and a replay blob — no message, no stats, no rendering. All
reporting below is client-side, computed from the case loop the client already
drives.

Hard constraint: counterexample values stay rendered as s-expressions
(Jane Street requirement). Improvements wrap or structure the sexp output,
never replace it.

## Done

- [x] Fix outright bugs: missing newline/flush and inconsistent
      capitalization on the blob line; multi-failure exceptions rendered with
      Core's `Exn.to_string` (sexp-quoted) instead of `Printexc.to_string`;
      leading `\n` embedded in the `Failure "\n2 failures found!"` message;
      blob-replay header printed to stdout while all other diagnostics go to
      stderr.
- [x] Framed failure report: `--- Failure: <name> (<file>:<line>) ---` header
      (location from `test_location` when the PPX supplies it), a
      `Falsified after N test cases (M discarded):` line counted client-side
      up to the first falsifying case, indented draw/note body, `Exception:`
      line, and a copy-pasteable `rerun with: [@@failure_blobs "..."]` line.
      A report with no printable draws stays tight (no blank body).
- [x] `print_blob` on by default (`with_print_blob false` to disable).
- [x] Multi-failure report: one shared header + `Falsified` line for the run,
      then `Failure i of n:` sections (e.g.
      `Failure 1 of 2:`). Order is the engine's discovery order (stable for a
      seeded run). The section header deliberately omits the engine origin —
      it duplicates the `Exception:` line printed two lines below.
- [x] `require` / `require_equal`. `Hegel.require tc ?msg cond` fails with a
      chosen message (`Failure msg`); `Hegel.require_equal tc ?msg sexp_of lhs
      rhs` compares the two values' sexps and prints a structural sexp diff
      (Jane Street's `sexp_diff`, new dependency) in the report body — `- `
      lines only in lhs, `+ ` lines only in rhs. The diff is rendered only
      when notes are visible, so shrink probes don't pay for it.
      `extract_origin` now also skips frames in `lib/internal.ml` so a
      require failure's origin is the caller's line, not require's raise
      site — without this every require failure in a run would collapse into
      one engine origin. Printers are passed explicitly (e.g.
      `Core.List.sexp_of_t Core.Int.sexp_of_t`); OCaml erases types at
      runtime, so there is no principled auto-printing.

## Todo

Ranked by leverage.

1. **Never print an empty counterexample.** A failing test whose draws are all
   `draw_silent` (e.g. derived generators) prints no body at all. On the final
   replay, a depth-0 silent draw should emit a `<no printer>` placeholder line
   (QCheck2 precedent) plus a one-line hint about `with_printer` /
   `[@@deriving sexp_of]`. Cautionary tale: Hedgehog issue #343 (inputs lost
   on exception paths).

2. **Stateful trace: results, state, and the failing step.** Current output is
   only `Step N: rule` lines. qcheck-stm prints `cmd : result` with a
   "Results incompatible with model" diagnosis; quickcheck-state-machine also
   shows model state per step. Client-side wins in `stateful.ml`:
   - `?sexp_of_state` on `Stateful.run`, printing state after each step;
   - mark the failing step — an invariant failure after step N should say
     `Invariant violated after step N` instead of a bare assertion escape;
   - `Rule.create ?sexp_of_result` so a rule's return value can print
     (`cmd : result`, mirroring qcheck-stm).

3. **Statistics / label collection.** No `label`/`collect`/`event` mechanism
   exists, and passing runs print nothing — the ICSE'24 paper's OCaml-specific
   critique (discard rates hidden on success). Client-side bookkeeping:
   `Hegel.event tc "label"` aggregated across cases with a QuickCheck-style
   percentage table at run end, plus an always-available
   `N passed, M discarded` summary. Stretch: opt-in OpenPBTStats JSONL
   (already implemented for base-quickcheck; ~5 fields suffice per the Tyche
   paper).

4. **Multiline sexp alignment.** Partially done: `note` is now
   multiline-aware, so continuation lines of `Sexp.to_string_hum` get the
   report body's indent instead of landing at column 0. Remaining: they still
   don't align under `name = (...)` — either pad by the name's width or print
   `name =` on its own line when the sexp is multiline.

5. **Colors.** None anywhere. QCheck2-style red/green on the failure header
   and runner PASS/FAIL lines — tty-detected, `NO_COLOR`-respecting.

6. **Verbose mode legibility.** Engine phase lines and client draw lines
   interleave with no per-case separator; add a client-printed case separator
   so `Verbose` is usable for watching shrink candidates (falsify's
   `--falsify-verbose` shrink history is the model).

7. **UTF-8 text readability (low priority).** Sexp escaping renders non-ASCII
   counterexamples as byte escapes (`"\194\128"`). The encoding can't change
   (sexp constraint); consider an auxiliary human-readable echo line for
   string draws containing escapes.

## Process notes

- Ground every output change in real output: extend `scratch_demo/demo.ml`,
  run it, and pin the format with ppx_expect snapshots in
  `ppx/test/expect_tests/` (empty `[%expect {||}]`, run, review diff,
  `dune promote`) — not with Alcotest client tests.
- Snapshot gotchas: call `run_hegel_test` directly in snapshot tests (a
  `let%hegel_test` header embeds shifting line numbers); disable the database
  when counts matter (reuse replays change them); the engine's first generated
  case draws minimal values, so "fails after several cases" tests must pass on
  minimal inputs.
- 100% coverage is enforced: every new formatting branch (singular/plural,
  body/no-body, blob on/off, location present/absent) needs a test that
  executes it.
