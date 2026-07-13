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
- [x] Multiline sexp alignment. `note` is multiline-aware (each line of a
      message gets the report body's indent), and `draw_named` renders values
      via `Format.asprintf "%s = %a" name Sexp.pp_hum` so the pretty-printer
      breaks the sexp knowing it starts after `name = ` — continuation lines
      align under the value's opening paren instead of landing at column 0.
- [x] Colors. Failure report headers (`--- Failure ---` rule, `Failure i of
      n:`) print red on a tty; `require_equal` diffs use `sexp_diff`'s
      red/green display; the runner's PASS/FAIL and summary lines print
      green/red. Enabled when the stream is a tty (stderr for the report,
      stdout for the runner); `HEGEL_COLOR=1|0` forces on/off. The decision logic is the pure
      `Internal.color_enabled`; the runtime carries a small stdlib-only copy
      (new `unix` dep for `isatty`).
- [x] Stateful trace: state and the failing step. `Stateful.run` takes
      `?sexp_of_state`, echoing the model state as `state = <value>` after the
      initial state and after every step, so the trace shows how the state
      evolved. Draws a rule makes are nested under its `Step N` header (`note`
      carries a `note_indent` depth). An escaping invariant is attributed to its
      step — the report notes `Invariant N violated after step M` (or
      `... in the initial state`, where `N` is the invariant's index) before
      re-raising the original exception. All routed through `note`, so the trace
      only surfaces on the final failing replay (or under verbose). Pinned in
      `ppx/test/expect_tests/test_stateful_trace.ml`. (A per-rule
      `Rule.create ?sexp_of_result` mirroring qcheck-stm's `cmd : result` was
      considered and dropped: a rule's `step` returns the new state, so its
      "result" is just that state — identical to what `?sexp_of_state` already
      prints. A genuine `cmd : result` would need `step` to return a value
      distinct from the model state, a larger API change not worth it here.)

## Todo

Ranked by leverage.

1. **Never print an empty counterexample.** A failing test whose draws are all
   `draw_silent` (e.g. derived generators) prints no body at all. On the final
   replay, a depth-0 silent draw should emit a `<no printer>` placeholder line
   (QCheck2 precedent) plus a one-line hint about `with_printer` /
   `[@@deriving sexp_of]`. Cautionary tale: Hedgehog issue #343 (inputs lost
   on exception paths). (Implemented on branch
   `never-print-empty-counterexample`, split out of this PR for team review.)

2. **Statistics / label collection.** No `label`/`collect`/`event` mechanism
   exists, and passing runs print nothing — the ICSE'24 paper's OCaml-specific
   critique (discard rates hidden on success). Client-side bookkeeping:
   `Hegel.event tc "label"` aggregated across cases with a QuickCheck-style
   percentage table at run end, plus an always-available
   `N passed, M discarded` summary. Stretch: opt-in OpenPBTStats JSONL
   (already implemented for base-quickcheck; ~5 fields suffice per the Tyche
   paper).

3. **Verbose mode legibility.** Engine phase lines and client draw lines
   interleave with no per-case separator; add a client-printed case separator
   so `Verbose` is usable for watching shrink candidates (falsify's
   `--falsify-verbose` shrink history is the model). (Investigated: the engine
   already prints a `Running test case` separator per case in `Verbose`,
   including shrink candidates, so a client change may be unnecessary. The
   libhegel 0.29.0 output-redirect callback that would let the client reroute
   engine output is a separate change — PR #111.)

4. **UTF-8 text readability (low priority).** Sexp escaping renders non-ASCII
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
