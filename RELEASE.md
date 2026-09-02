RELEASE_TYPE: minor

This release makes the following changes:

- `char` is now `chars` (also `Hegel_jane.char` to `Hegel_jane.chars`).
- `Hegel_jane.times` is now `Hegel_jane.ofdays`.
- `Hegel_jane.time_ns` is now `Hegel_jane.time_nanoseconds`, and `Hegel_jane.time_ns_spans` is now `Hegel_jane.time_nanosecond_spans`.
- Single-test-case mode has been removed. Set `test_cases` to 1 in the settings instead.

Times of day are now drawn at nanosecond resolution. `times ()` and `datetimes ()` print the subsecond component as nine digits (`HH:MM:SS.fffffffff`) instead of six, and `Hegel_jane.ofdays ()` now produces every `Core.Time_ns.Ofday.t`.

Every date and time generator now accepts optional inclusive bounds. The default generators take them as `date`/`time` records (a `(date * time)` pair for `datetimes`). The `Hegel_jane` generators take them as the `Core` values they produce.

`of_parts` in `make_dates`, `make_times`, and `make_datetimes` takes `Hegel.date`/`Hegel.time` records instead of labeled integers.

`Hegel_jane.time_nanoseconds ()` and `Hegel_jane.time_nanosecond_spans ()` now default to the representable `Core.Time_ns` ranges rather than the full 63-bit integer range.

In stateful testing, a subset of invariants are now randomly chosen after running a rule rather than running all of them. All invariants still run on the initial and final state.

Invalid generator arguments and settings now raise the new `Hegel.Usage_error` and report the `libhegel` diagnostic. 
