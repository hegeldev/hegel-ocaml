RELEASE_TYPE: minor

- `char` is now `chars` (also `Hegel_jane.char` to `Hegel_jane.chars`).
- `Hegel_jane.times` is now `Hegel_jane.ofdays`.
- `Hegel_jane.time_ns` is now `Hegel_jane.time_nanoseconds`, and `Hegel_jane.time_ns_spans` is now `Hegel_jane.time_nanosecond_spans`.
- Single-test-case mode has been removed.

Times of day are now drawn at nanosecond resolution. `times ()` and `datetimes ()` print the subsecond component as nine digits (`HH:MM:SS.fffffffff`) instead of six, and `Hegel_jane.ofdays ()` now produces every `Core.Time_ns.Ofday.t`.

Every date and time generator now accepts optional inclusive bounds. The default generators take them as `date`/`time` records (a `(date * time)` pair for `datetimes`). The `Hegel_jane` generators take them as the `Core` values they produce.

`Hegel_jane.time_nanoseconds ()` and `Hegel_jane.time_nanosecond_spans ()` now default to the representable `Core.Time_ns` ranges rather than the full 63-bit integer range.

Stateful `invariants` are now sampled between rules instead of running all of them.
