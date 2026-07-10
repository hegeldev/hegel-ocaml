RELEASE_TYPE: minor

This release cleans up several generator APIs where documentation and behavior disagreed.

**Breaking**: `dates`, `times`, and `datetimes` now generate typed values
instead of ISO 8601 strings: `dates` produces `Core.Date.t`, `times` produces
`Core.Time_ns.Ofday.t` (microsecond precision), and `datetimes` produces
`Core.Date.t * Core.Time_ns.Ofday.t` pairs. Previously the engine's structured
date/time values were formatted away into strings at the draw. The
`format_date` / `format_time` / `format_datetime` helpers are now documented
public API and render the new typed values in the old ISO 8601 forms, so
string-oriented tests migrate with a single function call:

```ocaml
(* before *)
let s = draw tc (dates ()) in

(* after *)
let d = draw tc (dates ()) in
let s = format_date d in
```

`[@@deriving hegel_generator]`'s `t list` fields now generate through the
same engine-driven collection protocol as `Generators.lists`. Previously the
deriver drew a length in a hidden, unconfigurable `[0, 20]` range and looped,
so derived lists could never exceed 20 elements and the engine could not
shrink them by deleting individual elements. Derived `t option` fields
likewise now go through the same machinery as `Generators.optional`.

`[@@deriving hegel_generator]` no longer clamps derived `int` fields to
±2³⁰−1. Derived ints now use the same full native-`int` default range as a
hand-written `integers ()`. The clamp deliberately weakened generation so
user arithmetic could not overflow — hiding exactly the bugs a property test
should find. Properties that relied on products of derived ints never
overflowing must handle overflow themselves (or bound their generators
explicitly).

**Breaking**: `ip_addresses` now takes `?version:[`V4 | `V6]` instead of
`?version:int`, and generates typed `Ipaddr.t` values instead of strings
(`ipaddr` was already a dependency). Previously `~version:5` type-checked and
then failed at draw time, and the typed address the implementation built was
thrown away via `to_string`. Render a drawn address with `Ipaddr.to_string`:

```ocaml
(* before *)
let s = draw tc (ip_addresses ~version:4 ()) in

(* after *)
let ip = draw tc (ip_addresses ~version:`V4 ()) in
let s = Ipaddr.to_string ip in
```

`one_of` now prints a drawn value through the printer of the branch it was
actually drawn from. Previously every branch's values printed through the
first branch's printer.

The internal PPX/engine plumbing that `Hegel.Generators` previously exposed
at its top level (`Labels`, `group`, `discardable_group`, `new_collection`,
`collection_more`, `collection_reject`, `pool_values`, `resolve_draw`,
`max_filter_attempts`, `composite_with_label`, and the `collection` type) has
moved into `Hegel.Generators.Ppx_internal`, an explicitly internal submodule
with no stability guarantees. These values were never documented API;
`open Hegel.Generators` no longer brings them into scope.

`with_suppress_health_check` now sets the suppressed-health-check list like
every other `with_*` builder, replacing any previously suppressed list.
Previously it appended, so repeated calls accumulated duplicates and a
suppression could never be undone. To suppress several checks, pass them in
one list.

Documentation corrections: `sampled_from` no longer claims uniform sampling
(the engine's bounded-integer draw deliberately over-weights boundary
indices, favoring earlier elements); the `Explicit` phase is documented as
reserved for future use (hegel-ocaml has no explicit-examples facility yet,
so selecting it has no effect); `assume` documents that its test-case handle
is accepted for API symmetry and the rejection is client-side; and drawing
from an empty pool is documented (and pinned by a test) as raising
`Assume_rejected` rather than `Data_exhausted`.
