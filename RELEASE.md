RELEASE_TYPE: minor

This release removes Jane Street's `core`, `core_unix`, and `sexp_diff` from
Hegel's dependencies.

`Generators.dates`, `times`, and `datetimes` now generate ISO 8601 strings.

`Generators.hash_tables` now generates a `Stdlib.Hashtbl.t` instead of 
`Core.Hashtbl.t`.

Projects that use `Core` can use the optional `hegel.core` sublibrary to generate
equivalent `Core` values. `Hegel_core.dates`, `times`, and `datetimes` generate
`Core.Date.t` and `Core.Time_ns.Ofday.t` values, and `Hegel_core.hash_tables`
generates a `Core.Hashtbl.t`.

`require_equal` failures now print the two values in full. The structural Jane 
Street `sexp_diff` rendering is now opt-in.

Thanks to @c-cube for his help with this release.
