RELEASE_TYPE: minor

This release reworks how settings are created. Settings are now in the
`Settings` submodule, so their fields and constructors no longer collide with
top-level names. All `with_*` settings functions have been removed in favor of 
record update syntax.

`Hegel.settings` is now `Hegel.Settings.create`, `Hegel.default_settings` is
now `Hegel.Settings.default`.
The `settings`, `verbosity`, `database`, `phase`, and `health_check` types 
are now in `Settings`. The `phases` field is now a `phase list`.

```ocaml
(* before *)
[@@settings settings ~test_cases:500 () |> with_verbosity Verbose |> with_database Disabled]

(* after *)
[@@settings
  { (Settings.create ~test_cases:500 ()) with
    verbosity = Settings.Verbose
  ; database = Settings.Disabled
  }]
```

This release also fixes a use-after-free in stateful tests and `spawn`ed clones.

