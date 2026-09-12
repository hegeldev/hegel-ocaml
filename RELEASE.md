RELEASE_TYPE: minor

This release reworks how settings are created. Settings are now in the
`Settings` submodule, so their fields and constructors no longer collide with
top-level names. All `with_*` settings functions have been removed in favor of 
record update syntax.

`Hegel.settings` is now `Hegel.Settings.create`, `Hegel.default_settings` is
now `Hegel.Settings.default`.

The `settings`, `verbosity`, `database`, `phase`, and `health_check` types 
are now in `Settings`. The `phases` field is now a `phase list`.

Settings can now also come from named profiles. There are three default profiles: 
`development` (for local runs), `ci` (selected automatically on CI servers), and 
`workload` (selected automatically inside Antithesis). 

Custom profiles can be defined with a `hegel.toml` at the project root:

```toml
default = "nightly"   # optional: the default profile for this project

[profiles.ci]
test_cases = 1000

[profiles.nightly]
test_cases = 10000
```

`Settings.default ()` is the resolved default profile. `Settings.from_profile
"name"` selects a profile by name, `Settings.register_profile` defines one
from code, and `Settings.set_default_profile` (or the `HEGEL_DEFAULT_PROFILE`
environment variable, or the `default` entry in `hegel.toml`) sets the
suite-wide default. `HEGEL_CONFIG` sets the `hegel.toml` path.

This release also fixes a use-after-free in stateful tests and `spawn`ed clones.