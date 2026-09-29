RELEASE_TYPE: minor

This release changes how Hegel handles nondeterminism.

A flaky test now fails with its own exception instead of "Flaky test detected". The failure report includes how reliably the failure reproduced:

```
--- Failure --------------------------------------------------------------------
Exception: Failure("first call only")
note: unconfirmed failure: failed 0 of 10 replays after the observed failure — a rare failure, or the environment changed between executions
```

The new `Settings.t` field `nondeterminism_strictness` controls how Hegel handles nondeterminism. The corresponding
environment variable is `HEGEL_NONDETERMINISM_STRICTNESS`. In `hegel.toml`, the key is `nondeterminism_strictness`.

A failing concurrent state machine now shrinks its counterexample.

Hegel now attempts to reproduce a flaky failure from a `[@@failure_blobs ...]` blob.
