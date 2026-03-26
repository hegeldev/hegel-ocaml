RELEASE_TYPE: minor

This release switches to stdio-based communication with hegel-core, matching the approach used by hegel-rust.

* Switch from Unix socket to `--stdio` pipe-based transport, eliminating temp directories and socket polling.
* Add a background reader thread for packet dispatch, replacing the demand-driven reader model.
* Add `Settings` API with `health_check`, `verbosity`, `database`, `derandomize`, and `suppress_health_check` support.
* Auto-install `hegel-core` via `uv` when not found on PATH.
* Add server crash detection via a monitor thread.
* Bump supported protocol versions to 0.1–0.7.
