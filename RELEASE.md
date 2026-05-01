RELEASE_TYPE: patch

Bump our pinned hegel-core to [0.7.0](https://github.com/hegeldev/hegel-core/releases/tag/v0.7.0), incorporating the following changes:

> This release changes the `one_of` protocol request to return a tuple of `[index, value]`, rather than just `value`.
>
> — [v0.5.0](https://github.com/hegeldev/hegel-core/releases/tag/v0.5.0)

> This release makes the following breaking protocol changes:
> - Removed `{"type": "sampled_from"}`. Instead of serializing the values to sample from, ask for an integer index and index into the collection of values on the client side.
> - Removed `{"type": "null"}`. Use `{"type": "constant", "value": null}` instead.
> - Replaced `{"type": "ipv4"}` and `{"type": "ipv6"}` with a single `{"type": "ip_address", "version": <4|6>}` schema.
>
> The protocol version is now 0.12.
>
> — [v0.6.0](https://github.com/hegeldev/hegel-core/releases/tag/v0.6.0)

> This patch changes the default Hegel server settings when running inside Antithesis (i.e. when `ANTITHESIS_OUTPUT_DIR` is set in the environment) to disable health checks and database. Health checks are designed for the sort of small fast test you would run in your unit tests and are not sensible defaults for Antithesis, and the database is essentially useless inside Antithesis as replay is done via the fuzzer.
>
> — [v0.6.1](https://github.com/hegeldev/hegel-core/releases/tag/v0.6.1)

> This release adds support for the `phases` parameter in the `run_test` protocol message,
> allowing clients to control which Hypothesis phases run (e.g. `generate`, `shrink`,
> `reuse`, `target`, `explicit`, `explain`).
>
> — [v0.7.0](https://github.com/hegeldev/hegel-core/releases/tag/v0.7.0)
