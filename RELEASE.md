RELEASE_TYPE: patch

Bump our pinned hegel-core to [0.6.0](https://github.com/hegeldev/hegel-core/releases/tag/v0.6.0), incorporating the following changes:

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
