RELEASE_TYPE: patch

`one_of` no longer wraps each child schema in a tagged tuple. It now sends the raw children and relies on the new protocol contract in which the server emits `[index, value]` for `one_of` schemas.

This release also bumps our pinned hegel-core to [0.5.0](https://github.com/hegeldev/hegel-core/releases/tag/v0.5.0), which ships the matching server-side protocol change:

> This release changes the `one_of` protocol request to return a tuple of `[index, value]`, rather than just `value`.
>
> — [v0.5.0](https://github.com/hegeldev/hegel-core/releases/tag/v0.5.0)
