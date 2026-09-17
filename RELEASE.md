RELEASE_TYPE: patch

This patch bumps our pinned libhegel ([hegel-rust](https://github.com/hegeldev/hegel-rust)) from [0.42.0](https://github.com/hegeldev/hegel-rust/releases/tag/libhegel-v0.42.0) to [0.42.4](https://github.com/hegeldev/hegel-rust/releases/tag/libhegel-v0.42.4), which raises the limit on the number of choices a single test case may make from 8,192 to 2^20 and improves shrinking in several situations.

The library now downloads `libhegel` from hegel-rust's `libhegel-v<version>` release tags, where libhegel releases and their binaries live since libhegel 0.42.1 (the plain `v<version>` tags now belong to the `hegeltest` crate). Earlier libhegel releases carry both tags, so a `HEGEL_LIBHEGEL_PATH` or cached copy from before this change keeps working.
