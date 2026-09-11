RELEASE_TYPE: patch

This patch loads `libhegel` lazily, on the first `Hegel_ffi.Ffi.context_new`, rather
than at module initialization. This avoids having to load the dynamic library in code
that only builds generators.
