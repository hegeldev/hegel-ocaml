RELEASE_TYPE: patch

This patch adds support for driving generation from more than one thread within a
single test. A test-case handle may only be drawn from by one thread at a time, so
to generate concurrently you clone it and give each thread its own clone. Each
clone draws from its own independent stream while the clones share the test case's
outcome and budget, so a failure still shrinks and its blob replays deterministically
(as long as your own code is deterministic).

`Hegel.clone tc` returns a fresh clone. `Hegel.spawn` clones `tc` and runs the
worker on a new thread, and `Hegel.join` waits for it and re-raises any exception 
the worker raised.
