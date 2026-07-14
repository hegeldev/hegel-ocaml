RELEASE_TYPE: patch

This patch adds `Hegel.with_clone` for driving generation from more than one
thread within a single test. A test-case handle may only be drawn from by one
thread at a time, so to generate concurrently you clone it and give each thread
its own clone. Each clone draws from its own independent stream, but the clones 
share the test case's outcome and budget. Concurrent stream still shrink and its 
blob replays deterministically (as long as your own code is deterministic).
