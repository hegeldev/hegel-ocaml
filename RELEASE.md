RELEASE_TYPE: patch

This patch changes how Hegel prints.

- An aggregate value exceeding the maximum line width when printed now prints with one 
  element per line, instead of filling each line with elements until the maximum width.
- A test case's output now prints after the test case completes, instead of streaming
  while the test runs.
- Output from cloned test cases now appears at the point the clone was made, instead 
  of being dependent on thread scheduling.
- A `note` message now must be valid UTF-8.
