RELEASE_TYPE: patch

This patch adds generators for functions `functions`, `functions2`, and `functions3` 
in `Hegel.Generators`. A generated function draws each result lazily
from a `returns` generator, memoized per argument.
