RELEASE_TYPE: patch

This patch fixes a crash when several test binaries start at the same time
with a cold libhegel cache. Every process downloaded the library to the same
temporary file and renamed it into place, so the first process to finish
pulled the file out from under the others, which then failed with checksum or
missing-file errors (and could corrupt the cached library). Each process now
downloads to its own temporary file and atomically renames it into place, so
concurrent first runs all succeed. Partial downloads left behind by a killed
process no longer interfere with later downloads.
