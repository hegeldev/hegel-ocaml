RELEASE_TYPE: patch

`ip_addresses` now emits the unified protocol schema `{"type": "ip_addresses", "version": N}` instead of the per-version `{"type": "ipv4"}` / `{"type": "ipv6"}` shapes. This release coordinates with a matching change in `hegel-core`.
