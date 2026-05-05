RELEASE_TYPE: patch

`ip_addresses` now emits the protocol schema `{"type": "ip_address", "version": 4|6}` instead of different types per version (`{"type": "ipv4"}` / `{"type": "ipv6"}`)
