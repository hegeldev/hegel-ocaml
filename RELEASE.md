RELEASE_TYPE: minor

`one_of` no longer wraps each child schema in a tagged tuple. It now sends the raw children and relies on the new protocol contract in which the server emits `[index, value]` for `one_of` schemas. This requires the matching `hegel-core` release; the auto-installed server version pin will be bumped in a follow-up release once that ships.
