RELEASE_TYPE: minor

The `hashmaps` generator is replaced by two generators that say what they
mean: `association_lists`, which produces what `hashmaps` actually
produced — a `(key * value) list` in generation order with unique keys —
and `hash_tables`, which produces a real polymorphic `Hashtbl.t` with the
same entry generation. Existing uses of `hashmaps` translate directly to
`association_lists`.
