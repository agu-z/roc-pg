module [Nullable, map, withDefault]

Nullable a : [Null, NotNull a]

map : Nullable a, (a -> b) -> Nullable b
map = \x, fn ->
    when x is
        Null -> Null
        NotNull a -> NotNull (fn a)

withDefault : Nullable a, a -> a
withDefault = \x, def ->
    when x is
        Null -> def
        NotNull a -> a
