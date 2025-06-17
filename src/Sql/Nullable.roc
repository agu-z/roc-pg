module [Nullable, map, with_default]

Nullable a : [Null, NotNull a]

map : Nullable a, (a -> b) -> Nullable b
map = |x, fn|
    when x is
        Null -> Null
        NotNull(a) -> NotNull(fn(a))

with_default : Nullable a, a -> a
with_default = |x, def|
    when x is
        Null -> def
        NotNull(a) -> a
