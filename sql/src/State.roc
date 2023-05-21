interface State exposes [State, map]

State s a := s -> { state: s, value: a }

update : State s a, (s, a -> { state: s, value: a }) -> State s2 b
update = \@State toA, fn ->
    @State \s -> fn s (toA s)

map : State s a, (a -> b) -> State s b
map = \@State toA, toB ->
    @State \s -> { state: s, value: toB (toA s) }

run : State s a, s -> a
run = \@State toA, s ->
    toA s


expect 
