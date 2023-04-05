interface Pg.Cmd
    exposes [
        Cmd,
        new,
        bind,
        Binding,
        null,
        str,
        u8,
        u16,
        u32,
        u64,
        u128,
        i8,
        i16,
        i32,
        i64,
        i128,
        f32,
        f64,
        bytes,
    ]
    imports [
        Cmd.{ makeBinding },
    ]

Cmd : Cmd.Cmd

Binding : Cmd.Binding

new : Str -> Cmd
new = Cmd.unprepared

bind : Cmd, List Binding -> Cmd
bind = Cmd.bind

null : Binding
null = makeBinding Null

str : Str -> Binding
str = \value ->
    makeBinding (Text value)

u8 = num
u16 = num
u32 = num
u64 = num
u128 = num
i8 = num
i16 = num
i32 = num
i64 = num
i128 = num
f32 = num
f64 = num

num : Num * -> Binding
num = \value ->
    makeBinding (Text (Num.toStr value))

bytes : List U8 -> Binding
bytes = \value ->
    makeBinding (Binary value)

