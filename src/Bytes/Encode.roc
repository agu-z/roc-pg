module [
    sequence,
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    c_str,
    null_terminate,
]

sequence : List (List U8) -> List U8
sequence =
    List.join

# Unsigned integers

u8 : U8 -> List U8
u8 =
    List.single

u16 : U16 -> List U8
u16 =
    sized(16)

u32 : U32 -> List U8
u32 =
    sized(32)

u64 : U64 -> List U8
u64 =
    sized(64)

# Signed integers

i8 : I8 -> List U8
i8 = |value|
    [Num.to_u8(value)]

i16 : I16 -> List U8
i16 =
    sized(16)

i32 : I32 -> List U8
i32 =
    sized(32)

i64 : I64 -> List U8
i64 =
    sized(64)

sized : U8 -> (Int * -> List U8)
sized = |size|
    |value|
        sized_help(value, (size - 8), [])

sized_help : Int *, U8, List U8 -> List U8
sized_help = |value, offset, collected|
    part =
        value
        |> Num.shift_right_by(offset)
        |> Num.to_u8

    added =
        List.append(collected, part)

    if offset == 0 then
        added
    else
        sized_help(value, (offset - 8), added)

# Strings

c_str : Str -> List U8
c_str = |value|
    value
    |> Str.to_utf8
    |> null_terminate

null_terminate : List U8 -> List U8
null_terminate = |bytes|
    List.append(bytes, 0)
