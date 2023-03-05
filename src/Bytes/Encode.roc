interface Bytes.Encode
    exposes [
        sequence,
        u8,
        u16,
        u32,
        u64,
        i8,
        i16,
        i32,
        i64,
        cStr,
        nullTerminate,
    ]
    imports []

sequence : List (List U8) -> List U8
sequence =
    List.join

# Unsigned integers

u8 : U8 -> List U8
u8 =
    List.single

u16 : U16 -> List U8
u16 =
    sized 16

u32 : U32 -> List U8
u32 =
    sized 32

u64 : U64 -> List U8
u64 =
    sized 64

# Signed integers

i8 : I8 -> List U8
i8 = \value ->
    [Num.toU8 value]

i16 : I16 -> List U8
i16 =
    sized 16

i32 : I32 -> List U8
i32 =
    sized 32

i64 : I64 -> List U8
i64 =
    sized 64

sized : U8 -> (Int * -> List U8)
sized = \size ->
    \value ->
        sizedHelp value (size - 8) []

sizedHelp : Int *, U8, List U8 -> List U8
sizedHelp = \value, offset, collected ->
    part =
        value
        |> Num.shiftRightBy offset
        |> Num.toU8

    added =
        List.append collected part

    if offset == 0 then
        added
    else
        sizedHelp value (offset - 8) added

# Strings

cStr : Str -> List U8
cStr = \value ->
    value
    |> Str.toUtf8
    |> nullTerminate

nullTerminate : List U8 -> List U8
nullTerminate = \bytes ->
    List.append bytes 0
