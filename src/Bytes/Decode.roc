module [
    Decode,
    decode,
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    cStr,
    bool,
    take,
    map,
    succeed,
    fail,
    await,
    Step,
    loop,
]

import Bool exposing [Bool]

Decode value err := List U8
    -> Result
        {
            decoded : value,
            remaining : List U8,
        }
        err

decode : List U8, Decode value err -> Result value err
decode = \bytes, @Decode decoder ->
    decoder bytes
    |> Result.map .decoded

# Unsigned Integers

u8 : Decode U8 [UnexpectedEnd]
u8 =
    bytes <- @Decode

    when bytes is
        [byte, ..] ->
            Ok { decoded: byte, remaining: List.dropFirst bytes 1 }

        _ ->
            Err UnexpectedEnd

u16 : Decode U16 [UnexpectedEnd]
u16 =
    bytes <- @Decode

    when bytes is
        [b0, b1, ..] ->
            value =
                Num.shiftLeftBy (Num.toU16 b0) 8
                |> Num.bitwiseOr (Num.toU16 b1)

            Ok { decoded: value, remaining: List.dropFirst bytes 2 }

        _ ->
            Err UnexpectedEnd

u32 : Decode U32 [UnexpectedEnd]
u32 =
    bytes <- @Decode

    when bytes is
        [b0, b1, b2, b3, ..] ->
            value =
                Num.shiftLeftBy (Num.toU32 b0) 24
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toU32 b1) 16)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toU32 b2) 8)
                |> Num.bitwiseOr (Num.toU32 b3)

            Ok { decoded: value, remaining: List.dropFirst bytes 4 }

        _ ->
            Err UnexpectedEnd

u64 : Decode U64 [UnexpectedEnd]
u64 =
    bytes <- @Decode

    when bytes is
        [b0, b1, b2, b3, b4, b5, b6, b7, ..] ->
            value =
                Num.shiftLeftBy (Num.toU64 b0) 56
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toU64 b1) 48)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toU64 b2) 40)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toU64 b3) 32)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toU64 b4) 24)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toU64 b5) 16)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toU64 b6) 8)
                |> Num.bitwiseOr (Num.toU64 b7)

            Ok { decoded: value, remaining: List.dropFirst bytes 8 }

        _ ->
            Err UnexpectedEnd

# Signed Integers

i8 : Decode I8 [UnexpectedEnd]
i8 =
    map u8 Num.toI8

i16 : Decode I16 [UnexpectedEnd]
i16 =
    map u16 Num.toI16

i32 : Decode I32 [UnexpectedEnd]
i32 =
    map u32 Num.toI32

i64 : Decode I64 [UnexpectedEnd]
i64 =
    map u64 Num.toI64

take : U64, (List U8 -> value) -> Decode value [UnexpectedEnd]
take = \count, callback ->
    bytes <- @Decode
    { before, others } = List.split bytes count

    if List.len before == count then
        Ok { decoded: callback before, remaining: others }
    else
        Err UnexpectedEnd

# Strings

cStr : Decode Str [TerminatorNotFound, Utf8DecodeError _]
cStr =
    bytes <- @Decode

    when List.splitFirst bytes 0 is
        Ok { before, after } ->
            when Str.fromUtf8 before is
                Ok value ->
                    Ok { decoded: value, remaining: after }

                Err err ->
                    Err (Utf8DecodeError err)

        Err _ ->
            Err TerminatorNotFound

# Bools

bool : Decode Bool [UnexpectedEnd]
bool =
    bytes <- @Decode

    when bytes is
        [byte, ..] ->
            Ok { decoded: byte == 1, remaining: List.dropFirst bytes 1 }

        _ ->
            Err UnexpectedEnd

# Mapping

succeed : value -> Decode value err
succeed = \value ->
    bytes <- @Decode
    Ok { decoded: value, remaining: bytes }

fail : err -> Decode value err
fail = \err ->
    _ <- @Decode
    Err err

await : Decode a err, (a -> Decode b err) -> Decode b err
await = \@Decode decoderA, callback ->
    bytes <- @Decode
    a <- Result.try (decoderA bytes)
    (@Decode decoderB) = callback a.decoded
    decoderB a.remaining

map : Decode a err, (a -> b) -> Decode b err
map = \@Decode decoder, mapFn ->
    bytes <- @Decode
    { decoded, remaining } <- Result.map (decoder bytes)
    { decoded: mapFn decoded, remaining }

# Loop

Step state a : [Loop state, Done a]

loop : state, (state -> Decode (Step state a) err) -> Decode a err
loop = \state, step ->
    bytes <- @Decode

    loopHelp step state bytes

loopHelp = \step, state, bytes ->
    (@Decode decoder) = step state

    { decoded, remaining } <- Result.try (decoder bytes)

    when decoded is
        Loop newState ->
            loopHelp step newState remaining

        Done result ->
            Ok { decoded: result, remaining }
