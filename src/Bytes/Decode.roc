interface Bytes.Decode
    exposes [
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
        map,
        map2,
        succeed,
        fail,
        await,
        Step,
        loop,
    ]
    imports []

Decode value err :=
    List U8
    -> Result
        {
            decoded : value,
            remaining : List U8,
        }
        err

decode : List U8, Decode value err -> Result { decoded : value, remaining : List U8 } err
decode = \bytes, @Decode decoder ->
    decoder bytes

# Unsigned Integers

u8 : Decode U8 [NotEnoughBytes]
u8 =
    bytes <- @Decode

    when bytes is
        [byte, ..] ->
            Ok { decoded: byte, remaining: List.drop bytes 1 }

        _ ->
            Err NotEnoughBytes

u16 : Decode U16 [NotEnoughBytes]
u16 =
    sized 16 Num.toU16

u32 : Decode U32 [NotEnoughBytes]
u32 =
    sized 32 Num.toU32

u64 : Decode U64 [NotEnoughBytes]
u64 =
    sized 64 Num.toU64

# Signed Integers

i8 : Decode I8 [NotEnoughBytes]
i8 =
    bytes <- @Decode

    when bytes is
        [byte, ..] ->
            Ok { decoded: Num.toI8 byte, remaining: List.drop bytes 1 }

        _ ->
            Err NotEnoughBytes

i16 : Decode I16 [NotEnoughBytes]
i16 =
    sized 16 Num.toI16

i32 : Decode I32 [NotEnoughBytes]
i32 =
    sized 32 Num.toI32

i64 : Decode I64 [NotEnoughBytes]
i64 =
    sized 64 Num.toI64

sized : U8, (U8 -> Int a) -> Decode (Int a) [NotEnoughBytes]
sized = \size, toType ->
    bytes <- take (Num.toNat size // 8)

    result = List.walk bytes { value: 0, offset: size - 8 } \{ value, offset }, byte -> {
        value: byte
        |> toType
        |> Num.shiftLeftBy offset
        |> Num.bitwiseOr value,
        offset: if offset == 0 then 0 else offset - 8,
    }

    result.value

take : Nat, (List U8 -> value) -> Decode value [NotEnoughBytes]
take = \count, callback ->
    bytes <- @Decode
    { before, others } = List.split bytes count

    if List.len before == count then
        Ok { decoded: callback before, remaining: others }
    else
        Err NotEnoughBytes

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

map2 : Decode a err, Decode b err, (a, b -> c) -> Decode c err
map2 = \@Decode decoderA, @Decode decoderB, mapFn ->
    bytes <- @Decode
    a <- Result.try (decoderA bytes)
    b <- Result.map (decoderB a.remaining)
    { decoded: mapFn a.decoded b.decoded, remaining: b.remaining }

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
