module [
    Decode,
    Step,
    await,
    bool,
    c_str,
    decode,
    fail,
    i16,
    i32,
    i64,
    i8,
    loop,
    map,
    succeed,
    take,
    u16,
    u32,
    u64,
    u8,
]

Decode value err :=
    List U8
    -> Result
        {
            decoded : value,
            remaining : List U8,
        }
        err

decode : List U8, Decode value err -> Result value err
decode = |bytes, @Decode(decode_decoder)|
    decode_decoder(bytes)
    |> Result.map_ok(.decoded)

# Unsigned Integers

u8 : Decode U8 [UnexpectedEnd]
u8 =
    @Decode(
        |bytes|
            when bytes is
                [byte, ..] ->
                    Ok({ decoded: byte, remaining: List.drop_first(bytes, 1) })

                _ ->
                    Err(UnexpectedEnd),
    )

u16 : Decode U16 [UnexpectedEnd]
u16 =
    @Decode(
        |bytes|
            when bytes is
                [b0, b1, ..] ->
                    value =
                        Num.shift_left_by(Num.to_u16(b0), 8)
                        |> Num.bitwise_or(Num.to_u16(b1))

                    Ok({ decoded: value, remaining: List.drop_first(bytes, 2) })

                _ ->
                    Err(UnexpectedEnd),
    )

u32 : Decode U32 [UnexpectedEnd]
u32 =
    @Decode(
        |bytes|
            when bytes is
                [b0, b1, b2, b3, ..] ->
                    value =
                        Num.shift_left_by(Num.to_u32(b0), 24)
                        |> Num.bitwise_or(Num.shift_left_by(Num.to_u32(b1), 16))
                        |> Num.bitwise_or(Num.shift_left_by(Num.to_u32(b2), 8))
                        |> Num.bitwise_or(Num.to_u32(b3))

                    Ok({ decoded: value, remaining: List.drop_first(bytes, 4) })

                _ ->
                    Err(UnexpectedEnd),
    )

u64 : Decode U64 [UnexpectedEnd]
u64 =
    @Decode(
        |bytes|
            when bytes is
                [b0, b1, b2, b3, b4, b5, b6, b7, ..] ->
                    value =
                        Num.shift_left_by(Num.to_u64(b0), 56)
                        |> Num.bitwise_or(Num.shift_left_by(Num.to_u64(b1), 48))
                        |> Num.bitwise_or(Num.shift_left_by(Num.to_u64(b2), 40))
                        |> Num.bitwise_or(Num.shift_left_by(Num.to_u64(b3), 32))
                        |> Num.bitwise_or(Num.shift_left_by(Num.to_u64(b4), 24))
                        |> Num.bitwise_or(Num.shift_left_by(Num.to_u64(b5), 16))
                        |> Num.bitwise_or(Num.shift_left_by(Num.to_u64(b6), 8))
                        |> Num.bitwise_or(Num.to_u64(b7))

                    Ok({ decoded: value, remaining: List.drop_first(bytes, 8) })

                _ ->
                    Err(UnexpectedEnd),
    )

# Signed Integers

i8 : Decode I8 [UnexpectedEnd]
i8 =
    map(u8, Num.to_i8)

i16 : Decode I16 [UnexpectedEnd]
i16 =
    map(u16, Num.to_i16)

i32 : Decode I32 [UnexpectedEnd]
i32 =
    map(u32, Num.to_i32)

i64 : Decode I64 [UnexpectedEnd]
i64 =
    map(u64, Num.to_i64)

take : U64, (List U8 -> value) -> Decode value [UnexpectedEnd]
take = |count, callback|
    @Decode(
        |bytes|
            { before, others } = List.split_at(bytes, count)

            if List.len(before) == count then
                Ok({ decoded: callback(before), remaining: others })
            else
                Err(UnexpectedEnd),
    )

# Strings

c_str : Decode Str [TerminatorNotFound, Utf8DecodeError _]
c_str =
    @Decode(
        |bytes|
            when List.split_first(bytes, 0) is
                Ok({ before, after }) ->
                    when Str.from_utf8(before) is
                        Ok(value) ->
                            Ok({ decoded: value, remaining: after })

                        Err(err) ->
                            Err(Utf8DecodeError(err))

                Err(_) ->
                    Err(TerminatorNotFound),
    )

# Bools

bool : Decode Bool [UnexpectedEnd]
bool =
    @Decode(
        |bytes|
            when bytes is
                [byte, ..] ->
                    Ok({ decoded: byte == 1, remaining: List.drop_first(bytes, 1) })

                _ ->
                    Err(UnexpectedEnd),
    )

# Mapping

succeed : value -> Decode value err
succeed = |value|
    @Decode(
        |bytes|
            Ok({ decoded: value, remaining: bytes }),
    )

fail : err -> Decode value err
fail = |err|
    @Decode(
        |_|
            Err(err),
    )

await : Decode a err, (a -> Decode b err) -> Decode b err
await = |@Decode(decoder_a), callback|
    @Decode(
        |bytes|
            Result.try(
                decoder_a(bytes),
                |a|
                    @Decode(decoder_b) = callback(a.decoded)
                    decoder_b(a.remaining),
            ),
    )

map : Decode a err, (a -> b) -> Decode b err
map = |@Decode(map_decoder), map_fn|
    @Decode(
        |bytes|
            Result.map_ok(
                map_decoder(bytes),
                |{ decoded, remaining }|
                    { decoded: map_fn(decoded), remaining },
            ),
    )

# Loop

Step state a : [Loop state, Done a]

loop : state, (state -> Decode (Step state a) err) -> Decode a err
loop = |state, step|
    @Decode(
        |bytes|
            loop_help(step, state, bytes),
    )

loop_help = |step, state, bytes|
    @Decode(loop_help_decoder) = step(state)

    Result.try(
        loop_help_decoder(bytes),
        |{ decoded, remaining }|
            when decoded is
                Loop(new_state) ->
                    loop_help(step, new_state, remaining)

                Done(result) ->
                    Ok({ decoded: result, remaining }),
    )
