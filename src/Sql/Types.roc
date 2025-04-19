module [
    decode,
    Decode,
    DecodeErr,
    map,
    succeed,
    fail,
    nullable,
    discard_phantom,
    i16,
    i32,
    i64,
    f32,
    f64,
    dec,
    str,
    bool,
    uuid,
    unsupported,
    Raw,
    row,
    array,
    PgI16,
    PgI32,
    PgI64,
    PgText,
    PgBool,
    PgUuid,
    PgNum,
    PgCmp,
]

import Sql.Nullable exposing [Nullable]

# Decoding

Decode pg a := List U8 -> Result a DecodeErr

DecodeErr : [
    InvalidUtf8,
    InvalidNumStr,
    InvalidBool (List U8),
    MissingColumn U64,
    Error Str,
]

decode : List U8, Decode pg a -> Result a DecodeErr
decode = |bytes, @Decode(fn)|
    fn(bytes)

map : Decode pg a, (a -> b) -> Decode pg b
map = |@Decode(a), to_b| @Decode(|bytes| a(bytes) |> Result.map_ok(to_b))

succeed : a -> Decode pg a
succeed = |value| @Decode(|_| Ok(value))

fail : Str -> Decode pg a
fail = |message| @Decode(|_| Err(Error(message)))

nullable : Decode pg a -> Decode (Nullable pg) (Nullable a)
nullable = |@Decode(sub)|
    @Decode(
        |bytes|
            if List.is_empty(bytes) then
                # TODO: Use Null tag instead of empty list
                Ok(Null)
            else
                sub(bytes)
                |> Result.map_ok(NotNull),
    )

discard_phantom : Decode * a -> Decode * a
discard_phantom = |@Decode(sub)| @Decode(sub)

i16 : Decode (PgI16 _) I16
i16 = text_format(Str.to_i16)

i32 : Decode (PgI32 _) I32
i32 = text_format(Str.to_i32)

i64 : Decode (PgI64 _) I64
i64 = text_format(Str.to_i64)

f32 : Decode (PgF32 _) F32
f32 = text_format(Str.to_f32)

f64 : Decode (PgF64 _) F64
f64 = text_format(Str.to_f64)

dec : Decode (PgDec _) Dec
dec = text_format(Str.to_dec)

str : Decode (PgText _) Str
str = text_format(Ok)

# TODO: Make a proper uuid type
uuid : Decode (PgUuid _) Str
uuid = text_format(Ok)

bool : Decode (PgBool *) Bool
bool =
    @Decode(
        |bytes|
            when bytes is
                ['t'] ->
                    Ok(Bool.true)

                ['f'] ->
                    Ok(Bool.false)

                _ ->
                    Err(InvalidBool(bytes)),
    )

Raw : {
    bytes : List U8,
    type_name : Str,
}

unsupported : Str -> Decode * Raw
unsupported = |type_name|
    @Decode(
        |bytes|
            Ok(
                {
                    bytes,
                    type_name,
                },
            ),
    )

array : Decode pg a -> Decode (PgArray pg *) (List a)
array = |@Decode(decode_item)|
    @Decode(
        |array_bytes|
            array_bytes
            |> parse_array
            |> List.map_try(
                |item|
                    when item is
                        Null ->
                            decode_item([])

                        NotNull(val) ->
                            decode_item(val),
            ),
    )

row : (List (List U8) -> Result a DecodeErr) -> Decode pg a
row = |decode_row|
    @Decode(
        |row_bytes|
            row_bytes
            |> parse_row
            |> List.map(
                |field|
                    when field is
                        Null ->
                            []

                        NotNull(val) ->
                            val,
            )
            |> decode_row,
    )

parse_row = |bytes|
    bytes
    |> List.drop_first(1)
    |> List.walk(
        {
            items: List.with_capacity(6),
            curr: List.with_capacity(32),
            quote: Pending,
            escaped: Bool.false,
        },
        row_char,
    )
    |> .items

row_char = |state, byte|
    # This parser assumes postgres won't respond with malformed syntax
    when byte is
        _ if state.escaped ->
            { state &
                escaped: Bool.false,
                curr: state.curr |> List.append(byte),
            }

        '"' ->
            when state.quote is
                Pending ->
                    { state & quote: Open }

                Open ->
                    { state & quote: Closed }

                Closed ->
                    { state &
                        quote: Open,
                        # reopening means the string contains a literal quote
                        curr: state.curr |> List.append(byte),
                    }

        '\\' ->
            { state & escaped: Bool.true }

        ',' | ')' if state.quote != Open ->
            item =
                if List.is_empty(state.curr) and state.quote == Pending then
                    Null
                else
                    NotNull(state.curr)

            { state &
                items: state.items |> List.append(item),
                curr: List.with_capacity(32),
                quote: Pending,
            }

        _ ->
            { state & curr: state.curr |> List.append(byte) }

prs = |input|
    input
    |> Str.to_utf8
    |> parse_row
    |> List.map(
        |item|
            when item is
                Null -> Null
                NotNull(present) ->
                    present
                    |> Str.from_utf8
                    |> Result.with_default("")
                    |> NotNull,
    )

expect prs("(42)") == [NotNull("42")]
expect prs("(42,hi)") == [NotNull("42"), NotNull("hi")]
expect prs("(42,\" hi\")") == [NotNull("42"), NotNull(" hi")]
expect prs("(\"hello world\",hi)") == [NotNull("hello world"), NotNull("hi")]
expect prs("(\"hello \\\"Agus\\\"\",21)") == [NotNull("hello \"Agus\""), NotNull("21")]
expect prs("(\"hello \"\"Agus\"\"\",21)") == [NotNull("hello \"Agus\""), NotNull("21")]
expect prs("(\"a\"\"\")") == [NotNull("a\"")]
expect prs("(\"a\\\\b\")") == [NotNull("a\\b")]
expect prs("(\"a\nb\")") == [NotNull("a\nb")]
expect prs("(\"hi, world!\",\"hello, agus\")") == [NotNull("hi, world!"), NotNull("hello, agus")]
expect prs("(spaces,no quotes)") == [NotNull("spaces"), NotNull("no quotes")]
expect prs("(,prev is null)") == [Null, NotNull("prev is null")]
expect prs("(next is null,)") == [NotNull("next is null"), Null]
expect prs("(next is null,,prev is null)") == [NotNull("next is null"), Null, NotNull("prev is null")]
expect prs("()") == [Null]
expect prs("(,)") == [Null, Null]
expect prs("(,,)") == [Null, Null, Null]
expect prs("(\"\")") == [NotNull("")]
expect prs("(\"\",)") == [NotNull(""), Null]

parse_array = |bytes|
    bytes
    |> List.drop_first(1)
    |> List.walk(
        {
            items: List.with_capacity(16),
            curr: List.with_capacity(32),
            quote: Pending,
            escaped: Bool.false,
        },
        array_char,
    )
    |> .items

array_char = |state, byte|
    # This parser assumes postgres won't respond with malformed syntax
    ## TODO: Handle null
    when byte is
        _ if state.escaped ->
            { state &
                escaped: Bool.false,
                curr: state.curr |> List.append(byte),
            }

        '"' ->
            when state.quote is
                Pending ->
                    { state & quote: Open }

                Open ->
                    { state & quote: Closed }

                Closed ->
                    { state &
                        quote: Open,
                        # reopening means the string contains a literal quote
                        curr: state.curr |> List.append(byte),
                    }

        '\\' ->
            { state & escaped: Bool.true }

        ',' | '}' if state.quote != Open ->
            items =
                if List.is_empty(state.curr) and state.quote == Pending then
                    state.items
                else
                    state.items |> List.append(NotNull(state.curr))

            { state &
                items,
                curr: List.with_capacity(32),
                quote: Pending,
            }

        _ ->
            { state & curr: state.curr |> List.append(byte) }

pas = |input|
    input
    |> Str.to_utf8
    |> parse_array
    |> List.map(
        |item|
            when item is
                Null -> Null
                NotNull(present) ->
                    present
                    |> Str.from_utf8
                    |> Result.with_default("")
                    |> NotNull,
    )

expect pas("{}") == []
expect pas("{42}") == [NotNull("42")]
expect
    result =
        pas("{\"(hi,\\\"lala\\\"\\\"\\\")\"}")
        |> List.map(
            |item|
                when item is
                    NotNull(p) ->
                        NotNull(prs(p))

                    Null ->
                        Null,
        )

    result == [NotNull([NotNull("hi"), NotNull("lala\"")])]

text_format : (Str -> Result a DecodeErr) -> Decode pg a
text_format = |fn|
    @Decode(
        |bytes|
            when Str.from_utf8(bytes) is
                Ok(text) ->
                    fn(text)

                Err(BadUtf8(_)) ->
                    Err(InvalidUtf8),
    )

# Types

PgCmp a : { cmp : {} }a

PgNum a : PgCmp { num : {} }a

PgI16 a : PgNum { i16 : {} }a
PgI32 a : PgNum { i32 : {} }a
PgI64 a : PgNum { i64 : {} }a
PgF32 a : PgNum { f32 : {} }a
PgF64 a : PgNum { f64 : {} }a
PgDec a : PgNum { dec : {} }a

PgText a : PgCmp { text : {} }a

PgBool a : PgCmp { bool : {} }a

PgUuid a : PgCmp { text : {} }a

PgArray item a : { array : item }a

