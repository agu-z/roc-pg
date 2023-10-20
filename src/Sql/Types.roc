interface Sql.Types exposes [
        decode,
        Decode,
        DecodeErr,
        map,
        succeed,
        fail,
        nullable,
        Nullable,
        i16,
        i32,
        i64,
        f32,
        f64,
        dec,
        str,
        bool,
        unsupported,
        row,
        array,
        PgI16,
        PgI32,
        PgI64,
        PgText,
        PgBool,
        PgCmp,
    ]
    imports []

# Decoding

Decode pg a := List U8 -> Result a DecodeErr

DecodeErr : [
    InvalidUtf8,
    InvalidNumStr,
    InvalidBool (List U8),
    MissingColumn Nat,
    Error Str,
]

decode : List U8, Decode pg a -> Result a DecodeErr
decode = \bytes, @Decode fn ->
    fn bytes

map : Decode pg a, (a -> b) -> Decode pg b
map = \@Decode a, toB -> @Decode \bytes -> a bytes |> Result.map toB

succeed : a -> Decode pg a
succeed = \value -> @Decode \_ -> Ok value

fail : Str -> Decode pg a
fail = \message -> @Decode \_ -> Err (Error message)

Nullable a : [Null, Present a]

nullable : Decode pg a -> Decode (Nullable pg) (Nullable a)
nullable = \@Decode sub ->
    bytes <- @Decode

    if List.isEmpty bytes then
        # TODO: Use Null tag instead of empty list
        Ok Null
    else
        sub bytes
        |> Result.map Present

i16 : Decode PgI16 I16
i16 = textFormat Str.toI16

i32 : Decode PgI32 I32
i32 = textFormat Str.toI32

i64 : Decode PgI64 I64
i64 = textFormat Str.toI64

f32 : Decode PgF32 F32
f32 = textFormat Str.toF32

f64 : Decode PgF64 F64
f64 = textFormat Str.toF64

dec : Decode (PgNum *) Dec
dec = textFormat Str.toDec

str : Decode PgText Str
str = textFormat Ok

bool : Decode PgBool Bool
bool =
    bytes <- @Decode

    when bytes is
        ['t'] ->
            Ok Bool.true

        ['f'] ->
            Ok Bool.false

        _ ->
            Err (InvalidBool bytes)

unsupported = \typeName ->
    bytes <- @Decode

    Unsupported {
        bytes,
        typeName,
    }
    |> Ok

array : Decode pg a -> Decode (PgArray pg) (List a)
array = \@Decode decodeItem ->
    arrayBytes <- @Decode

    arrayBytes
    |> parseArray
    |> List.mapTry \item ->
        when item is
            Null ->
                decodeItem []

            Present val ->
                decodeItem val

row : (List (List U8) -> Result a DecodeErr) -> Decode pg a
row = \decodeRow ->
    rowBytes <- @Decode

    rowBytes
    |> parseRow
    |> List.map \field ->
        when field is
            Null ->
                []

            Present val ->
                val
    |> decodeRow

parseRow = \bytes ->
    bytes
    |> List.dropFirst
    |> List.walk
        {
            items: List.withCapacity 6,
            curr: List.withCapacity 32,
            quote: Pending,
            escaped: Bool.false,
        }
        rowChar
    |> .items

rowChar = \state, byte ->
    # This parser assumes postgres won't respond with malformed syntax
    when byte is
        _ if state.escaped ->
            { state &
                escaped: Bool.false,
                curr: state.curr |> List.append byte,
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
                        curr: state.curr |> List.append byte,
                    }

        '\\' ->
            { state & escaped: Bool.true }

        ',' | ')' if state.quote != Open ->
            item =
                if List.isEmpty state.curr && state.quote == Pending then
                    Null
                else
                    Present state.curr

            { state &
                items: state.items |> List.append item,
                curr: List.withCapacity 32,
                quote: Pending,
            }

        _ ->
            { state & curr: state.curr |> List.append byte }

prs = \input ->
    input
    |> Str.toUtf8
    |> parseRow
    |> List.map \item ->
        when item is
            Null -> Null
            Present present ->
                present
                |> Str.fromUtf8
                |> Result.withDefault ""
                |> Present

expect prs "(42)" == [Present "42"]
expect prs "(42,hi)" == [Present "42", Present "hi"]
expect prs "(42,\" hi\")" == [Present "42", Present " hi"]
expect prs "(\"hello world\",hi)" == [Present "hello world", Present "hi"]
expect prs "(\"hello \\\"Agus\\\"\",21)" == [Present "hello \"Agus\"", Present "21"]
expect prs "(\"hello \"\"Agus\"\"\",21)" == [Present "hello \"Agus\"", Present "21"]
expect prs "(\"a\"\"\")" == [Present "a\""]
expect prs "(\"a\\\\b\")" == [Present "a\\b"]
expect prs "(\"a\nb\")" == [Present "a\nb"]
expect prs "(\"hi, world!\",\"hello, agus\")" == [Present "hi, world!", Present "hello, agus"]
expect prs "(spaces,no quotes)" == [Present "spaces", Present "no quotes"]
expect prs "(,prev is null)" == [Null, Present "prev is null"]
expect prs "(next is null,)" == [Present "next is null", Null]
expect prs "(next is null,,prev is null)" == [Present "next is null", Null, Present "prev is null"]
expect prs "()" == [Null]
expect prs "(,)" == [Null, Null]
expect prs "(,,)" == [Null, Null, Null]
expect prs "(\"\")" == [Present ""]
expect prs "(\"\",)" == [Present "", Null]

parseArray = \bytes ->
    bytes
    |> List.dropFirst
    |> List.walk
        {
            items: List.withCapacity 16,
            curr: List.withCapacity 32,
            quote: Pending,
            escaped: Bool.false,
        }
        arrayChar
    |> .items

arrayChar = \state, byte ->
    # This parser assumes postgres won't respond with malformed syntax
    ## TODO: Handle null
    when byte is
        _ if state.escaped ->
            { state &
                escaped: Bool.false,
                curr: state.curr |> List.append byte,
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
                        curr: state.curr |> List.append byte,
                    }

        '\\' ->
            { state & escaped: Bool.true }

        ',' | '}' if state.quote != Open ->
            items =
                if List.isEmpty state.curr && state.quote == Pending then
                    state.items
                else
                    state.items |> List.append (Present state.curr)

            { state &
                items,
                curr: List.withCapacity 32,
                quote: Pending,
            }

        _ ->
            { state & curr: state.curr |> List.append byte }

pas = \input ->
    input
    |> Str.toUtf8
    |> parseArray
    |> List.map \item ->
        when item is
            Null -> Null
            Present present ->
                present
                |> Str.fromUtf8
                |> Result.withDefault ""
                |> Present

expect pas "{}" == []
expect pas "{42}" == [Present "42"]
expect
    result =
        pas "{\"(hi,\\\"lala\\\"\\\"\\\")\"}"
        |> List.map \item ->
            when item is
                Present p ->
                    Present (prs p)

                Null ->
                    Null

    result == [Present [Present "hi", Present "lala\""]]

textFormat : (Str -> Result a DecodeErr) -> Decode pg a
textFormat = \fn ->
    bytes <- @Decode

    when Str.fromUtf8 bytes is
        Ok text ->
            fn text

        Err (BadUtf8 _ _) ->
            Err InvalidUtf8

# Types

PgCmp a : { eq : {} }a

PgNum a : PgCmp { num : {} }a

PgI16 : PgNum { i16 : {} }
PgI32 : PgNum { i32 : {} }
PgI64 : PgNum { i64 : {} }
PgF32 : PgNum { f32 : {} }
PgF64 : PgNum { f64 : {} }

PgText : PgCmp { text : {} }

PgBool : PgCmp { bool : {} }

PgArray a : { array : a }
