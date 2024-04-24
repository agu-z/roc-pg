interface Sql.Types exposes [
        decode,
        Decode,
        DecodeErr,
        map,
        succeed,
        fail,
        nullable,
        discardPhantom,
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
    imports [
        Sql.Nullable.{ Nullable },
    ]

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
decode = \bytes, @Decode fn ->
    fn bytes

map : Decode pg a, (a -> b) -> Decode pg b
map = \@Decode a, toB -> @Decode \bytes -> a bytes |> Result.map toB

succeed : a -> Decode pg a
succeed = \value -> @Decode \_ -> Ok value

fail : Str -> Decode pg a
fail = \message -> @Decode \_ -> Err (Error message)

nullable : Decode pg a -> Decode (Nullable pg) (Nullable a)
nullable = \@Decode sub ->
    bytes <- @Decode

    if List.isEmpty bytes then
        # TODO: Use Null tag instead of empty list
        Ok Null
    else
        sub bytes
        |> Result.map NotNull

discardPhantom : Decode * a -> Decode * a
discardPhantom = \@Decode sub -> @Decode sub

i16 : Decode (PgI16 *) I16
i16 = textFormat Str.toI16

i32 : Decode (PgI32 *) I32
i32 = textFormat Str.toI32

i64 : Decode (PgI64 *) I64
i64 = textFormat Str.toI64

f32 : Decode (PgF32 *) F32
f32 = textFormat Str.toF32

f64 : Decode (PgF64 *) F64
f64 = textFormat Str.toF64

dec : Decode (PgDec *) Dec
dec = textFormat Str.toDec

str : Decode (PgText *) Str
str = textFormat Ok

# TODO: Make a proper uuid type
uuid : Decode (PgUuid *) Str
uuid = textFormat Ok

bool : Decode (PgBool *) Bool
bool =
    bytes <- @Decode

    when bytes is
        ['t'] ->
            Ok Bool.true

        ['f'] ->
            Ok Bool.false

        _ ->
            Err (InvalidBool bytes)

Raw : {
    bytes : List U8,
    typeName : Str,
}

unsupported : Str -> Decode * Raw
unsupported = \typeName ->
    bytes <- @Decode

    Ok {
        bytes,
        typeName,
    }

array : Decode pg a -> Decode (PgArray pg *) (List a)
array = \@Decode decodeItem ->
    arrayBytes <- @Decode

    arrayBytes
    |> parseArray
    |> List.mapTry \item ->
        when item is
            Null ->
                decodeItem []

            NotNull val ->
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

            NotNull val ->
                val
    |> decodeRow

parseRow = \bytes ->
    bytes
    |> List.dropFirst 1
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
                    NotNull state.curr

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
            NotNull present ->
                present
                |> Str.fromUtf8
                |> Result.withDefault ""
                |> NotNull

expect prs "(42)" == [NotNull "42"]
expect prs "(42,hi)" == [NotNull "42", NotNull "hi"]
expect prs "(42,\" hi\")" == [NotNull "42", NotNull " hi"]
expect prs "(\"hello world\",hi)" == [NotNull "hello world", NotNull "hi"]
expect prs "(\"hello \\\"Agus\\\"\",21)" == [NotNull "hello \"Agus\"", NotNull "21"]
expect prs "(\"hello \"\"Agus\"\"\",21)" == [NotNull "hello \"Agus\"", NotNull "21"]
expect prs "(\"a\"\"\")" == [NotNull "a\""]
expect prs "(\"a\\\\b\")" == [NotNull "a\\b"]
expect prs "(\"a\nb\")" == [NotNull "a\nb"]
expect prs "(\"hi, world!\",\"hello, agus\")" == [NotNull "hi, world!", NotNull "hello, agus"]
expect prs "(spaces,no quotes)" == [NotNull "spaces", NotNull "no quotes"]
expect prs "(,prev is null)" == [Null, NotNull "prev is null"]
expect prs "(next is null,)" == [NotNull "next is null", Null]
expect prs "(next is null,,prev is null)" == [NotNull "next is null", Null, NotNull "prev is null"]
expect prs "()" == [Null]
expect prs "(,)" == [Null, Null]
expect prs "(,,)" == [Null, Null, Null]
expect prs "(\"\")" == [NotNull ""]
expect prs "(\"\",)" == [NotNull "", Null]

parseArray = \bytes ->
    bytes
    |> List.dropFirst 1
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
                    state.items |> List.append (NotNull state.curr)

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
            NotNull present ->
                present
                |> Str.fromUtf8
                |> Result.withDefault ""
                |> NotNull

expect pas "{}" == []
expect pas "{42}" == [NotNull "42"]
expect
    result =
        pas "{\"(hi,\\\"lala\\\"\\\"\\\")\"}"
        |> List.map \item ->
            when item is
                NotNull p ->
                    NotNull (prs p)

                Null ->
                    Null

    result == [NotNull [NotNull "hi", NotNull "lala\""]]

textFormat : (Str -> Result a DecodeErr) -> Decode pg a
textFormat = \fn ->
    bytes <- @Decode

    when Str.fromUtf8 bytes is
        Ok text ->
            fn text

        Err (BadUtf8 _ _) ->
            Err InvalidUtf8

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

