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
        rowArray,
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

rowArray = \cb ->
    arr <- textFormat

    # TODO: Write a proper parser
    # This parser is a huge hack for a PoC
    arr
    |> Str.graphemes
    |> List.drop 2
    |> List.dropLast
    |> List.dropLast
    |> Str.joinWith ""
    |> Str.split "\",\""
    |> List.map row
    |> cb

row = \items ->
    items
    |> Str.graphemes
    |> List.dropFirst
    |> List.dropLast
    |> Str.joinWith ""
    |> Str.split ","
    |> List.map Str.toUtf8

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
