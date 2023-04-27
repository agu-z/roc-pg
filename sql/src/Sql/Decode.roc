interface Sql.Decode
    exposes [
        decode,
        Decode,
        DecodeErr,
        succeed,
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
    ]
    imports []

Decode a := List U8 -> Result a DecodeErr

DecodeErr : [
    InvalidUtf8,
    InvalidNumStr,
    InvalidBool,
    MissingColumn Nat,
]

decode : List U8, Decode a -> Result a DecodeErr
decode = \bytes, @Decode fn ->
    fn bytes

succeed : a -> Decode a
succeed = \value -> @Decode \_ -> Ok value

i16 = textFormat Str.toI16

i32 = textFormat Str.toI32

i64 = textFormat Str.toI64

f32 = textFormat Str.toF32

f64 = textFormat Str.toF64

dec = textFormat Str.toDec

str = textFormat Ok

bool =
    bytes <- @Decode

    when bytes is
        ['t'] ->
            Ok Bool.true

        ['f'] ->
            Ok Bool.false

        _ ->
            Err InvalidBool

unsupported = @Decode \bytes -> Ok (Unsupported bytes)

rowArray = \cb ->
    arr <- textFormat

    # TODO: Write a proper parser
    # This parser is a huge hack for a PoC
    arr
    |> Str.graphemes
    |> List.drop 2
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

textFormat : (Str -> Result a DecodeErr) -> Decode a
textFormat = \fn ->
    bytes <- @Decode

    when Str.fromUtf8 bytes is
        Ok text ->
            fn text

        Err (BadUtf8 _ _) ->
            Err InvalidUtf8
