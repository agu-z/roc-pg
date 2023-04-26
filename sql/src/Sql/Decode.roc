interface Sql.Decode
    exposes [
        decode,
        Decode,
        DecodeErr,
        succeed,
        text,
        bool,
        u8,
        u32,
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

u8 = textFormat Str.toU8

u32 = textFormat Str.toU32

text = textFormat Ok

bool =
    bytes <- @Decode

    when bytes is
        ['t'] ->
            Ok Bool.true

        ['f'] ->
            Ok Bool.false

        _ ->
            Err InvalidBool

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
        Ok str ->
            fn str

        Err (BadUtf8 _ _) ->
            Err InvalidUtf8
