interface Sql.Decode
    exposes [
        decodeText,
        decodeBool,
        decodeU8,
        decodeU32,
    ]
    imports []

Decode a : List U8 -> Result a DecodeErr

DecodeErr : [
    InvalidUtf8,
    InvalidNumStr,
    InvalidBool,
]

decodeTextFormat : (Str -> Result a DecodeErr) -> Decode a
decodeTextFormat = \fn -> \bytes ->
        when Str.fromUtf8 bytes is
            Ok str ->
                fn str

            Err (BadUtf8 _ _) ->
                Err InvalidUtf8

decodeU8 = decodeTextFormat Str.toU8

decodeU32 = decodeTextFormat Str.toU32

decodeText = decodeTextFormat Ok

decodeBool = \bytes ->
    when bytes is
        ['t', 'r', 'u', 'e'] ->
            Ok Bool.true

        ['f', 'a', 'l', 's', 'e'] ->
            Ok Bool.false

        _ ->
            Err InvalidBool
