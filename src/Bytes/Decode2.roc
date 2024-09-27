module [readU8]

Endianness : [LE]

## Read an 8-bit unsigned integer
readU8 : List U8 -> Result (U8, List U8) [TooShort]
readU8 = \bytes ->
    when bytes is
        [byte, .. as rest] ->
            Ok (byte, rest)

        _ ->
            Err TooShort

expect readU8 [0x01, 0x02, 0x03] == Ok (0x01, [0x02, 0x03])
expect readU8 [0x01] == Ok (0x01, [])
expect readU8 [] == Err TooShort

## Read a 16-bit unsigned integer
readU16 : List U8, Endianness -> Result (U16, List U8) [TooShort]
readU16 = \bytes, LE ->
    when bytes is
        [b0, b1, .. as rest] ->
            value =
                Num.shiftLeftBy (Num.toU16 b0) 8
                |> Num.bitwiseOr (Num.toU16 b1)

            Ok (value, rest)

        _ ->
            Err TooShort

expect readU16 [0x01, 0x02, 0x03] LE == Ok (0x0102, [0x03])
expect readU16 [0x01, 0x02] LE == Ok (0x0102, [])
expect readU16 [0x01] LE == Err TooShort
