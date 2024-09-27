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

## Read a 32-bit unsigned integer
readU32 : List U8, Endianness -> Result (U32, List U8) [TooShort]
readU32 = \bytes, LE ->
    when bytes is
        [b0, b1, b2, b3, .. as rest] ->
            value =
                Num.shiftLeftBy (Num.toU32 b0) 24
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toU32 b1) 16)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toU32 b2) 8)
                |> Num.bitwiseOr (Num.toU32 b3)

            Ok (value, rest)

        _ ->
            Err TooShort

expect readU32 [0x01, 0x02, 0x03, 0x04, 0x05] LE == Ok (0x01020304, [0x05])
expect readU32 [0x01, 0x02, 0x03, 0x04] LE == Ok (0x01020304, [])
expect readU32 [0x01, 0x02, 0x03] LE == Err TooShort

## Read a 64-bit unsigned integer
readU64 : List U8, Endianness -> Result (U64, List U8) [TooShort]
readU64 = \bytes, LE ->
    when bytes is
        [b0, b1, b2, b3, b4, b5, b6, b7, .. as rest] ->
            value =
                Num.shiftLeftBy (Num.toU64 b0) 56
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toU64 b1) 48)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toU64 b2) 40)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toU64 b3) 32)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toU64 b4) 24)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toU64 b5) 16)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toU64 b6) 8)
                |> Num.bitwiseOr (Num.toU64 b7)

            Ok (value, rest)

        _ ->
            Err TooShort

expect readU64 [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09] LE == Ok (0x0102030405060708, [0x09])
expect readU64 [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08] LE == Ok (0x0102030405060708, [])
expect readU64 [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07] LE == Err TooShort
