module [
    readU8,
    readU16,
    readI32,
    readU64,
    readI8,
    readI16,
    readI32,
    readI64,
]

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

## Read a 8-bit signed integer (two's complement)
readI8 : List U8 -> Result (I8, List U8) [TooShort]
readI8 = \bytes ->
    when bytes is
        [byte, .. as rest] ->
            Ok (Num.toI8 byte, rest)

        _ ->
            Err TooShort

expect readI8 [0x00] == Ok (0, [])
expect readI8 [0xFF] == Ok (-1, [])
expect readI8 [0x2A] == Ok (42, [])
expect readI8 [0xD6] == Ok (-42, [])
expect readI8 [] == Err TooShort

## Read a 16-bit signed integer (two's complement)
readI16 : List U8, Endianness -> Result (I16, List U8) [TooShort]
readI16 = \bytes, LE ->
    when bytes is
        [b0, b1, .. as rest] ->
            value =
                Num.shiftLeftBy (Num.toI16 b0) 8
                |> Num.bitwiseOr (Num.toI16 b1)

            Ok (value, rest)

        _ ->
            Err TooShort

expect readI16 [0x00, 0x00] LE == Ok (0, [])
expect readI16 [0xFF, 0xFF] LE == Ok (-1, [])
expect readI16 [0x00, 0x2A] LE == Ok (42, [])
expect readI16 [0xFF, 0xD6] LE == Ok (-42, [])
expect readI16 [0x00] LE == Err TooShort

## Read a 32-bit signed integer (two's complement)
readI32 : List U8, Endianness -> Result (I32, List U8) [TooShort]
readI32 = \bytes, LE ->
    when bytes is
        [b0, b1, b2, b3, .. as rest] ->
            value =
                Num.shiftLeftBy (Num.toI32 b0) 24
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toI32 b1) 16)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toI32 b2) 8)
                |> Num.bitwiseOr (Num.toI32 b3)

            Ok (value, rest)

        _ ->
            Err TooShort

expect readI32 [0x00, 0x00, 0x00, 0x00] LE == Ok (0, [])
expect readI32 [0xFF, 0xFF, 0xFF, 0xFF] LE == Ok (-1, [])
expect readI32 [0x00, 0x00, 0x00, 0x2A] LE == Ok (42, [])
expect readI32 [0xFF, 0xFF, 0xFF, 0xD6] LE == Ok (-42, [])
expect readI32 [0x00, 0x00, 0x00] LE == Err TooShort

## Read a 64-bit signed integer (two's complement)
readI64 : List U8, Endianness -> Result (I64, List U8) [TooShort]
readI64 = \bytes, LE ->
    when bytes is
        [b0, b1, b2, b3, b4, b5, b6, b7, .. as rest] ->
            value =
                Num.shiftLeftBy (Num.toI64 b0) 56
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toI64 b1) 48)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toI64 b2) 40)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toI64 b3) 32)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toI64 b4) 24)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toI64 b5) 16)
                |> Num.bitwiseOr (Num.shiftLeftBy (Num.toI64 b6) 8)
                |> Num.bitwiseOr (Num.toI64 b7)

            Ok (value, rest)

        _ ->
            Err TooShort
