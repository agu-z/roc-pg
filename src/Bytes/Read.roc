module [
    readU8,
    readU16,
    readI32,
    readU64,
    readI8,
    readI16,
    readI32,
    readI64,
    readNullTerminatedUtf8,
    Utf8ByteProblem,
]

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
readU16 : List U8 -> Result (U16, List U8) [TooShort]
readU16 = \bytes ->
    when bytes is
        [b0, b1, .. as rest] ->
            value =
                Num.shiftLeftBy (Num.toU16 b0) 8
                |> Num.bitwiseOr (Num.toU16 b1)

            Ok (value, rest)

        _ ->
            Err TooShort

expect readU16 [0x01, 0x02, 0x03] == Ok (0x0102, [0x03])
expect readU16 [0x01, 0x02] == Ok (0x0102, [])
expect readU16 [0x01] == Err TooShort

## Read a 32-bit unsigned integer
readU32 : List U8 -> Result (U32, List U8) [TooShort]
readU32 = \bytes ->
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

expect readU32 [0x01, 0x02, 0x03, 0x04, 0x05] == Ok (0x01020304, [0x05])
expect readU32 [0x01, 0x02, 0x03, 0x04] == Ok (0x01020304, [])
expect readU32 [0x01, 0x02, 0x03] == Err TooShort

## Read a 64-bit unsigned integer
readU64 : List U8 -> Result (U64, List U8) [TooShort]
readU64 = \bytes ->
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

expect readU64 [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09] == Ok (0x0102030405060708, [0x09])
expect readU64 [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08] == Ok (0x0102030405060708, [])
expect readU64 [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07] == Err TooShort

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
readI16 : List U8 -> Result (I16, List U8) [TooShort]
readI16 = \bytes ->
    when bytes is
        [b0, b1, .. as rest] ->
            value =
                Num.shiftLeftBy (Num.toI16 b0) 8
                |> Num.bitwiseOr (Num.toI16 b1)

            Ok (value, rest)

        _ ->
            Err TooShort

expect readI16 [0x00, 0x00] == Ok (0, [])
expect readI16 [0xFF, 0xFF] == Ok (-1, [])
expect readI16 [0x00, 0x2A] == Ok (42, [])
expect readI16 [0xFF, 0xD6] == Ok (-42, [])
expect readI16 [0x00] == Err TooShort

## Read a 32-bit signed integer (two's complement)
readI32 : List U8 -> Result (I32, List U8) [TooShort]
readI32 = \bytes ->
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

expect readI32 [0x00, 0x00, 0x00, 0x00] == Ok (0, [])
expect readI32 [0xFF, 0xFF, 0xFF, 0xFF] == Ok (-1, [])
expect readI32 [0x00, 0x00, 0x00, 0x2A] == Ok (42, [])
expect readI32 [0xFF, 0xFF, 0xFF, 0xD6] == Ok (-42, [])
expect readI32 [0x00, 0x00, 0x00] == Err TooShort

## Read a 64-bit signed integer (two's complement)
readI64 : List U8 -> Result (I64, List U8) [TooShort]
readI64 = \bytes ->
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

Utf8ByteProblem : [
    InvalidStartByte,
    UnexpectedEndOfSequence,
    ExpectedContinuation,
    OverlongEncoding,
    CodepointTooLarge,
    EncodesSurrogateHalf,
]

## Read a UTF-8 encoded string until a null byte is found
readNullTerminatedUtf8 : List U8 -> Result (Str, List U8) [TooShort, BadUtf8 Utf8ByteProblem U64]
readNullTerminatedUtf8 = \bytes ->
    when List.findFirstIndex bytes (\byte -> byte == 0x00) is
        Ok index ->
            { before, others } = List.split bytes index
            str = Str.fromUtf8? before
            after = List.dropFirst others 1

            Ok (str, after)

        Err NotFound ->
            Err TooShort

expect readNullTerminatedUtf8 [0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x00, 0x57, 0x00] == Ok ("Hello", [0x57, 0x00])
expect readNullTerminatedUtf8 [0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x00] == Ok ("Hello", [])
expect readNullTerminatedUtf8 [0x48, 0x65, 0x6C, 0x6C, 0x6F] == Err TooShort
expect readNullTerminatedUtf8 [0x48, 0x65, 0x6C, 0x6C, 0x6F, 0xC0, 0x00] == Err (BadUtf8 UnexpectedEndOfSequence 5)
expect readNullTerminatedUtf8 [0x53, 0x6F, 0x75, 0x6E, 0x64, 0x73, 0x20, 0x67, 0x6F, 0x6F, 0x64, 0x20, 0xF0, 0x9F, 0x91, 0x8D, 0x00] == Ok ("Sounds good 👍", [])
