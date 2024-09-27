module [readU8]

# Unsigned Integers

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
