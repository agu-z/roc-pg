interface Protocol.Frontend
    exposes [startup]
    imports [
        Bytes.Encode.{
            sequence,
            nullTerminate,
            i16,
            i32,
            cStr,
        },
    ]

startup : { user: Str, database ? [Specific Str, Unspecified] } -> List U8
startup = \{ user, database ? Unspecified } ->
    sequence [
        # Version number
        i16 3,
        i16 0,
        # Encoding
        sequence [
            param "client_encoding" "utf_8",
            param "user" user,
            when database is
                Specific db ->
                    param "database" db

                Unspecified ->
                    []
        ]
        |> nullTerminate,
    ]
    |> prependLength

param : Str, Str -> List U8
param = \key, value ->
    sequence [
        cStr key,
        cStr value,
    ]

prependLength : List U8 -> List U8
prependLength = \msg ->
    totalLength =
        List.len msg
        + 4
        |> Num.toI32

    List.concat (i32 totalLength) msg
