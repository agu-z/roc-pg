interface Protocol.Frontend
    exposes [startup, query, terminate]
    imports [
        Bytes.Encode.{
            sequence,
            nullTerminate,
            u8,
            i16,
            i32,
            cStr,
        },
    ]

startup : { user : Str, database : Str } -> List U8
startup = \{ user, database } ->
    sequence [
        # Version number
        i16 3,
        i16 0,
        # Encoding
        sequence [
            param "client_encoding" "utf_8",
            param "user" user,
            param "database" database,
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

query : Str -> List U8
query = \source ->
    message 'Q' [
        cStr source,
    ]

terminate : List U8
terminate =
    message 'X' []

message : U8, List (List U8) -> List U8
message = \msgType, content ->
    sequence [
        u8 msgType,
        prependLength (sequence content),
    ]

prependLength : List U8 -> List U8
prependLength = \msg ->
    totalLength =
        List.len msg
        + 4
        |> Num.toI32

    List.concat (i32 totalLength) msg
