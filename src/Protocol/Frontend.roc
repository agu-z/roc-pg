interface Protocol.Frontend
    exposes [
        startup,
        passwordMessage,
        terminate,
        parse,
        bind,
        FormatCode,
        describePortal,
        describeStatement,
        execute,
        sync,
    ]
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

passwordMessage : Str -> List U8
passwordMessage = \pwd ->
    message 'p' [
        cStr pwd,
    ]

terminate : List U8
terminate =
    message 'X' []

parse : { sql : Str, name ? Str, paramTypeIds ? List I32 } -> List U8
parse = \{ sql, name ? "", paramTypeIds ? [] } ->
    message 'P' [
        cStr name,
        cStr sql,
        array paramTypeIds i32,
    ]

bind :
    {
        portal ? Str,
        preparedStatement ? Str,
        formatCodes ? List FormatCode,
        paramValues : List [Null, Value (List U8)],
        columnFormatCodes ? List FormatCode,
    }
    -> List U8
bind = \{ portal ? "", preparedStatement ? "", formatCodes ? [], paramValues, columnFormatCodes ? [] } ->
    message 'B' [
        cStr portal,
        cStr preparedStatement,
        array formatCodes formatCode,
        array
            paramValues
            (\value ->
                when value is
                    Null ->
                        i32 -1

                    Value b ->
                        bytes b
            ),
        array columnFormatCodes formatCode,
    ]

FormatCode : [Text, Binary]

formatCode : FormatCode -> List U8
formatCode = \code ->
    when code is
        Text ->
            i16 0

        Binary ->
            i16 1

describePortal : { name ? Str } -> List U8
describePortal = \{ name ? "" } ->
    message 'D' [
        u8 'P',
        cStr name,
    ]

describeStatement : { name ? Str } -> List U8
describeStatement = \{ name ? "" } ->
    message 'D' [
        u8 'S',
        cStr name,
    ]

execute : { portal ? Str } -> List U8
execute = \{ portal ? "" } ->
    message 'E' [
        cStr portal,

        # No row limit
        i32 0,
    ]

sync : List U8
sync =
    message 'S' []

array : List item, (item -> List U8) -> List U8
array = \items, itemEncode ->
    sequence [
        i16 (List.len items |> Num.toI16),
        sequence (List.map items itemEncode),
    ]

bytes : List U8 -> List U8
bytes = \value ->
    sequence [
        i32 (List.len value |> Num.toI32),
        value,
    ]

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
