module [
    startup,
    password_message,
    terminate,
    parse,
    bind,
    FormatCode,
    describe_portal,
    describe_statement,
    execute,
    close_statement,
    sync,
]

import Bytes.Encode exposing [
    sequence,
    null_terminate,
    u8,
    i16,
    i32,
    c_str,
]

startup : { user : Str, database : Str } -> List U8
startup = |{ user, database }|
    sequence(
        [
            # Version number
            i16(3),
            i16(0),
            # Encoding
            sequence(
                [
                    param("client_encoding", "utf_8"),
                    param("user", user),
                    param("database", database),
                ],
            )
            |> null_terminate,
        ],
    )
    |> prepend_length

param : Str, Str -> List U8
param = |key, value|
    sequence(
        [
            c_str(key),
            c_str(value),
        ],
    )

password_message : Str -> List U8
password_message = |pwd|
    message(
        'p',
        [
            c_str(pwd),
        ],
    )

terminate : List U8
terminate =
    message('X', [])

parse : { sql : Str, name ?? Str, param_type_ids ?? List I32 } -> List U8
parse = |{ sql, name ?? "", param_type_ids ?? [] }|
    message(
        'P',
        [
            c_str(name),
            c_str(sql),
            array(param_type_ids, i32),
        ],
    )

bind :
    {
        portal ?? Str,
        prepared_statement ?? Str,
        format_codes ?? List FormatCode,
        param_values : List [Null, Value (List U8)],
        column_format_codes ?? List FormatCode,
    }
    -> List U8
bind = |{ portal ?? "", prepared_statement ?? "", format_codes ?? [], param_values, column_format_codes ?? [] }|
    message(
        'B',
        [
            c_str(portal),
            c_str(prepared_statement),
            array(format_codes, format_code),
            array(
                param_values,
                |value|
                    when value is
                        Null ->
                            i32(-1)

                        Value(b) ->
                            bytes(b),
            ),
            array(column_format_codes, format_code),
        ],
    )

FormatCode : [Text, Binary]

format_code : FormatCode -> List U8
format_code = |code|
    when code is
        Text ->
            i16(0)

        Binary ->
            i16(1)

describe_portal : { name ?? Str } -> List U8
describe_portal = |{ name ?? "" }|
    message(
        'D',
        [
            u8('P'),
            c_str(name),
        ],
    )

describe_statement : { name ?? Str } -> List U8
describe_statement = |{ name ?? "" }|
    message(
        'D',
        [
            u8('S'),
            c_str(name),
        ],
    )

execute : { portal ?? Str, limit ?? [None, Limit I32] } -> List U8
execute = |{ portal ?? "", limit ?? None }|
    limit_or_zero =
        when limit is
            None ->
                0

            Limit(lim) ->
                lim

    message(
        'E',
        [
            c_str(portal),
            i32(limit_or_zero),
        ],
    )

close_statement : { name : Str } -> List U8
close_statement = |{ name }|
    message(
        'C',
        [
            u8('S'),
            c_str(name),
        ],
    )

sync : List U8
sync =
    message('S', [])

array : List item, (item -> List U8) -> List U8
array = |items, item_encode|
    sequence(
        [
            i16((List.len(items) |> Num.to_i16)),
            sequence(List.map(items, item_encode)),
        ],
    )

bytes : List U8 -> List U8
bytes = |value|
    sequence(
        [
            i32((List.len(value) |> Num.to_i32)),
            value,
        ],
    )

message : U8, List (List U8) -> List U8
message = |msg_type, content|
    sequence(
        [
            u8(msg_type),
            prepend_length(sequence(content)),
        ],
    )

prepend_length : List U8 -> List U8
prepend_length = |msg|
    total_length =
        List.len(msg)
        + 4
        |> Num.to_i32

    List.concat(i32(total_length), msg)
