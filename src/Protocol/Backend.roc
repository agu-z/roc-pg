module [
    Error,
    KeyData,
    Message,
    ParameterField,
    RowField,
    Status,
    header,
    message,
]

import Bytes.Decode exposing [
    Decode,
    await,
    map,
    succeed,
    fail,
    loop,
    u8,
    i16,
    i32,
    c_str,
    take,
]

Message : [
    AuthOk,
    AuthCleartextPassword,
    AuthUnsupported,
    ParameterStatus { name : Str, value : Str },
    BackendKeyData KeyData,
    ReadyForQuery Status,
    ErrorResponse Error,
    ParseComplete,
    BindComplete,
    NoticeResponse (List { code : U8, value : Str }),
    NoData,
    RowDescription (List RowField),
    ParameterDescription (List ParameterField),
    DataRow (List (List U8)),
    PortalSuspended,
    CommandComplete Str,
    EmptyQueryResponse,
    CloseComplete,
]

header : Decode { msg_type : U8, len : I32 } _
header =
    await(
        u8,
        |msg_type|
            map(
                i32,
                |len|
                    { msg_type, len: len - 4 },
            ),
    )

message : U8 -> Decode Message _
message = |msg_type|
    when msg_type is
        'R' ->
            auth_request

        'S' ->
            param_status

        'K' ->
            backend_key_data

        'Z' ->
            ready_for_query

        'E' ->
            error_response

        '1' ->
            succeed(ParseComplete)

        '2' ->
            succeed(BindComplete)

        'N' ->
            notice_response

        'n' ->
            succeed(NoData)

        'T' ->
            row_description

        't' ->
            parameter_description

        'D' ->
            data_row

        's' ->
            succeed(PortalSuspended)

        'C' ->
            command_complete

        'I' ->
            succeed(EmptyQueryResponse)

        '3' ->
            succeed(CloseComplete)

        _ ->
            fail(UnrecognizedBackendMessage(msg_type))

auth_request : Decode Message _
auth_request =
    map(
        i32,
        |auth_type|
            when auth_type is
                0 ->
                    AuthOk

                3 ->
                    AuthCleartextPassword

                _ ->
                    AuthUnsupported,
    )

param_status : Decode Message _
param_status =
    await(
        c_str,
        |name|
            await(
                c_str,
                |value|
                    succeed(ParameterStatus({ name, value })),
            ),
    )

KeyData : { process_id : I32, secret_key : I32 }

backend_key_data : Decode Message _
backend_key_data =
    await(
        i32,
        |process_id|
            await(
                i32,
                |secret_key|
                    succeed(BackendKeyData({ process_id, secret_key })),
            ),
    )

Status : [Idle, TransactionBlock, FailedTransactionBlock]

ready_for_query : Decode Message _
ready_for_query =
    await(
        u8,
        |status|
            when status is
                'I' ->
                    succeed(ReadyForQuery(Idle))

                'T' ->
                    succeed(ReadyForQuery(TransactionBlock))

                'E' ->
                    succeed(ReadyForQuery(FailedTransactionBlock))

                _ ->
                    fail(UnrecognizedBackendStatus(status)),
    )

read_notice_responses : Decode (List { code : U8, value : Str }) _
read_notice_responses =
    loop(
        [],
        |collected|
            await(
                u8,
                |code|
                    if code == 0 then
                        succeed(Done(collected))
                    else
                        map(
                            c_str,
                            |value|
                                Loop(List.append(collected, { code, value })),
                        ),
            ),
    )

notice_response =
    await(
        read_notice_responses,
        |notices|
            succeed(NoticeResponse(notices)),
    )

Error : {
    localized_severity : Str,
    severity : Result ErrorSeverity {},
    code : Str,
    message : Str,
    detail : Result Str {},
    hint : Result Str {},
    position : Result U32 {},
    internal_position : Result U32 {},
    internal_query : Result Str {},
    ewhere : Result Str {},
    schema_name : Result Str {},
    table_name : Result Str {},
    column_name : Result Str {},
    data_type_name : Result Str {},
    constraint_name : Result Str {},
    file : Result Str {},
    line : Result Str {},
    routine : Result Str {},
}

error_response : Decode Message _
error_response =
    await(
        known_str_fields,
        |dict|
            'S'
            |> required_field(
                dict,
                |localized_severity|
                    'V'
                    |> optional_field_with(
                        dict,
                        decode_severity,
                        |severity|
                            'C'
                            |> required_field(
                                dict,
                                |code|
                                    'M'
                                    |> required_field(
                                        dict,
                                        |msg|
                                            'P'
                                            |> optional_field_with(
                                                dict,
                                                Str.to_u32,
                                                |position|
                                                    'p'
                                                    |> optional_field_with(
                                                        dict,
                                                        Str.to_u32,
                                                        |internal_position|
                                                            ErrorResponse(
                                                                {
                                                                    localized_severity,
                                                                    severity,
                                                                    code,
                                                                    message: msg,
                                                                    detail: 'D' |> optional_field(dict),
                                                                    hint: 'H' |> optional_field(dict),
                                                                    position,
                                                                    internal_position,
                                                                    internal_query: 'q' |> optional_field(dict),
                                                                    ewhere: 'W' |> optional_field(dict),
                                                                    schema_name: 's' |> optional_field(dict),
                                                                    table_name: 't' |> optional_field(dict),
                                                                    column_name: 'c' |> optional_field(dict),
                                                                    data_type_name: 'd' |> optional_field(dict),
                                                                    constraint_name: 'n' |> optional_field(dict),
                                                                    file: 'F' |> optional_field(dict),
                                                                    line: 'L' |> optional_field(dict),
                                                                    routine: 'R' |> optional_field(dict),
                                                                },
                                                            )
                                                            |> succeed,
                                                    ),
                                            ),
                                    ),
                            ),
                    ),
            ),
    )

optional_field = |field_id, dict|
    when Dict.get(dict, field_id) is
        Ok(value) ->
            Ok(value)

        Err(_) ->
            Err({})

optional_field_with = |field_id, dict, validate, callback|
    result = Dict.get(dict, field_id)

    when result is
        Ok(value) ->
            when validate(value) is
                Ok(validated) ->
                    callback(Ok(validated))

                Err(err) ->
                    fail(err)

        Err(_) ->
            callback(Err({}))

required_field = |field_id, dict, callback|
    result = Dict.get(dict, field_id)

    when result is
        Ok(value) ->
            callback(value)

        Err(_) ->
            fail(MissingField(field_id))

ErrorSeverity : [
    Error,
    Fatal,
    Panic,
    Warning,
    Notice,
    Debug,
    Info,
    Log,
]

decode_severity : Str -> Result ErrorSeverity [InvalidSeverity Str]
decode_severity = |str|
    when str is
        "ERROR" ->
            Ok(Error)

        "FATAL" ->
            Ok(Fatal)

        "PANIC" ->
            Ok(Panic)

        "WARNING" ->
            Ok(Warning)

        "NOTICE" ->
            Ok(Notice)

        "DEBUG" ->
            Ok(Debug)

        "INFO" ->
            Ok(Info)

        "LOG" ->
            Ok(Log)

        _ ->
            Err(InvalidSeverity(str))

known_str_fields : Decode (Dict U8 Str) _
known_str_fields =
    loop(
        Dict.empty({}),
        |collected|
            await(
                u8,
                |field_id|
                    if field_id == 0 then
                        succeed(Done(collected))
                    else
                        map(
                            c_str,
                            |value|
                                collected
                                |> Dict.insert(field_id, value)
                                |> Loop,
                        ),
            ),
    )

RowField : {
    name : Str,
    column : Result { table_oid : I32, attribute_number : I16 } [NotAColumn],
    data_type_oid : I32,
    data_type_size : I16,
    type_modifier : I32,
    format_code : I16,
}

ParameterField : {
    data_type_oid : I32,
}

parameter_description : Decode Message _
parameter_description =
    await(
        i16,
        |field_count|
            if field_count == 0 then
                succeed(ParameterDescription([]))
            else
                fixed_list(field_count, parameter_field)
                |> map(ParameterDescription),
    )

parameter_field : Decode ParameterField _
parameter_field =
    await(
        i32,
        |data_type_oid|
            succeed({ data_type_oid }),
    )

row_description : Decode Message _
row_description =
    await(
        i16,
        |field_count|
            fixed_list(field_count, row_field)
            |> map(RowDescription),
    )

row_field : Decode RowField _
row_field =
    await(
        c_str,
        |name|
            await(
                i32,
                |table_oid|
                    await(
                        i16,
                        |attribute_number|
                            await(
                                i32,
                                |data_type_oid|
                                    await(
                                        i16,
                                        |data_type_size|
                                            await(
                                                i32,
                                                |type_modifier|
                                                    map(
                                                        i16,
                                                        |format_code|
                                                            column =
                                                                if table_oid != 0 and attribute_number != 0 then
                                                                    Ok({ table_oid, attribute_number })
                                                                else
                                                                    Err(NotAColumn)

                                                            {
                                                                name,
                                                                column,
                                                                data_type_oid,
                                                                data_type_size,
                                                                type_modifier,
                                                                format_code,
                                                            },
                                                    ),
                                            ),
                                    ),
                            ),
                    ),
            ),
    )

data_row : Decode Message _
data_row =
    await(
        i16,
        |column_count|
            fixed_list(
                column_count,
                await(
                    i32,
                    |value_len|
                        if value_len == -1 then
                            succeed([])
                        else
                            take(Num.to_u64(value_len), |x| x),
                ),
            )
            |> map(DataRow),
    )

fixed_list = |count, item_decode|
    loop(
        List.with_capacity(Num.to_u64(count)),
        |collected|
            map(
                item_decode,
                |item|
                    added = List.append(collected, item)

                    if List.len(added) == Num.to_u64(count) then
                        Done(added)
                    else
                        Loop(added),
            ),
    )

command_complete : Decode Message _
command_complete =
    map(c_str, CommandComplete)
