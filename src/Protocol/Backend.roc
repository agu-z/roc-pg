interface Protocol.Backend
    exposes [
        decode,
        Message,
        KeyData,
        Status,
        RowField,
        Error,
    ]
    imports [
        Bytes.Decode.{
            Decode,
            await,
            map,
            succeed,
            fail,
            loop,
            u8,
            i16,
            i32,
            cStr,
            take,
        },
    ]

decode : List U8 -> Result { decoded : Message, remaining : List U8 } _
decode = \bytes ->
    Bytes.Decode.decode bytes message

Message : [
    AuthOk,
    AuthRequired,
    ParameterStatus { name : Str, value : Str },
    BackendKeyData KeyData,
    ReadyForQuery Status,
    ErrorResponse Error,
    ParseComplete,
    BindComplete,
    RowDescription (List RowField),
    DataRow (List (List U8)),
    CommandComplete Str,
    EmptyQueryResponse,
]

message : Decode Message _
message =
    msgType <- await u8
    _len <- await i32

    when msgType is
        'R' ->
            authRequest

        'S' ->
            paramStatus

        'K' ->
            backendKeyData

        'Z' ->
            readyForQuery

        'E' ->
            errorResponse

        '1' ->
            succeed ParseComplete

        '2' ->
            succeed BindComplete

        'T' ->
            rowDescription

        'D' ->
            dataRow

        'C' ->
            commandComplete

        'I' ->
            succeed EmptyQueryResponse

        _ ->
            fail (UnrecognizedBackendMessage msgType)

authRequest : Decode Message _
authRequest =
    authType <- map i32

    when authType is
        0 ->
            AuthOk

        _ ->
            AuthRequired

paramStatus : Decode Message _
paramStatus =
    name <- await cStr
    value <- await cStr
    succeed (ParameterStatus { name, value })

KeyData : { processId : I32, secretKey : I32 }

backendKeyData : Decode Message _
backendKeyData =
    processId <- await i32
    secretKey <- await i32
    succeed (BackendKeyData { processId, secretKey })

Status : [Idle, TransactionBlock, FailedTransactionBlock]

readyForQuery : Decode Message _
readyForQuery =
    status <- await u8

    when status is
        'I' ->
            succeed (ReadyForQuery Idle)

        'T' ->
            succeed (ReadyForQuery TransactionBlock)

        'E' ->
            succeed (ReadyForQuery FailedTransactionBlock)

        _ ->
            fail (UnrecognizedBackendStatus status)

Error : {
    localizedSeverity : Str,
    severity : Result ErrorSeverity {},
    code : Str,
    message : Str,
    detail : Result Str {},
    hint : Result Str {},
    position : Result U32 {},
    internalPosition : Result U32 {},
    internalQuery : Result Str {},
    where : Result Str {},
    schemaName : Result Str {},
    tableName : Result Str {},
    columnName : Result Str {},
    dataTypeName : Result Str {},
    constraintName : Result Str {},
    file : Result Str {},
    line : Result Str {},
    routine : Result Str {},
}

errorResponse : Decode Message _
errorResponse =
    dict <- await knownStrFields

    localizedSeverity <- 'S' |> requiredField dict
    severity <- 'V' |> optionalFieldWith dict decodeSeverity
    code <- 'C' |> requiredField dict
    msg <- 'M' |> requiredField dict
    position <- 'P' |> optionalFieldWith dict Str.toU32
    internalPosition <- 'p' |> optionalFieldWith dict Str.toU32

    ErrorResponse {
        localizedSeverity,
        severity,
        code,
        message: msg,
        detail: 'D' |> optionalField dict,
        hint: 'H' |> optionalField dict,
        position,
        internalPosition,
        internalQuery: 'q' |> optionalField dict,
        where: 'W' |> optionalField dict,
        schemaName: 's' |> optionalField dict,
        tableName: 't' |> optionalField dict,
        columnName: 'c' |> optionalField dict,
        dataTypeName: 'd' |> optionalField dict,
        constraintName: 'n' |> optionalField dict,
        file: 'F' |> optionalField dict,
        line: 'L' |> optionalField dict,
        routine: 'R' |> optionalField dict,
    }
    |> succeed

optionalField = \fieldId, dict ->
    when Dict.get dict fieldId is
        Ok value ->
            Ok value

        Err _ ->
            Err {}

optionalFieldWith = \fieldId, dict, validate, callback ->
    result = Dict.get dict fieldId

    when result is
        Ok value ->
            when validate value is
                Ok validated ->
                    callback (Ok validated)

                Err err ->
                    fail err

        Err _ ->
            callback (Err {})

requiredField = \fieldId, dict, callback ->
    result = Dict.get dict fieldId

    when result is
        Ok value ->
            callback value

        Err _ ->
            fail (MissingField fieldId)

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

decodeSeverity = \str ->
    when str is
        "ERROR" ->
            Ok Error

        "FATAL" ->
            Ok Fatal

        "PANIC" ->
            Ok Panic

        "WARNING" ->
            Ok Warning

        "NOTICE" ->
            Ok Notice

        "DEBUG" ->
            Ok Debug

        "INFO" ->
            Ok Info

        "LOG" ->
            Ok Log

        _ ->
            Err (InvalidSeverity str)

knownStrFields : Decode (Dict U8 Str) _
knownStrFields =
    collected <- loop (Dict.empty {})

    fieldId <- await u8

    if fieldId == 0 then
        succeed (Done collected)
    else
        value <- map cStr

        collected
        |> Dict.insert fieldId value
        |> Loop

RowField : {
    name : Str,
    column : Result { tableOid : I32, attributeNumber : I16 } [NotAColumn],
    dataTypeOid : I32,
    dataTypeSize : I16,
    typeModifier : I32,
    formatCode : I16,
}

rowDescription : Decode Message _
rowDescription =
    fieldCount <- await i16

    fixedList fieldCount rowField
    |> map RowDescription

rowField : Decode RowField _
rowField =
    name <- await cStr
    tableOid <- await i32
    attributeNumber <- await i16
    dataTypeOid <- await i32
    dataTypeSize <- await i16
    typeModifier <- await i32
    formatCode <- map i16

    column =
        if tableOid != 0 && attributeNumber != 0 then
            Ok { tableOid, attributeNumber }
        else
            Err NotAColumn

    {
        name,
        column,
        dataTypeOid,
        dataTypeSize,
        typeModifier,
        formatCode,
    }

dataRow : Decode Message _
dataRow =
    columnCount <- await i16

    fixedList
        columnCount
        (
            valueLen <- await i32

            if valueLen == -1 then
                succeed []
            else
                take (Num.toNat valueLen) \x -> x
        )
    |> map DataRow

fixedList = \count, itemDecode ->
    loop [] \collected ->
        item <- map itemDecode

        added = List.append collected item

        if List.len added == Num.toNat count then
            Done added
        else
            Loop added

commandComplete : Decode Message _
commandComplete =
    map cStr CommandComplete
