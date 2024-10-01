module [
    header,
    message,
    Message,
    KeyData,
    Status,
    RowField,
    Error,
    ReadResult,
    ReadErr,
]

import Bytes.Read exposing [
    readU8,
    readI16,
    readI32,
    readNullTerminatedUtf8,
    Utf8ByteProblem,
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
    NoData,
    RowDescription (List RowField),
    ParameterDescription,
    DataRow (List (List U8)),
    PortalSuspended,
    CommandComplete Str,
    EmptyQueryResponse,
    CloseComplete,
]

ReadErr : [
    TooShort,
    BadUtf8 Utf8ByteProblem U64,
    UnrecognizedBackendMessage U8,
    UnrecognizedBackendStatus U8,
    MissingErrorField U8,
    InvalidErrorSeverity Str,
    InvalidErrorPosition,
]

ReadResult a : Result (a, List U8) ReadErr

header : List U8 -> ReadResult { msgType : U8, len : I32 }
header = \bytes ->
    (msgType, afterType) = readU8? bytes
    (len, afterLen) = readI32? afterType

    Ok ({ msgType, len: len - 4 }, afterLen)

message : List U8, U8 -> ReadResult Message
message = \bytes, msgType ->
    when msgType is
        'R' ->
            authRequest bytes

        'S' ->
            paramStatus bytes

        'K' ->
            backendKeyData bytes

        'Z' ->
            readyForQuery bytes

        'E' ->
            errorResponse bytes

        '1' ->
            Ok (ParseComplete, bytes)

        '2' ->
            Ok (BindComplete, bytes)

        'n' ->
            Ok (NoData, bytes)

        'T' ->
            rowDescription bytes

        't' ->
            Ok (ParameterDescription, bytes)

        'D' ->
            dataRow bytes

        's' ->
            Ok (PortalSuspended, bytes)

        'C' ->
            commandComplete bytes

        'I' ->
            Ok (EmptyQueryResponse, bytes)

        '3' ->
            Ok (CloseComplete, bytes)

        _ ->
            Err (UnrecognizedBackendMessage msgType)

authRequest : List U8 -> ReadResult Message
authRequest = \bytes ->
    (authType, after) = readI32? bytes

    when authType is
        0 ->
            Ok (AuthOk, after)

        3 ->
            Ok (AuthCleartextPassword, after)

        _ ->
            Ok (AuthUnsupported, after)

paramStatus : List U8 -> ReadResult Message
paramStatus = \bytes ->
    (name, afterName) = readNullTerminatedUtf8? bytes
    (value, afterValue) = readNullTerminatedUtf8? afterName

    Ok (ParameterStatus { name, value }, afterValue)

KeyData : { processId : I32, secretKey : I32 }

backendKeyData : List U8 -> ReadResult Message
backendKeyData = \bytes ->
    (processId, afterProcessId) = readI32? bytes
    (secretKey, afterSecretId) = readI32? afterProcessId

    Ok (BackendKeyData { processId, secretKey }, afterSecretId)

Status : [Idle, TransactionBlock, FailedTransactionBlock]

readyForQuery : List U8 -> ReadResult Message
readyForQuery = \bytes ->
    (status, after) = readU8? bytes

    when status is
        'I' ->
            Ok (ReadyForQuery Idle, after)

        'T' ->
            Ok (ReadyForQuery TransactionBlock, after)

        'E' ->
            Ok (ReadyForQuery FailedTransactionBlock, after)

        _ ->
            Err (UnrecognizedBackendStatus status)

Error : {
    localizedSeverity : Str,
    severity : Result ErrorSeverity [Missing],
    code : Str,
    message : Str,
    detail : Result Str [Missing],
    hint : Result Str [Missing],
    position : Result U32 [Missing],
    internalPosition : Result U32 [Missing],
    internalQuery : Result Str [Missing],
    ewhere : Result Str [Missing],
    schemaName : Result Str [Missing],
    tableName : Result Str [Missing],
    columnName : Result Str [Missing],
    dataTypeName : Result Str [Missing],
    constraintName : Result Str [Missing],
    file : Result Str [Missing],
    line : Result Str [Missing],
    routine : Result Str [Missing],
}

errorResponse : List U8 -> ReadResult Message
errorResponse = \bytes ->
    (dict, afterDict) = readDict? bytes (Dict.withCapacity 16)

    getRequired = \field ->
        Dict.get dict field |> Result.mapErr \KeyNotFound -> MissingErrorField field

    getOptional = \field ->
        Dict.get dict field |> Result.mapErr \KeyNotFound -> Missing

    onOk = \result, fn ->
        when result is
            Err err -> Ok (Err err)
            Ok value -> Ok (Ok (fn? value))

    localizedSeverity = getRequired? 'S'
    severity = getOptional 'V' |> onOk? decodeSeverity
    code = getRequired? 'C'
    msg = getRequired? 'M'
    position =
        getOptional 'P'
            |> onOk Str.toU32
            |> Result.mapErr? \InvalidNumStr -> InvalidErrorPosition
    internalPosition = getOptional 'p' |> onOk Str.toU32 |> Result.mapErr? \InvalidNumStr -> InvalidErrorPosition

    res = ErrorResponse {
        localizedSeverity,
        severity,
        code,
        message: msg,
        detail: getOptional 'D',
        hint: getOptional 'H',
        position,
        internalPosition,
        internalQuery: getOptional 'q',
        ewhere: getOptional 'W',
        schemaName: getOptional 's',
        tableName: getOptional 't',
        columnName: getOptional 'c',
        dataTypeName: getOptional 'd',
        constraintName: getOptional 'n',
        file: getOptional 'F',
        line: getOptional 'L',
        routine: getOptional 'R',
    }

    Ok (res, afterDict)

readDict : List U8, Dict U8 Str -> ReadResult (Dict U8 Str)
readDict = \bytes, dict ->
    (fieldId, afterFieldId) = readU8? bytes

    if fieldId == 0 then
        Ok (dict, afterFieldId)
    else
        (value, afterValue) = readNullTerminatedUtf8? afterFieldId
        inserted = Dict.insert dict fieldId value

        readDict afterValue inserted

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

decodeSeverity : Str -> Result ErrorSeverity [InvalidErrorSeverity Str]
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
            Err (InvalidErrorSeverity str)

RowField : {
    name : Str,
    column : Result { tableOid : I32, attributeNumber : I16 } [NotAColumn],
    dataTypeOid : I32,
    dataTypeSize : I16,
    typeModifier : I32,
    formatCode : I16,
}

rowDescription : List U8 -> ReadResult Message
rowDescription = \bytes ->
    (fields, afterFields) = readList? bytes rowField

    Ok (RowDescription fields, afterFields)

rowField : List U8 -> ReadResult RowField
rowField = \bytes ->
    (name, afterName) = readNullTerminatedUtf8? bytes
    (tableOid, afterTableOid) = readI32? afterName
    (attributeNumber, afterAttributeNumber) = readI16? afterTableOid
    (dataTypeOid, afterDataTypeOid) = readI32? afterAttributeNumber
    (dataTypeSize, afterDataTypeSize) = readI16? afterDataTypeOid
    (typeModifier, afterTypeModifier) = readI32? afterDataTypeSize
    (formatCode, afterFormatCode) = readI16? afterTypeModifier

    column =
        if tableOid != 0 && attributeNumber != 0 then
            Ok { tableOid, attributeNumber }
        else
            Err NotAColumn

    field = {
        name,
        column,
        dataTypeOid,
        dataTypeSize,
        typeModifier,
        formatCode,
    }

    Ok (field, afterFormatCode)

dataRow : List U8 -> ReadResult Message
dataRow = \bytes ->
    (values, afterValues) = readList? bytes \current ->
        (valueLen, afterLen) = readI32? current

        if valueLen == -1 then
            Ok ([], afterLen)
        else
            { before, others } = List.split afterLen (Num.toU64 valueLen)

            Ok (before, others)

    Ok (DataRow values, afterValues)

readList : List U8, (List U8 -> ReadResult a) -> ReadResult (List a)
readList = \bytes, decodeItem ->
    (count, afterCount) = readI16? bytes
    collected = List.withCapacity (Num.toU64 count)

    readListHelp afterCount count decodeItem collected

readListHelp : List U8, I16, (List U8 -> ReadResult a), List a -> ReadResult (List a)
readListHelp = \bytes, count, readItem, collected ->
    if count == 0 then
        Ok (collected, bytes)
        else

    when readItem bytes is
        Ok (item, afterItem) ->
            added = List.append collected item
            readListHelp afterItem (count - 1) readItem added

        Err err ->
            Err err

commandComplete : List U8 -> ReadResult Message
commandComplete = \bytes ->
    (str, after) = readNullTerminatedUtf8? bytes
    Ok (CommandComplete str, after)
