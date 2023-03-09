interface Protocol.Backend
    exposes [
        decode,
        errorToStr,
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
            i32,
            cStr,
        },
    ]

decode : List U8 -> Result { decoded : Message, remaining : List U8 } _
decode = \bytes ->
    Bytes.Decode.decode bytes message

Message : [
    AuthOk,
    AuthRequired,
    ErrorResponse Error,
    ParameterStatus { name : Str, value : Str },
    BackendKeyData { processId : I32, secretKey : I32 },
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

        'E' ->
            errorResponse

        'K' ->
            backendKeyData

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

backendKeyData : Decode Message _
backendKeyData =
    processId <- await i32
    secretKey <- await i32
    succeed (BackendKeyData { processId, secretKey })

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

errorToStr : Error -> Str
errorToStr = \err ->
    addField = \str, name, result ->
        when result is
            Ok value ->
                "\(str)\n\(name): \(value)"

            Err {} ->
                str

    fieldsStr =
        ""
        |> addField "Detail" err.detail
        |> addField "Hint" err.hint
        |> addField "Position" (err.position |> Result.map Num.toStr)
        |> addField "Internal Position" (err.internalPosition |> Result.map Num.toStr)
        |> addField "Internal Query" err.internalQuery
        |> addField "Where" err.where
        |> addField "Schema" err.schemaName
        |> addField "Table" err.tableName
        |> addField "Data type" err.dataTypeName
        |> addField "Constraint" err.constraintName
        |> addField "File" err.file
        |> addField "Line" err.line
        |> addField "Routine" err.line

    "\(err.localizedSeverity) (\(err.code)): \(err.message)\n\(fieldsStr)"
    |> Str.trim

errorResponse : Decode Message _
errorResponse =
    dict <- await fields

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

fields : Decode (Dict U8 Str) _
fields =
    collected <- loop (Dict.empty {})

    fieldId <- await u8

    if fieldId == 0 then
        succeed (Done collected)
    else
        value <- map cStr

        collected
        |> Dict.insert fieldId value
        |> Loop
