interface Pg.Client
    exposes [
        withConnect,
        command,
        prepare,
        Error,
        errorToStr,
    ]
    imports [
        Protocol.Backend,
        Protocol.Frontend,
        Bytes.Encode,
        Bytes.Decode.{ decode },
        Pg.Result.{ QueryResult },
        Pg.Cmd.{ Cmd, Unprepared },
        pf.Task.{ Task, await, fail },
        pf.Tcp,
        Cmd,
    ]

Client := {
    stream : Tcp.Stream,
    backendKey : Result Protocol.Backend.KeyData [Pending],
}

withConnect :
    {
        host : Str,
        port : U16,
        user : Str,
        auth ? [None, Password Str],
        database : Str,
    },
    (Client -> Task {} _)
    -> Task {} _
withConnect = \{ host, port, database, auth ? None, user }, callback ->
    stream <- Tcp.withConnect host port

    _ <- Protocol.Frontend.startup { user, database } |> send stream

    msg, state <- messageLoop stream {
            parameters: Dict.empty {},
            backendKey: Err Pending,
        }

    when msg is
        AuthOk ->
            next state

        AuthCleartextPassword ->
            when auth is
                None ->
                    fail PasswordRequired

                Password pwd ->
                    _ <- Protocol.Frontend.passwordMessage pwd
                        |> send stream

                    next state

        AuthUnsupported ->
            fail UnsupportedAuth

        BackendKeyData backendKey ->
            next { state & backendKey: Ok backendKey }

        ReadyForQuery _ ->
            result <-
                @Client {
                    stream,
                    backendKey: state.backendKey,
                }
                |> callback
                |> await

            _ <- Protocol.Frontend.terminate |> send stream

            return result

        _ ->
            unexpected msg

command : Cmd, Client -> Task QueryResult _
command = \cmd, @Client { stream } ->
    { formatCodes, paramValues } = Cmd.encodeBindings cmd

    init =
        when Cmd.getState cmd is
            Unprepared sql ->
                {
                    messages: Bytes.Encode.sequence [
                        Protocol.Frontend.parse { sql },
                        Protocol.Frontend.bind { formatCodes, paramValues },
                        Protocol.Frontend.describePortal {},
                        Protocol.Frontend.execute {},
                        Protocol.Frontend.sync,
                    ],
                    fields: [],
                }

            Prepared prepared ->
                {
                    messages: Bytes.Encode.sequence [
                        Protocol.Frontend.bind {
                            formatCodes,
                            paramValues,
                            preparedStatement: prepared.name,
                        },
                        Protocol.Frontend.execute {},
                        Protocol.Frontend.sync,
                    ],
                    fields: prepared.fields,
                }

    _ <- send init.messages stream

    msg, state <- messageLoop stream {
            fields: init.fields,
            rows: [],
        }

    when msg is
        ParseComplete | BindComplete | ParameterDescription ->
            next state

        RowDescription fields ->
            next { state & fields: fields }

        DataRow row ->
            next { state & rows: List.append state.rows row }

        CommandComplete _ | EmptyQueryResponse ->
            next state

        ReadyForQuery _ ->
            Pg.Result.create state
            |> Done
            |> Task.succeed

        _ ->
            unexpected msg

prepare : Str, { name : Str, client : Client } -> Task Cmd _
prepare = \sql, { name, client } ->
    (@Client { stream }) = client

    _ <- Bytes.Encode.sequence [
            Protocol.Frontend.parse { sql, name },
            Protocol.Frontend.describeStatement { name },
            Protocol.Frontend.sync,
        ]
        |> send stream

    msg, state <- messageLoop stream []

    when msg is
        ParseComplete | ParameterDescription ->
            next state

        RowDescription fields ->
            next fields

        ReadyForQuery _ ->
            return (Cmd.prepared { name, fields: state })

        _ ->
            unexpected msg

Error : Protocol.Backend.Error

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

# Helpers

readMessage : Tcp.Stream -> Task Protocol.Backend.Message _
readMessage = \stream ->
    headerBytes <- Tcp.readExactly 5 stream |> await
    meta <- headerBytes |> decode Protocol.Backend.header |> try

    if meta.len > 0 then
        payload <- Tcp.readExactly (Num.toNat meta.len) stream |> await
        decode payload (Protocol.Backend.message meta.msgType) |> Task.fromResult
    else
        decode [] (Protocol.Backend.message meta.msgType) |> Task.fromResult

messageLoop : Tcp.Stream, state, (Protocol.Backend.Message, state -> Task [Done done, Step state] _) -> Task done _
messageLoop = \stream, initState, stepFn ->
    state <- Task.loop initState

    message <- readMessage stream |> await

    when message is
        ErrorResponse error ->
            fail (ErrorResponse error)

        ParameterStatus _ ->
            Task.succeed (Step state)

        _ ->
            stepFn message state

next : a -> Task [Step a] *
next = \state ->
    Task.succeed (Step state)

return : a -> Task [Done a] *
return = \result ->
    Task.succeed (Done result)

unexpected : a -> Task * [UnexpectedMsg a]
unexpected = \msg ->
    fail (UnexpectedMsg msg)

send : List U8, Tcp.Stream, ({} -> Task a _) -> Task a _
send = \bytes, stream, callback ->
    Tcp.write bytes stream |> await callback

try : Result a err, (a -> Task b err) -> Task b err
try = \result, callback ->
    Task.fromResult result |> await callback
