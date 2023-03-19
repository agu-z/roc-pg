interface Pg.Client
    exposes [
        withConnect,
        query,
        Error,
        errorToStr,
    ]
    imports [
        Protocol.Backend,
        Protocol.Frontend,
        Bytes.Encode,
        Pg.Result.{ QueryResult },
        Pg.Bind.{ Binding },
        pf.Task.{ Task, await },
        pf.Tcp,
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
        database : Str,
    },
    (Client -> Task {} _)
    -> Task {} _
withConnect = \{ host, port, database, user }, callback ->
    stream <- Tcp.withConnect host port

    _ <- Protocol.Frontend.startup { user, database }
        |> Tcp.writeBytes stream
        |> await

    msg, state <- messageLoop stream {
            parameters: Dict.empty {},
            backendKey: Err Pending,
        }

    when msg is
        AuthOk ->
            loopWith state

        AuthRequired ->
            Task.fail UnsupportedAuth

        BackendKeyData backendKey ->
            loopWith { state & backendKey: Ok backendKey }

        ReadyForQuery _ ->
            done <-
                @Client {
                    stream,
                    backendKey: state.backendKey,
                }
                |> callback
                |> await

            _ <- Protocol.Frontend.terminate
                |> Tcp.writeBytes stream
                |> await

            Task.succeed (Done done)

        _ ->
            unexpected msg

query : Client, Str, List Binding -> Task QueryResult _
query = \@Client { stream }, sql, bindings ->
    { formatCodes, paramValues } = Pg.Bind.encode bindings

    initMessages = Bytes.Encode.sequence [
        Protocol.Frontend.parse { sql },
        Protocol.Frontend.bind { formatCodes, paramValues },
        Protocol.Frontend.describePortal {},
        Protocol.Frontend.execute {},
        Protocol.Frontend.sync,
    ]

    _ <- initMessages
        |> Tcp.writeBytes stream
        |> await

    initResult = {
        fields: [],
        rows: [],
    }

    msg, state <- messageLoop stream initResult

    when msg is
        ParseComplete ->
            loopWith state

        BindComplete ->
            loopWith state

        RowDescription fields ->
            loopWith { state & fields: fields }

        DataRow row ->
            loopWith { state & rows: List.append state.rows row }

        CommandComplete _ ->
            loopWith state

        EmptyQueryResponse ->
            loopWith state

        ReadyForQuery _ ->
            Pg.Result.create state
            |> Done
            |> Task.succeed

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

messageLoop = \stream, initState, stepFn ->
    initBytes <- Tcp.readBytes stream |> await

    { bytes, state } <- Task.loop { bytes: initBytes, state: initState }

    if List.isEmpty bytes then
        Task.fail UnexpectedEnd
    else
        { decoded, remaining } <- Protocol.Backend.decode bytes |> Task.fromResult |> await

        when decoded is
            ErrorResponse error ->
                Task.fail (ErrorResponse error)

            ParameterStatus _ ->
                Task.succeed (Step { bytes: remaining, state })

            _ ->
                result <- stepFn decoded state |> Task.map

                when result is
                    Step newState ->
                        Step { bytes: remaining, state: newState }

                    Done done ->
                        Done done

loopWith = \state ->
    Task.succeed (Step state)

unexpected = \msg ->
    Task.fail (UnexpectedMsg msg)
