interface Client
    exposes [
        withConnect,
        query,
    ]
    imports [
        Protocol.Backend,
        Protocol.Frontend,
        pf.Task.{ Task, await },
        pf.Tcp,
    ]

Client := {
    stream : Tcp.Stream,
    parameters : Dict Str Str,
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

        ParameterStatus { name, value } ->
            loopWith { state & parameters: Dict.insert state.parameters name value }

        BackendKeyData backendKey ->
            loopWith { state & backendKey: Ok backendKey }

        ReadyForQuery _ ->
            @Client {
                stream,
                parameters: state.parameters,
                backendKey: state.backendKey,
            }
            |> callback
            |> Task.map Done

        ErrorResponse error ->
            Task.fail (ErrorResponse error)

        _ ->
            Task.fail (UnexpectedMsg msg)

query = \@Client { stream }, sql ->
    _ <- Protocol.Frontend.query sql
        |> Tcp.writeBytes stream
        |> await

    msg, state <- messageLoop stream {
            fields: [],
            rows: [],
        }
    when msg is
        RowDescription fields ->
            loopWith { state & fields: fields }

        DataRow row ->
            loopWith { state & rows: List.append state.rows row }

        CommandComplete _ ->
            loopWith state

        ReadyForQuery _ ->
            Task.succeed (Done state)

        ErrorResponse error ->
            Task.fail (ErrorResponse error)

        _ ->
            Task.fail (UnexpectedMsg msg)

# Helpers

messageLoop = \stream, initState, step ->
    initBytes <- Tcp.readBytes stream |> await

    { bytes, state } <- Task.loop { bytes: initBytes, state: initState }

    if List.isEmpty bytes then
        Task.fail UnexpectedEndOfMessages
    else
        backendMsg = Protocol.Backend.decode bytes

        when backendMsg is
            Ok { decoded, remaining } ->
                result <- Task.map (step decoded state)

                when result is
                    Step newState ->
                        Step { bytes: remaining, state: newState }

                    Done done ->
                        Done done

            Err err ->
                Task.fail err

loopWith = \state ->
    Task.succeed (Step state)
