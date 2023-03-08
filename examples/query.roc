app "query"
    packages {
        pf: "../../basic-cli/src/main.roc",
        pg: "../src/main.roc",
    }
    imports [
        pf.Tcp,
        pf.Task.{ Task, await },
        pf.Stdout,
        pf.Process,
        Json,
        pg.Protocol.Frontend,
        pg.Protocol.Backend,
    ]
    provides [main] to pf

main : Task {} []
main =
    task =
        stream <- Tcp.withConnect "localhost" 5432

        startupMsg = Protocol.Frontend.startup { user: "aguz" }
        _ <- Tcp.writeBytes startupMsg stream |> await

        received <- Tcp.readBytes stream |> await
        Task.loop received messageTick

    Task.attempt task \result ->
        when result is
            Ok _ ->
                Process.exit 0

            Err _ ->
                Process.exit 1

messageTick : List U8 -> Task [Step (List U8), Done {}] _
messageTick = \bytes ->
    if List.isEmpty bytes then
        Task.succeed (Done {})
    else
        Task.map (handleMessage bytes) Step


handleMessage : List U8 -> Task (List U8) _
handleMessage = \bytes ->
    backendMsg = Protocol.Backend.decode bytes

    when backendMsg is
        Ok { decoded, remaining } ->
            when decoded is
                AuthOk ->
                    _ <- Stdout.line "Authenticated!" |> await
                    Task.succeed remaining

                AuthRequired ->
                    _ <- Stdout.line "Authentication required" |> await
                    Task.succeed remaining

                ParameterStatus { name, value } ->
                    _ <- Stdout.line "\(name): \(value)" |> await
                    Task.succeed remaining

                ErrorResponse error ->
                    _ <- Stdout.line (Protocol.Backend.errorToStr error) |> await
                    Task.succeed remaining

        Err (UnrecognizedBackendMessage msgType) ->
            msgCode =
                Str.fromUtf8 [msgType]
                |> Result.withDefault (Num.toStr msgType)

            _ <- Stdout.line "Unrecognized backend message: \(msgCode)" |> await
            Task.succeed []

        Err _ ->
            _ <- Stdout.line "Failed to decode backend message" |> await
            Task.succeed []
