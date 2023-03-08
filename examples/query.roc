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
        backendMsg = Protocol.Backend.decode received

        when backendMsg is
            Ok { decoded, remaining } ->
                when decoded is
                    AuthOk ->
                        Stdout.line "Authenticated!"

                    AuthRequired ->
                        Stdout.line "Authentication required"

                    ErrorResponse error ->
                        Stdout.line (Protocol.Backend.errorToStr error)

                    Unrecognized msgType ->
                        msgCode =
                            Str.fromUtf8 [msgType]
                            |> Result.withDefault (Num.toStr msgType)

                        remainingBytesStr =
                            remaining
                            |> List.map Num.toStr
                            |> Str.joinWith ", "

                        Stdout.line "Unrecognized backend message: \(msgCode)\n\(remainingBytesStr)"

            Err _ ->
                Stdout.line "failed to decode"

    Task.attempt task \result ->
        when result is
            Ok _ ->
                Process.exit 0

            Err _ ->
                Process.exit 1

