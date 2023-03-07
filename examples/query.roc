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
        Json.{ Json },
        pg.Bytes.Encode,
        pg.Bytes.Decode,
    ]
    provides [main] to pf

main : Task {} []
main =
    task =
        stream <- Tcp.withConnect "localhost" 5432
        _ <- Tcp.writeBytes startupMsg stream |> await
        received <- Tcp.readBytes stream |> await

        msg =
            Bytes.Decode.decode received msgDecoder
            |> Result.withDefault (UnknownMessageType 0)

        if msg == AuthenticationOk then
            Stdout.line "authed"
        else
            Stdout.line "something else"

    Task.attempt task \result ->
        when result is
            Ok _ ->
                Process.exit 0

            Err _ ->
                Process.exit 1

startupMsg : List U8
startupMsg =
    Bytes.Encode.sequence [
        # Version number
        Bytes.Encode.i16 3,
        Bytes.Encode.i16 0,
        # Encoding
        Bytes.Encode.sequence [
            param "client_encoding" "utf_8",
            param "user" "aguz",
            param "database" "aguz",
        ]
        |> Bytes.Encode.nullTerminate,
    ]
    |> withLength

msgDecoder =
    messageType <- Bytes.Decode.await Bytes.Decode.u8
    _messageLength <- Bytes.Decode.await Bytes.Decode.i32

    when messageType is
        'R' ->
            authMsgDecoder

        _ ->
            Bytes.Decode.succeed (UnknownMessageType messageType)

authMsgDecoder =
    success <- Bytes.Decode.await Bytes.Decode.i32

    if success == 0 then
        Bytes.Decode.succeed AuthenticationOk
    else
        Bytes.Decode.succeed AuthenticationFailed

# Message helpers

param : Str, Str -> List U8
param = \key, value ->
    Bytes.Encode.sequence [
        Bytes.Encode.cStr key,
        Bytes.Encode.cStr value,
    ]

withLength : List U8 -> List U8
withLength = \msg ->
    totalLength =
        List.len msg
        + 4
        |> Num.toI32

    List.concat (Bytes.Encode.i32 totalLength) msg

# Debug helpers

printBytes : List U8 -> Task {} []
printBytes = \bytes ->
    bytes
    |> List.map Num.toStr
    |> Str.joinWith ", "
    |> Stdout.line
