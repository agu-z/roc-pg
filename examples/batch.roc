app "batch"
    packages {
        pg: "../src/main.roc",
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
    }
    imports [
        pf.Task.{ Task, await },
        pf.Stdout,
        pf.Stderr,
        pg.Pg.Cmd,
        pg.Pg.Client,
        pg.Pg.Batch,
        pg.Pg.Result,
        # Unused but required because of: https://github.com/roc-lang/roc/issues/5477
        pf.Tcp,
        pg.Cmd,
    ]
    provides [main] to pf

task =
    client <- Pg.Client.withConnect {
            host: "localhost",
            port: 5432,
            user: "aguz",
            auth: Password "the_password",
            database: "aguz",
        }

    _ <- Stdout.line "Connected!" |> await

    resultWith <-
        Pg.Batch.succeed (\hi -> \eleven -> \thirtyOne -> { hi, fortyTwo: eleven + thirtyOne })
        |> Pg.Batch.with
            (
                Pg.Cmd.new "select 'hi' as value"
                |> Pg.Cmd.expect1 (Pg.Result.str "value")
            )
        |> Pg.Batch.with
            (
                Pg.Cmd.new "select $1::int as value"
                |> Pg.Cmd.bind [Pg.Cmd.u8 11]
                |> Pg.Cmd.expect1 (Pg.Result.u8 "value")
            )
        |> Pg.Batch.with
            (
                Pg.Cmd.new "select $1::int as value"
                |> Pg.Cmd.bind [Pg.Cmd.u8 31]
                |> Pg.Cmd.expect1 (Pg.Result.u8 "value")
            )
        |> Pg.Client.batch client
        |> await

    str42 = Num.toStr resultWith.fortyTwo
    _ <- Stdout.line "\(resultWith.hi) \(str42)" |> await

    resultSeq <-
        List.range { start: At 0, end: At 20 }
        |> List.map \num ->
            Pg.Cmd.new "select $1::int as value"
            |> Pg.Cmd.bind [Pg.Cmd.u8 num]
            |> Pg.Cmd.expect1 (Pg.Result.u8 "value")
        |> Pg.Batch.sequence
        |> Pg.Client.batch client
        |> await

    resultSeqStr = resultSeq |> List.map Num.toStr |> Str.joinWith ", "

    Stdout.line resultSeqStr

main : Task {} I32
main =
    Task.attempt task \result ->
        when result is
            Ok _ ->
                Task.ok {}

            Err (TcpPerformErr (PgErr err)) ->
                _ <- Stderr.line (Pg.Client.errorToStr err) |> await
                Task.err 2

            Err err ->
                dbg
                    err

                _ <- Stderr.line "Something went wrong" |> await
                Task.err 1
