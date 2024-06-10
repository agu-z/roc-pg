app [main] {
    pg: "../src/main.roc",
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
}

import pf.Task exposing [Task, await]
import pf.Stdout
import pf.Stderr
import pg.Pg.Cmd
import pg.Pg.BasicCliClient
import pg.Pg.Batch
import pg.Pg.Result
# Unused but required because of: https://github.com/roc-lang/roc/issues/5477
import pf.Tcp
import pg.Cmd

task =
    client <- Pg.BasicCliClient.withConnect {
            host: "localhost",
            port: 5432,
            user: "postgres",
            auth: None,
            database: "postgres",
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
        |> Pg.BasicCliClient.batch client
        |> await

    str42 = Num.toStr resultWith.fortyTwo
    _ <- Stdout.line "$(resultWith.hi) $(str42)" |> await

    resultSeq <-
        List.range { start: At 0, end: At 20 }
        |> List.map \num ->
            Pg.Cmd.new "select $1::int as value"
            |> Pg.Cmd.bind [Pg.Cmd.u8 num]
            |> Pg.Cmd.expect1 (Pg.Result.u8 "value")
        |> Pg.Batch.sequence
        |> Pg.BasicCliClient.batch client
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
                _ <- Stderr.line (Pg.BasicCliClient.errorToStr err) |> await
                Task.err 2

            Err err ->
                _ <- Stderr.line (Inspect.toStr err) |> await
                Task.err 1
