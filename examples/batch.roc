app [main] {
    pg: "../src/main.roc",
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
}

import pf.Task exposing [Task, await]
import pf.Stdout
import pg.Pg.Cmd
import pg.Pg.BasicCliClient
import pg.Pg.Batch
import pg.Pg.Result

main =
    client <- Pg.BasicCliClient.withConnect {
            host: "localhost",
            port: 5432,
            user: "postgres",
            auth: None,
            database: "postgres",
        }

    Stdout.line! "Connected!"

    resultWith =
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
            |> Pg.BasicCliClient.batch! client

    str42 = Num.toStr resultWith.fortyTwo
    Stdout.line! "$(resultWith.hi) $(str42)"

    resultSeq =
        List.range { start: At 0, end: At 20 }
            |> List.map \num ->
                Pg.Cmd.new "select $1::int as value"
                |> Pg.Cmd.bind [Pg.Cmd.u8 num]
                |> Pg.Cmd.expect1 (Pg.Result.u8 "value")
            |> Pg.Batch.sequence
            |> Pg.BasicCliClient.batch! client

    resultSeqStr = resultSeq |> List.map Num.toStr |> Str.joinWith ", "

    Stdout.line resultSeqStr
