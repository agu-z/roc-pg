app [main] {
    pg: "../src/main.roc",
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
}

import pf.Task exposing [Task, await]
import pf.Stdout
import pg.Pg.Cmd
import pg.Pg.BasicCliClient
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

    addCmd =
        "select $1::int + $2::int as result"
            |> Pg.BasicCliClient.prepare! { client, name: "add" }

    addAndPrint = \a, b ->
        result =
            addCmd
                |> Pg.Cmd.bind [Pg.Cmd.u8 a, Pg.Cmd.u8 b]
                |> Pg.Cmd.expect1 (Pg.Result.u8 "result")
                |> Pg.BasicCliClient.command! client

        aStr = Num.toStr a
        bStr = Num.toStr b
        resultStr = Num.toStr result

        Stdout.line "$(aStr) + $(bStr) = $(resultStr)"
    
    addAndPrint! 1 2
    addAndPrint! 11 31
