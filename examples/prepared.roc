app [main] {
    pg: "../src/main.roc",
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
}

import pf.Stdout
import pg.Pg.Cmd
import pg.Pg.BasicCliClient
import pg.Pg.Result

main =
    client = Pg.BasicCliClient.connect! {
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
