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

    rows =
        Pg.Cmd.new
            """
            select $1 as name, $2 as age
            union all
            select 'Julio' as name, 23 as age
            """
            |> Pg.Cmd.bind [Pg.Cmd.str "John", Pg.Cmd.u8 32]
            |> Pg.Cmd.expectN
                (
                    Pg.Result.succeed
                        (\name -> \age ->
                                ageStr = Num.toStr age

                                "$(name): $(ageStr)"
                        )
                    |> Pg.Result.with (Pg.Result.str "name")
                    |> Pg.Result.with (Pg.Result.u8 "age")
                )
            |> Pg.BasicCliClient.command! client

    Stdout.line (Str.joinWith rows "\n")
