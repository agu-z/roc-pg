app [main] {
    pg: "../src/main.roc",
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
}

import pf.Stdout
import pf.Tcp
import pg.Pg.Cmd
import pg.Pg.BasicCliClient {
    streamWrite: Tcp.write,
    streamReadExactly: Tcp.readExactly,
}
import pg.Pg.Result

main =
    stream = Tcp.connect! "localhost" 5432

    client = Pg.BasicCliClient.new! {
        user: "postgres",
        auth: None,
        database: "postgres",
        stream,
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
                { Pg.Result.combine <-
                    name: Pg.Result.str "name",
                    age: Pg.Result.u8 "age",
                }
            |> Pg.BasicCliClient.command! client

    rows
    |> List.map \row -> "$(row.name): $(Num.toStr row.age)"
    |> Str.joinWith "\n"
    |> Stdout.line
