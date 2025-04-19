app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br",
    pg: "../src/main.roc",
}

import pf.Stdout
import pg.Pg.Cmd
import pg.Pg.Client
import pg.Pg.Result

main! = |_|
    client = Pg.Client.connect!(
        {
            host: "localhost",
            port: 5432,
            user: "postgres",
            auth: None,
            database: "postgres",
        },
    )?

    _ = Stdout.line!("Connected!")

    rows =
        Pg.Cmd.new(
            """
            select $1 as name, $2 as age
            union all
            select 'Julio' as name, 23 as age
            """,
        )
        |> Pg.Cmd.bind([Pg.Cmd.str("John"), Pg.Cmd.u8(32)])
        |> Pg.Cmd.expect_n(
            { Pg.Result.record_builder <-
                name: Pg.Result.str("name"),
                age: Pg.Result.u8("age"),
            },
        )
        |> Pg.Client.command!(client)?

    str =
        rows
        |> List.map(Inspect.to_str)
        |> Str.join_with("\n")

    Stdout.line!("Got records:\n${str}")
