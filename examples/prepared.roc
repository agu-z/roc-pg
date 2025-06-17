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

    add_cmd =
        "select $1::int + $2::int as result"
        |> Pg.Client.prepare!({ client, name: "add" })?

    add_and_print! = |a, b|
        result =
            add_cmd
            |> Pg.Cmd.bind([Pg.Cmd.u8(a), Pg.Cmd.u8(b)])
            |> Pg.Cmd.expect1(Pg.Result.u8("result"))
            |> Pg.Client.command!(client)?

        a_str = Num.to_str(a)
        b_str = Num.to_str(b)
        result_str = Num.to_str(result)

        Stdout.line!("${a_str} + ${b_str} = ${result_str}")

    _ = add_and_print!(1, 2)
    add_and_print!(11, 31)
