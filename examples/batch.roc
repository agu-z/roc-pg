app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br",
    pg: "../src/main.roc",
}

import pf.Stdout
import pg.Pg.Cmd
import pg.Pg.Client
import pg.Pg.Batch
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

    result_with =
        Pg.Batch.succeed(|hi| |eleven| |thirty_one| { hi, forty_two: eleven + thirty_one })
        |> Pg.Batch.with(
            (
                Pg.Cmd.new("select 'hi' as value")
                |> Pg.Cmd.expect1(Pg.Result.str("value"))
            ),
        )
        |> Pg.Batch.with(
            (
                Pg.Cmd.new("select $1::int as value")
                |> Pg.Cmd.bind([Pg.Cmd.u8(11)])
                |> Pg.Cmd.expect1(Pg.Result.u8("value"))
            ),
        )
        |> Pg.Batch.with(
            (
                Pg.Cmd.new("select $1::int as value")
                |> Pg.Cmd.bind([Pg.Cmd.u8(31)])
                |> Pg.Cmd.expect1(Pg.Result.u8("value"))
            ),
        )
        |> Pg.Client.batch!(client)?

    str42 = Num.to_str(result_with.forty_two)
    _ = Stdout.line!("${result_with.hi} ${str42}")

    result_seq =
        List.range({ start: At(0), end: At(20) })
        |> List.map(
            |num|
                Pg.Cmd.new("select $1::int as value")
                |> Pg.Cmd.bind([Pg.Cmd.u8(num)])
                |> Pg.Cmd.expect1(Pg.Result.u8("value")),
        )
        |> Pg.Batch.sequence
        |> Pg.Client.batch!(client)?

    result_seq_str = result_seq |> List.map(Num.to_str) |> Str.join_with(", ")

    Stdout.line!(result_seq_str)
