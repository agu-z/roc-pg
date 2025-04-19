app [Model, init!, respond!] {
    pf: platform "https://github.com/roc-lang/basic-webserver/releases/download/0.12.0/Q4h_In-sz1BqAvlpmCsBHhEJnn_YvfRRMiNACB_fBbk.tar.br",
    pg: "../src/main.roc",
}

import pf.Stdout
import pg.Pg.Client exposing [Client]
import pg.Pg.Cmd
import pg.Pg.Result

Model : {
    client : Client,
}

init! = |_|
    _ = Stdout.line!("Start!")

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

    Ok({ client })

respond! = |_request, model|
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
        |> Pg.Client.command!(model.client)?

    response_body =
        rows
        |> List.map(Inspect.to_str)
        |> Str.join_with("\n")

    Stdout.line!("Responding with: ${response_body}")?

    Ok(
        {
            status: 200,
            headers: [],
            body: Str.to_utf8(response_body),
        },
    )
