app "query"
    packages {
        pf: "../../basic-cli/src/main.roc",
        pg: "../src/main.roc",
    }
    imports [
        pf.Task.{ Task, await },
        pf.Process,
        pf.Stdout,
        pg.Pg.Client,
    ]
    provides [main] to pf

main : Task {} []
main =
    task =
        client <- Pg.Client.withConnect {
                host: "localhost",
                port: 5432,
                user: "aguz",
                database: "aguz",
            }

        _ <- Stdout.line "Connected!" |> await

        result <- client
            |> Pg.Client.query "SELECT 'Hi Roc!' as message, 42 as answer"
            |> await

        rows =
            result.rows
            |> List.map (\row -> List.keepOks row Str.fromUtf8 |> Str.joinWith ", ")
            |> Str.joinWith "\n"

        Stdout.line "\nResult:\n\(rows)"

    Task.attempt task \result ->
        when result is
            Ok _ ->
                Process.exit 0

            Err err ->
                dbg
                    err

                Process.exit 1
