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
        pg.Pg.Result.{ succeed, apply },
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
            |> Pg.Client.query
                """
                select 'John' as name, 25 as age 
                union all
                select 'Julio' as name, 23 as age
                """
            |> await

        rows =
            result
            |> Pg.Result.decode
                (
                    succeed (\name -> \age -> { name, age })
                    |> apply (Pg.Result.str "name")
                    |> apply (Pg.Result.i32 "age")
                )

        dbg
            rows

        Stdout.line "Result"

    Task.attempt task \result ->
        when result is
            Ok _ ->
                Process.exit 0

            Err err ->
                dbg
                    err

                Process.exit 1
