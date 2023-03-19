app "query"
    packages {
        pf: "../../basic-cli/src/main.roc",
        pg: "../src/main.roc",
    }
    imports [
        pf.Task.{ Task, await },
        pf.Process,
        pf.Stdout,
        pf.Stderr,
        pg.Pg.Bind,
        pg.Pg.Client,
        pg.Pg.Result.{ succeed, apply },
    ]
    provides [main] to pf

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
            select $1 as name, $2 as age 
            union all
            select 'Julio' as name, 23 as age
            """
            [
                Pg.Bind.str "John",
                Pg.Bind.i32 25,
            ]
        |> await

    dbg
        result

    out <-
        result
        |> Pg.Result.decode
            (
                succeed
                    (\name -> \age ->
                            ageStr = Num.toStr age

                            "\(name): \(ageStr)"
                    )
                |> apply (Pg.Result.str "name")
                |> apply (Pg.Result.i32 "age")
            )
        |> Result.map (\rows -> Str.joinWith rows "\n")
        |> Task.fromResult
        |> await

    Stdout.line out

main : Task {} []
main =
    Task.attempt task \result ->
        when result is
            Ok _ ->
                Process.exit 0

            Err (ErrorResponse err) ->
                _ <- Stderr.line (Pg.Client.errorToStr err) |> await
                Process.exit 2

            Err _ ->
                _ <- Stderr.line "Something went wrong" |> await
                Process.exit 1
