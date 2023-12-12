app "query"
    packages {
        pg: "../src/main.roc",
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Task.{ Task, await },
        pf.Stdout,
        pf.Stderr,
        pg.Pg.Cmd,
        pg.Pg.BasicCliClient,
        pg.Pg.Result,
        # Unused but required because of: https://github.com/roc-lang/roc/issues/5477
        pf.Tcp,
        pg.Cmd,
    ]
    provides [main] to pf

task =
    client <- Pg.BasicCliClient.withConnect {
        host: "localhost",
        port: 5432,
        user: "postgres",
        auth: None,
        database: "postgres",
    }

    _ <- Stdout.line "Connected!" |> await

    rows <-
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

                            "\(name): \(ageStr)"
                    )
                |> Pg.Result.with (Pg.Result.str "name")
                |> Pg.Result.with (Pg.Result.u8 "age")
            )
        |> Pg.BasicCliClient.command client
        |> await

    Stdout.line (Str.joinWith rows "\n")

main : Task {} I32
main =
    Task.attempt task \result ->
        when result is
            Ok _ ->
                Task.ok {}

            Err (TcpPerformErr (PgErr err)) ->
                _ <- Stderr.line (Pg.BasicCliClient.errorToStr err) |> await
                Task.err 2

            Err err ->
                dbg
                    err

                _ <- Stderr.line "Something went wrong" |> await
                Task.err 2
