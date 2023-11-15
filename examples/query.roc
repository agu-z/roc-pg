app "query"
    packages {
        pg: "../src/main.roc",
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
    }
    imports [
        pf.Task.{ Task, await },
        pf.Stdout,
        pf.Stderr,
        pg.Pg.Cmd,
        pg.Pg.Client,
        pg.Pg.Result,
        # Unused but required because of: https://github.com/roc-lang/roc/issues/5477
        pf.Tcp,
        pg.Cmd,
    ]
    provides [main] to pf

task =
    client <- Pg.Client.withConnect {
            host: "localhost",
            port: 5432,
            user: "aguz",
            auth: Password "the_password",
            database: "aguz",
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
        |> Pg.Client.command client
        |> await

    Stdout.line (Str.joinWith rows "\n")

main : Task {} I32
main =
    Task.attempt task \result ->
        when result is
            Ok _ ->
                Task.ok {}

            Err (TcpPerformErr (PgErr err)) ->
                _ <- Stderr.line (Pg.Client.errorToStr err) |> await
                Task.err 2

            Err err ->
                dbg
                    err

                _ <- Stderr.line "Something went wrong" |> await
                Task.err 2
