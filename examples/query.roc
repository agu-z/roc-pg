app "query"
    packages {
        pf: "https://github.com/agu-z/roc-basic-cli/releases/download/0.5.0/S8r4wytSGYKi-iMrjaRZxv2Hope_CX7dF6rMdciYja8.tar.gz",
        pg: "../src/main.roc",
    }
    imports [
        pf.Task.{ Task, await },
        pf.Process,
        pf.Stdout,
        pf.Stderr,
        pg.Pg.Cmd,
        pg.Pg.Client,
        pg.Pg.Result.{ succeed, apply },
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
                succeed
                    (\name -> \age ->
                            ageStr = Num.toStr age

                            "\(name): \(ageStr)"
                    )
                |> apply (Pg.Result.str "name")
                |> apply (Pg.Result.u8 "age")
            )
        |> Pg.Client.execute client
        |> await

    Stdout.line (Str.joinWith rows "\n")

main : Task {} []
main =
    Task.attempt task \result ->
        when result is
            Ok _ ->
                Process.exit 0

            Err (TcpPerformErr (PgErr err)) ->
                _ <- Stderr.line (Pg.Client.errorToStr err) |> await
                Process.exit 2

            Err err ->
                dbg
                    err

                _ <- Stderr.line "Something went wrong" |> await
                Process.exit 1
