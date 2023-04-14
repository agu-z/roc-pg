app "batch"
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
        pg.Pg.Batch,
        pg.Pg.Result,
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

    result <-
        Pg.Batch.succeed (\hi -> \fortyTwo -> { hi, fortyTwo })
        |> Pg.Batch.with
            (
                Pg.Cmd.new "select 'hi' as value"
                |> Pg.Cmd.expect1 (Pg.Result.str "value")
            )
        |> Pg.Batch.with
            (
                Pg.Cmd.new "select 42 as value"
                |> Pg.Cmd.expect1 (Pg.Result.u8 "value")
            )
        |> Pg.Client.batch client
        |> await

    str42 = Num.toStr result.fortyTwo

    Stdout.line "\(result.hi) \(str42)"

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
