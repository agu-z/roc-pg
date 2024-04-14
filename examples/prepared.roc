app "prepared"
    packages {
        pg: "../src/main.roc",
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
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

    addCmd <-
        "select $1::int + $2::int as result"
        |> Pg.BasicCliClient.prepare { client, name: "add" }
        |> await

    addAndPrint = \a, b ->
        result <-
            addCmd
            |> Pg.Cmd.bind [Pg.Cmd.u8 a, Pg.Cmd.u8 b]
            |> Pg.Cmd.expect1 (Pg.Result.u8 "result")
            |> Pg.BasicCliClient.command client
            |> await

        aStr = Num.toStr a
        bStr = Num.toStr b
        resultStr = Num.toStr result

        Stdout.line "$(aStr) + $(bStr) = $(resultStr)"

    _ <- addAndPrint 1 2 |> await

    addAndPrint 11 31

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
                _ <- Stderr.line (Inspect.toStr err) |> await
                Task.err 1
