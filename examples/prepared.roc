app "prepared"
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

    addCmd <-
        """
        select $1 + $2 as result
        """
        |> Pg.Client.prepare {client, name: "add"}
        |> await

    addAndPrint = \a, b ->
        result <-
            addCmd
            |> Pg.Cmd.bind [Pg.Cmd.u8 a, Pg.Cmd.u8 b]
            |> Pg.Client.command client
            |> await       

        decoded <-
            result
            |> Pg.Result.decode (Pg.Result.u8 "result")
            |> Result.try List.first
            |> Task.fromResult
            |> await

        aStr = Num.toStr a
        bStr = Num.toStr b
        resultStr = Num.toStr decoded

        Stdout.line "\(aStr) + \(bStr) = \(resultStr)"

    _ <- addAndPrint 1 2 |> await

    addAndPrint 11 31

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
