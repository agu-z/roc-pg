app "query"
    packages {
        pf: "../../basic-cli/src/main.roc",
        pg: "../src/main.roc",
    }
    imports [
        pf.Task.{ Task },
        pf.Process,
        pf.Stdout,
        pg.Client,
    ]
    provides [main] to pf

main : Task {} []
main =
    task =
        _ <- Client.withConnect {
                host: "localhost",
                port: 5432,
                user: "aguz",
                database: "aguz",
            }

        Stdout.line "Connected!"

    Task.attempt task \result ->
        when result is
            Ok _ ->
                Process.exit 0

            Err err ->
                dbg
                    err

                Process.exit 1
