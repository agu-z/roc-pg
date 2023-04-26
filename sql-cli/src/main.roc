app "roc-sql"
    packages {
        pf: "https://github.com/agu-z/roc-basic-cli/releases/download/0.5.0/S8r4wytSGYKi-iMrjaRZxv2Hope_CX7dF6rMdciYja8.tar.gz",
        pg: "../../src/main.roc",
        sql: "../../sql/src/main.roc",
    }
    imports [
        pf.Task.{ Task, await },
        pf.Process,
        pf.Stdout,
        pf.Stderr,
        pg.Pg.Client,
        pg.Pg.Cmd,
        pg.Pg.Result,
        sql.Sql.{ from, select, where, eq, into, with, str, rowArray },
        Schema,
    ]
    provides [main] to pf

generate = \client ->
    query = 
        tables <- from Schema.tables

        into (\name -> \columns -> { name, columns })
        |> with tables.name
        |> rowArray
            (
                columns <- from Schema.columns

                selection =
                    into (\name -> \dataType -> { name, dataType })
                    |> with columns.name
                    |> with columns.dataType

                select selection
                |> where (columns.tableName |> eq tables.name)
            )
        |> select
        |> where (tables.schema |> eq (str "public"))

    tables <-
        Sql.all query
        |> Pg.Client.command client
        |> await

    tables
    |> List.map \table ->
        colCount = 
            table.columns 
            |> List.map .name 
            |> Str.joinWith ", "

        "\(table.name)(\(colCount))"
    |> Str.joinWith "\n"
    |> Stdout.line



main : Task {} []
main =
    task =
        Pg.Client.withConnect
            {
                host: "localhost",
                port: 5432,
                user: "postgres",
                database: "pagalia",
            }
            generate

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
