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
        sql.Sql.{ from, select, where, eq, and, into, with, str, rowArray },
        Schema,
    ]
    provides [main] to pf

generate = \client ->
    tables <-
        Sql.all tablesQuery
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

tablesQuery =
    tables <- from Schema.tables

    into \name -> \columns -> { name, columns }
    |> with tables.name
    |> rowArray (columnsQuery tables)
    |> select
    |> where (tables.schema |> eq (str "public"))

columnsQuery = \tables ->
    columns <- from Schema.columns

    into \name -> \dataType -> { name, dataType }
    |> with columns.name
    |> with columns.dataType
    |> select
    |> where (eq columns.tableName tables.name |> and (eq columns.schema tables.schema))

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
