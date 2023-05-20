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
        pf.Arg,
        pg.Pg.Client,
        pg.Pg.Cmd,
        pg.Pg.Result,
        sql.Sql.{ from, select, where, eq, and, into, column, str, rowArray },
        Schema,
        Generate,

    ]
    provides [main] to pf

Options : {
    host : Str,
    port : I64,
    user : Str,
    database : Str,
    schema : Str,
}

generate : Options -> Task {} _
generate = \options ->
    client <- Pg.Client.withConnect {
            host: options.host,
            port: Num.toU16 options.port,
            user: options.user,
            database: options.database,
        }

    tables <-
        Sql.all (tablesQuery options.schema)
        |> Pg.Client.command client
        |> await

    Stdout.line (Generate.module options.schema tables)

tablesQuery = \schemaName ->
    tables <- from Schema.tables

    into {
        name: <- column tables.name,
        schema: <- column tables.schema,
        columns: <- rowArray (columnsQuery tables),
    }
    |> select
    |> where (tables.schema |> eq (str schemaName))

columnsQuery = \tables ->
    columns <- from Schema.columns

    into {
        name: <- column columns.name,
        dataType: <- column columns.dataType,
        isNullable: <-
            columns.isNullable
            |> eq (str "YES")
            |> column,
    }
    |> select
    |> where (eq columns.tableName tables.name |> and (eq columns.schema tables.schema))

argsParser : Arg.NamedParser Options
argsParser =
    Arg.succeed {
        host: <-
            Arg.strOption {
                long: "host",
                short: "h",
                help: "database server host",
            }
            |> applyParser,
        port: <-
            Arg.i64Option {
                long: "port",
                short: "p",
                help: "database server port",
            }
            |> applyParser,
        user: <-
            Arg.strOption {
                long: "user",
                short: "U",
                help: "database user name",
            }
            |> applyParser,
        database: <-
            Arg.strOption {
                long: "database",
                short: "d",
                help: "database name to connect to",
            }
            |> applyParser,
        schema: <-
            Arg.strOption {
                long: "schema",
                help: "specific schema to generate"
            }
            |> applyParser,
    }
    |> Arg.program {
        name: "roc-sql",
        help: "Generate Roc for your PostgreSQL schema",
    }

applyParser = \a -> \fn -> Arg.withParser fn a

main : Task {} []
main =
    args <- Arg.list |> await

    when Arg.parseFormatted argsParser args is
        Ok options ->
            runGenerateTask options

        Err helpMenu ->
            {} <- Stdout.line helpMenu |> Task.await
            Process.exit 1

runGenerateTask : Options -> Task {} []
runGenerateTask = \options ->
    result <- Task.attempt (generate options)

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
