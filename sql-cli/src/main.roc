app "roc-sql"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.2/tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk.tar.br",
        pg: "../../src/main.roc",
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
        pg.Sql.{
            from,
            join,
            leftJoin,
            on,
            useOuter,
            select,
            where,
            eq,
            gt,
            not,
            into,
            column,
            in,
            str,
            i16,
            rowArray,
        },
        pg.Sql.Nullable.{ Nullable },
        PgCatalog,
        Generate,
        Schema,
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
        Sql.queryAll (tablesQuery options.schema)
        |> Pg.Client.command client
        |> await

    Schema.new options.schema tables
    |> Generate.module
    |> Stdout.line

tablesQuery = \schemaName ->
    tables <- from PgCatalog.pgClass
    schema <- join PgCatalog.pgNamespace (on .oid tables.relnamespace)

    into {
        id: <- column tables.oid,
        name: <- column tables.relname,
        columns: <- rowArray (columnsQuery tables),
        constraints: <- rowArray (constraintsQuery tables),
    }
    |> select
    |> where (schema.nspname |> eq (str schemaName))
    |> where
        (
            tables.relkind
            |> in str [
                "r", # ordinary table
                "v", # view
                "m", # materialized view
                "f", # foreign table
                "p", # partitioned table
            ]
        )

columnsQuery = \tables ->
    columns <- from PgCatalog.pgAttribute
    typ <- join PgCatalog.pgType (on .oid columns.atttypid)
    elemTyp <- leftJoin PgCatalog.pgType (on .oid typ.typelem)

    into {
        num: <- column columns.attnum,
        name: <- column columns.attname,
        dataType: <- column typ.typname,
        typeCategory: <- column typ.typcategory,
        elemDataType: <- column (useOuter elemTyp .typname),
        isNullable: <- column (not columns.attnotnull),
    }
    |> select
    |> where (Sql.discardPhantom tables.oid |> eq columns.attrelid) # exclude dropped columns
    |> where (not columns.attisdropped) # exclude system columns
    |> where (columns.attnum |> gt (i16 0))

constraintsQuery = \tables ->
    constraints <- from PgCatalog.pgConstraint

    into {
        type: <- column constraints.contype,
        columns: <- columnNumArray constraints.conkey,
        foreignTable: <- column constraints.confrelid,
        foreignColumns: <- columnNumArray constraints.confkey,
    }
    |> select
    |> where (tables.oid |> eq constraints.conrelid)
    |> where
        (
            constraints.contype
            |> in str [
                "p", # primary key
                "f", # foreign key
            ]
        )

# TODO: add helpers to Sql to make this simpler
columnNumArray = \expr ->
    into \nullableArray ->
        nullableArray
        |> Sql.Nullable.withDefault []
        |> List.map unwrapNullable
    |> (column expr)
    |> Sql.with

unwrapNullable : Nullable a -> a
unwrapNullable = \nullable ->
    when nullable is
        Null -> crash "unexpected null"
        NotNull a -> a

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
                help: "specific schema to generate",
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
