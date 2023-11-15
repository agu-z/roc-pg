app "roc-sql"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        pg: "../../src/main.roc",
    }
    imports [
        pf.Task.{ Task, await },
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
        # Unused but required because of: https://github.com/roc-lang/roc/issues/5477
        pf.Tcp,
        pg.Cmd,
    ]
    provides [main] to pf

Options : {
    host : Str,
    port : U16,
    user : Str,
    database : Str,
    schema : Str,
}

generate : Options -> Task {} _
generate = \options ->
    client <- Pg.Client.withConnect {
            host: options.host,
            port: options.port,
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

# Can't use in latest basic-cli because of:  https://github.com/roc-lang/basic-cli/issues/90
# argsParser : ArgParser.NamedParser Options
# argsParser =
#    ArgParser.succeed {
#        host: <-
#            ArgParser.strOption {
#                long: "host",
#                short: "h",
#                help: "database server host",
#            }
#            |> applyParser,
#        port: <-
#            ArgParser.i64Option {
#                long: "port",
#                short: "p",
#                help: "database server port",
#            }
#            |> applyParser,
#        user: <-
#            ArgParser.strOption {
#                long: "user",
#                short: "U",
#                help: "database user name",
#            }
#            |> applyParser,
#        database: <-
#            ArgParser.strOption {
#                long: "database",
#                short: "d",
#                help: "database name to connect to",
#            }
#            |> applyParser,
#        schema: <-
#            ArgParser.strOption {
#                long: "schema",
#                help: "specific schema to generate",
#            }
#            |> applyParser,
#    }
#    |> ArgParser.program {
#        name: "roc-sql",
#        help: "Generate Roc for your PostgreSQL schema",
#    }
#
# applyParser = \a -> \fn -> ArgParser.withParser fn a

# Poor man's arg parser:
parseArgs = \args ->
    dropFirstStr : Str, Nat -> Str
    dropFirstStr = \val, n ->
        # Sorry unicode gods
        val
        |> Str.toUtf8
        |> List.dropFirst n
        |> Str.fromUtf8
        |> Result.withDefault val

    initState = {
        args: Dict.withCapacity 5,
        current: None,
    }

    args
    |> List.dropFirst 1
    |> List.walkTry initState \state, arg ->
        when state.current is
            None ->
                if Str.startsWith arg "--" then
                    Ok { state & current: Param (dropFirstStr arg 2) }
                else if Str.startsWith arg "-" then
                    Ok { state & current: Param (dropFirstStr arg 1) }
                else
                    Err "Expected param"

            Param param ->
                { state &
                    args: Dict.insert state.args param arg,
                    current: None,
                }
                |> Ok
    |> Result.try \state ->
        when state.current is
            None ->
                Ok state.args

            Param arg ->
                Err "Missing value for \(arg)"
    |> Result.try \argsDict ->
        arg = \{ long, short ? "" } -> \fnResult ->
                getResult =
                    Dict.get argsDict long
                    |> Result.onErr \_ -> Dict.get argsDict short

                when (fnResult, getResult) is
                    (Ok fn, Ok value) ->
                        Ok (fn value)

                    _ ->
                        Err "The option `--\(long)` is required but was not provided!"

        Ok {
            host: <- arg { long: "host", short: "h" },
            port: <- arg { long: "port", short: "p" },
            user: <- arg { long: "user", short: "U" },
            database: <- arg { long: "database", short: "d" },
            schema: <- arg { long: "schema" },
        }
    |> Result.try \options ->
        # I could make a special `arg` but I'm in a hurry
        when Str.toU16 options.port is
            Ok port ->
                Ok {
                    port,
                    host: options.host,
                    user: options.user,
                    database: options.database,
                    schema: options.schema,
                }

            Err InvalidNumStr ->
                Err "Invalid port number: \(options.port)"
    |> Result.mapErr \err ->
        """
        Generate Roc for your PostgreSQL schema

        OPTIONS:
            --host, -h    database server host  (string)
            --port, -p    database server port  (integer, 16-bit signed)
            --user, -U    database user name  (string)
            --database, -d    database name to connect to  (string)
            --schema    specific schema to generate  (string)

        \(err)
        """

main : Task {} I32
main =
    args <- Arg.list |> await

    dbg
        parseArgs args

    when parseArgs args is
        Ok options ->
            runGenerateTask options

        Err help ->
            {} <- Stdout.line help |> Task.await
            Task.err 1

runGenerateTask : Options -> Task {} I32
runGenerateTask = \options ->
    result <- Task.attempt (generate options)

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
            Task.err 1
