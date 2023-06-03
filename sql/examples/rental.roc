app "rental"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.2/tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk.tar.br",
        pg: "../../src/main.roc",
        sql: "../src/main.roc",
    }
    imports [
        pf.Task.{ Task, await },
        pf.Process,
        pf.Stdout,
        pf.Stderr,
        pf.Arg,
        pg.Pg.Cmd,
        pg.Pg.Client,
        pg.Pg.Result,
        Json,
        sql.Sql.{ select, into, from, column, where, eq, i32, join, on },
        sql.Sql.Types.{ Nullable },
        VideoRental,
    ]
    provides [main] to pf

printCustomer : I32 -> Task {} [TcpConnectErr _, TcpPerformErr _]
printCustomer = \customerId ->
    client <- Pg.Client.withConnect {
            host: "localhost",
            port: 5432,
            user: "postgres",
            database: "video_rental",
        }

    query =
        customer <- from VideoRental.customer
        customerAddr <- join VideoRental.address (on .addressId customer.addressId)
        store <- join VideoRental.store (on .storeId customer.storeId)
        storeAddr <- join VideoRental.address (on .addressId store.addressId)

        fullName =
            customer.firstName
            |> Sql.concat (Sql.str " ")
            |> Sql.concat customer.lastName

        into {
            fullName: <- column fullName,
            address: <- selectAddress customerAddr |> Sql.with,
            storeAddress: <- selectAddress storeAddr |> Sql.with,
        }
        |> select
        |> where (customer.customerId |> eq (i32 customerId))

    _ <- logSql query |> await

    data <-
        Sql.queryOne query
        |> Pg.Client.command client
        |> await

    printJson data

Address : {
    line1 : Str,
    district : Str,
    postalCode : Nullable Str,
}

selectAddress : _ -> Sql.Selection Address
selectAddress = \table ->
    into {
        line1: <- table.address |> column,
        district: <- table.district |> column,
        postalCode: <- table.postalCode |> column,
    }

main : Task {} []
main =
    args <- Arg.list |> await

    argsResult =
        Arg.i64Option {
            long: "customer",
        }
        |> Arg.program {
            name: "video-rental",
        }
        |> Arg.parseFormatted args

    when argsResult is
        Ok customerId ->
            customerId
            |> Num.toI32
            |> printCustomer
            |> handlePgTask

        Err helpMenu ->
            _ <- Stdout.line helpMenu |> Task.await
            Process.exit 1

handlePgTask : Task {} [TcpConnectErr _, TcpPerformErr _] -> Task {} []
handlePgTask = \task ->
    result <- Task.attempt task

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
            Process.exit 3

printJson = \value ->
    Encode.toBytes value Json.json
    |> Str.fromUtf8
    |> Result.withDefault ""
    |> Stdout.line

logSql = \query ->
    sqlStr = (Sql.compileQuery query).sql
    Stdout.line "SQL: \(sqlStr)\n"
