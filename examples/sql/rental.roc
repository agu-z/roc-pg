app "rental"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.5.0/jEPD_1ZLFiFrBeYKiKvHSisU-E3LZJeenfa9nvqJGeE.tar.br",
        pg: "../../src/main.roc",
    }
    imports [
        pf.Task.{ Task, await },
        pf.Stdout,
        pf.Stderr,
        pf.Arg,
        pg.Pg.Cmd,
        pg.Pg.Client,
        pg.Pg.Result,
        json.Core,
        pg.Sql.{ select, into, from, column, where, eq, i32, join, on },
        pg.Sql.Nullable.{ Nullable },
        VideoRental,
        # Unused but required because of: https://github.com/roc-lang/roc/issues/5477
        pf.Tcp,
        pg.Cmd,
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

selectAddress : _ -> Sql.Selection Address err
selectAddress = \table ->
    into {
        line1: <- table.address |> column,
        district: <- table.district |> column,
        postalCode: <- table.postalCode |> column,
    }

main : Task {} I32
main =
    args <- Arg.list |> await

    customerIdArg =
        args
        |> List.get 1
        |> Result.try Str.toI32

    when customerIdArg is
        Ok customerId ->
            customerId
            |> printCustomer
            |> handlePgTask

        Err _ ->
            _ <- Stdout.line "Usage: rental customer-id" |> Task.await
            Task.err 1

handlePgTask : Task {} [TcpConnectErr _, TcpPerformErr _] -> Task {} I32
handlePgTask = \task ->
    result <- Task.attempt task

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
            Task.err 3

printJson = \value ->
    Encode.toBytes value Core.json
    |> Str.fromUtf8
    |> Result.withDefault ""
    |> Stdout.line

logSql = \query ->
    sqlStr = (Sql.compileQuery query).sql
    Stdout.line "SQL: \(sqlStr)\n"
