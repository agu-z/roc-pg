app "query"
    packages {
        pf: "https://github.com/agu-z/roc-basic-cli/releases/download/0.5.0/S8r4wytSGYKi-iMrjaRZxv2Hope_CX7dF6rMdciYja8.tar.gz",
        sql: "../src/main.roc",
        pg: "../../src/main.roc",
    }
    imports [
        pf.Task.{ Task, await },
        pf.Process,
        pf.Stdout,
        pf.Stderr,
        pg.Pg.Client,
        pg.Pg.Cmd,
        pg.Pg.Result,
        sql.Sql.{ from, select, where, join, on, into, eq, str, column, with, limit, orderBy },
        sql.Sql.Types.{ Nullable },
        Public,
    ]
    provides [main] to pf

customerQuery =
    customers <- from Public.customer
    addr <- join Public.address (on .addressId customers.addressId)
    cities <- join Public.city (on .cityId addr.cityId)
    country <- join Public.country (on .countryId cities.countryId)

    fullName =
        customers.firstName
        |> Sql.concat (str " ")
        |> Sql.concat customers.lastName

    into {
        name: <- column fullName,
        address: <- with (selectAddress addr),
    }
    |> select
    |> orderBy [Sql.asc fullName, Sql.desc customers.customerId]
    |> where (country.country |> eq (str "United States"))
    |> limit 10

Address : {
    addr : Str,
    phone : Str,
    district : Str,
    postalCode : Nullable Str,
}

selectAddress : _ -> Sql.Selection Address
selectAddress = \table ->
    into {
        addr: <- table.address |> column,
        phone: <- table.phone |> column,
        district: <- table.district |> column,
        postalCode: <- table.postalCode |> column,
    }

addressToStr = \{ addr, district, phone, postalCode } ->
    postalCodeStr =
        when postalCode is
            Present code ->
                code

            Null ->
                ""
    "\(addr), \(district) \(postalCodeStr). Phone: \(phone)"

task =
    client <- Pg.Client.withConnect {
            host: "localhost",
            port: 5432,
            user: "postgres",
            database: "pagalia",
        }

    customers <-
        Sql.all customerQuery
        |> Pg.Client.command client
        |> await

    sqlStr = (Sql.compile customerQuery).sql

    _ <- Stdout.line "\(sqlStr)\n" |> await

    customers
    |> List.map \c ->
        addrStr = addressToStr c.address
        "\(c.name) | \(addrStr)"
    |> Str.joinWith "\n"
    |> Stdout.line

main : Task {} []
main =
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
