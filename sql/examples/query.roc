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
        sql.Sql.{ from, select },
        sql.Sql.Decode,
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

    productsTable = {
        schema: "public",
        name: "products",
        fields: \alias -> {
            name: Sql.identifier alias "name" Sql.Decode.text,
            discount: Sql.identifier alias "discount" Sql.Decode.bool,
        },
    }

    result <-
        Sql.all
            (
                products <- from productsTable

                select
                    (
                        Sql.succeed (\name -> \discount -> { name, discount })
                        |> Sql.with products.name
                        |> Sql.with products.discount
                    )
            )
        |> Pg.Client.command client
        |> await

    products =
        result
        |> List.map \product ->
            sale =
                if product.discount then
                    " (SALE)"
                else
                    ""

            "- \(product.name)\(sale)"
        |> Str.joinWith "\n"

    Stdout.line "Products:\n\n\(products)"

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
