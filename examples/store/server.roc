app "store-server"
    packages {
        pf: "https://github.com/roc-lang/basic-webserver/releases/download/0.1/dCL3KsovvV-8A5D_W_0X_abynkcRcoAngsgF0xtvQsk.tar.br",
        json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.5.0/jEPD_1ZLFiFrBeYKiKvHSisU-E3LZJeenfa9nvqJGeE.tar.br",
        pg: "../../src/main.roc",
    }
    imports [
        pf.Task.{ Task, await },
        pf.Stdout,
        pf.Http.{ Request, Response },
        pf.Url.{ Url },
        json.Core,
        pg.Pg.Client,
        pg.Pg.Result,
        pg.Pg.Cmd,
        pg.Sql.{ select, into, from, column, where, eq, join, on },
        Public,
        # Unused but required because of: https://github.com/roc-lang/roc/issues/5477
        pf.Tcp,
        pg.Cmd,
    ]
    provides [main] to pf

handleRequest : Request -> Task _ _
handleRequest = \req ->
    method = Http.methodToStr req.method
    {} <- Stdout.line "\n\(method) \(req.url)" |> Task.await

    url = Url.fromStr req.url

    when (req.method, urlSegments url) is
        (Get, [""]) ->
            {
                availableRoutes: [
                    "/products",
                    "/products/:id",
                    "/orders",
                    "/orders/:id/products",
                ],
            }
            |> okJson
            |> Task.ok

        (Get, ["products"]) ->
            query =
                products <- from Public.products

                into {
                    id: <- column products.id,
                    name: <- column products.name,
                }
                |> select

            Sql.queryAll query
            |> runDb
            |> Task.map okJson

        (Get, ["products", idStr]) ->
            id <- parseId idStr |> await

            query =
                products <- from Public.products

                into {
                    id: <- column products.id,
                    name: <- column products.name,
                }
                |> select
                |> where (products.id |> eq (Sql.i32 id))

            Sql.queryOne query
            |> runDb
            |> Task.map okJson

        (Get, ["orders"]) ->
            query =
                orders <- from Public.orders
                customers <- join Public.customers (on .id orders.customerId)

                customerObj = into {
                    id: <- column customers.id,
                    name: <- column customers.name,
                    email: <- column customers.email,
                }

                into {
                    id: <- column orders.id,
                    customer: <- Sql.with customerObj,
                }
                |> select

            Sql.queryAll query
            |> runDb
            |> Task.map okJson

        (Get, ["orders", idStr, "products"]) ->
            id <- parseId idStr |> await

            query =
                products <- from Public.products
                orderProducts <- join Public.orderProducts (on .productId products.id)

                into {
                    id: <- column products.id,
                    name: <- column products.name,
                }
                |> select
                |> where (orderProducts.orderId |> eq (Sql.i32 id))

            Sql.queryAll query
            |> runDb
            |> Task.map okJson

        _ ->
            Task.err RouteNotFound

main : Request -> Task Response []
main = \req ->
    result <- Task.attempt (handleRequest req)

    when result is
        Ok response ->
            Task.ok response

        Err RouteNotFound ->
            Task.ok {
                status: 404,
                headers: [],
                body: "Not found" |> Str.toUtf8,
            }

        Err (BadRequest err) ->
            Task.ok {
                status: 400,
                headers: [],
                body: err |> Str.toUtf8,
            }

        Err (TcpConnectErr _) ->
            Task.ok {
                status: 500,
                headers: [],
                body:
                """
                Failed to connect to PostgreSQL Server.

                Make sure it's running on localhost:5432, or tweak the connection params in examples/store/server.roc.
                """
                |> Str.toUtf8,
            }

        Err (TcpPerformErr (PgErr err)) ->
            if err.code == "3D000" then
                Task.ok {
                    status: 500,
                    headers: [],
                    body:
                    """
                    It looks like the `roc_pg_example` db hasn't been created.

                    See examples/store/README.md for instructions.
                    """
                    |> Str.toUtf8,
                }
            else
                errStr = Pg.Client.errorToStr err

                Task.ok {
                    status: 500,
                    headers: [],
                    body: errStr |> Str.toUtf8,
                }

        Err err ->
            dbg
                err

            Task.ok {
                status: 500,
                headers: [],
                body: "Something went wrong" |> Str.toUtf8,
            }

runDb : Pg.Cmd.Cmd a err -> Task a _
runDb = \cmd ->
    # Currently creating a connection per request.
    # We will support pooling in the future, but we need to come up with some new platform primitives.
    client <- Pg.Client.withConnect {
            host: "localhost",
            port: 5432,
            user: "postgres",
            database: "roc_pg_example",
        }

    _ <- cmd
        |> Pg.Cmd.inspect
        |> Stdout.line
        |> await

    Pg.Client.command cmd client

okJson : a -> Response where a implements Encoding
okJson = \data -> {
    status: 200,
    headers: [
        {
            name: "Content-Type",
            value: "application/json" |> Str.toUtf8,
        },
    ],
    body: Encode.toBytes data Core.json,
}

urlSegments : Url -> List Str
urlSegments = \url ->
    url
    |> Url.path
    |> Str.split "/"
    |> List.dropFirst 1

parseId : Str -> Task I32 [BadRequest Str]
parseId = \idStr ->
    Str.toI32 idStr
    |> Result.mapErr \InvalidNumStr -> BadRequest "Invalid id: \(idStr)"
    |> Task.fromResult
