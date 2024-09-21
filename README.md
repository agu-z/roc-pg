# roc-pg

⚠️ BROKEN: This package runs into a bug in the recent versions of the compiler and platforms. I'm looking into fixing those, but it might take a while. If you want to use this now, reach out to me in [Roc's Zulip](https://roc.zulipchat.com/#narrow/dm/489294-Agus-Zubiaga), and I can help you find a combination of versions that works.

--

Interface with PostgreSQL databases from Roc.

This package implements a PostgreSQL client on pure Roc that depends only on a TCP effect from the platform. 

It exposes a simple API that allows you to run SQL commands as strings and a query builder that helps you write composable type-safe queries against your schema.

## Status

I'd like this to become a stable PostgreSQL interface for Roc, but this project is currently a work in progress.

You can already use this to build useful applications. However, until we have a platform with TLS support, you should stick to experiments or simple apps where you run the database in the same machine.

### Query Builder

The query builder is one of the most exciting features of this package, but more experimental than the lower-level API.

You can currently generate a Roc module from your schema that you can use through the functions exposed under [`Sql`](./src/Sql.roc) to compose type-safe `SELECT` statements.

The plan is to support all the other SQL commands, but that's coming later. In the meantime, you can perform those by creating a raw SQL command with `Pg.Cmd.new`.

**See an [example](./examples/store) of a simple HTTP API built with the query builder!**

## Examples

Connecting and performing a query

```haskell
task : Task (List { name: Str, price: Dec }) _
task =
    client <- Pg.Client.withConnect {
              host: "localhost",
              port: 5432,
              user: "postgres",
              database: "postgres",
              auth: Password "password"
          }

    Pg.Cmd.new "select name, price from products"
    |> Pg.Cmd.expectN (
        Pg.Result.succeed { 
            name: <- Pg.Result.str "name" |> Pg.Result.apply, 
            price: <- Pg.Result.dec "price" |> Pg.Result.apply
        }
    ) 
    |> Pg.Client.command client
```

<details>
<summary>
Parameterized queries
</summary>

```elm
Pg.Cmd.new "select name, price from products where id = $1"
|> Pg.Cmd.bind [ Pg.Cmd.u32 productId ]
|> Pg.Cmd.expect1 (
    Pg.Result.succeed { 
        name: <- Pg.Result.str "name" |> Pg.Result.apply, 
        price: <- Pg.Result.dec "price" |> Pg.Result.apply
    }
) 
|> Pg.Client.command client
```

</details>

<details>
<summary>
Prepared statements
</summary>

```elm
selectUser <-
    "select email from users where id = $1"
    |> Pg.Client.prepare { client, name: "selectUser" }
    |> await

selectUser
|> Pg.Cmd.bind [ Pg.Cmd.u32 userId ]
|> Pg.Cmd.expect1 (Pg.Result.str "email")
|> Pg.Client.command client

```

</details>

<details>
<summary>
Batch commands in a single roundtrip (applicative)
</summary>

```elm
Pg.Batch.succeed \email -> \products -> { email, products }
|> Pg.Batch.with
    (
        selectUser
        |> Pg.Cmd.bind [ Pg.Cmd.u32 userId ]
        |> Pg.Cmd.expect1 (Pg.Result.str "email")
    )
|> Pg.Batch.with
    (
        Pg.Cmd.new
            """
            select name, price from products
            inner join orders on orders.product_id = products.id
            where orders.id = $1
            """
        |> Pg.Cmd.bind [ Pg.Cmd.u32 orderId ]
        |> Pg.Cmd.expectN (
            Pg.Result.succeed { 
                name: <- Pg.Result.str "name" |> Pg.Result.apply, 
                price: <- Pg.Result.dec "price" |> Pg.Result.apply
            }
        ) 
    )
|> Pg.Client.batch client
```

Note: `selectUser` referes to prepared statement in the previous example

</details>

<details>
<summary>
Batch commands in a single roundtrip (list)
</summary>

```elm
updateCmd = \product ->
    Pg.Cmd.new "update products set desc = $1 where id = $2"
    |> Pg.Cmd.bind [ Pg.Cmd.str product.desc, Pg.Cmd.u32 product.id ]

productsToUpdate
|> List.map updateCmd
|> Pg.Batch.sequence
|> Pg.Client.batch client
```

Note: `roc-pg` automatically reuses statements in a batch by only parsing (and describing) once per unique SQL string. This also works with applicative batches.

</details>

## Documentation

The API has been in a state of flux until recently. I have now started working on documentation, but the [examples](./examples) is probably the best we have for now.

Feel free to DM me at [Roc's Zulip](https://roc.zulipchat.com/#narrow/dm/489294-Agus-Zubiaga), though!


## Features

- [x] Connection handling
- [x] Parameterized queries
- [x] Decoding results
- [x] Decoding errors
- [ ] Authentication methods
  - [x] Cleartext password
  - [ ] MD5 password \*
  - [ ] SASL / SCRAM-SHA-256 \*
- [x] Prepared statements
- [ ] Close prepared statements
- [x] Pipelining
  - [x] Applicative batches
  - [x] Sequence list of commands expecting same type
  - [x] 🚀 Parse and Describe once per unique SQL string
- [ ] Bulk copying
- [ ] Cursors
- [ ] SSL \*
- [ ] Connection pooling \*
- [ ] Notifications (listen/notify)
- [ ] Notices

\* Requires new platform primitives

This list does not include features of the query builder as I'm still figuring out those.


## Resources

- [PostgreSQL Protocol Flow](https://www.postgresql.org/docs/current/protocol-flow.html)
- [PostgreSQL Message Formats](https://www.postgresql.org/docs/current/protocol-message-formats.html)
