# roc-pg

Interface with PostgreSQL databases from Roc.

This package implements a PostgreSQL client on pure Roc that depends only on a TCP effect from the platform.

It exposes a simple API that allows you to run SQL commands as strings and a query builder that helps you write composable type-safe queries against your schema.

## Status

I'd like this to become a stable PostgreSQL interface for Roc, but this project is currently a work in progress.

You can already use this to build useful applications. However, until we have a platform with TLS support, you should stick to experiments or simple apps where you run the database in the same machine.

### Query Builder

For a while, I explored creating a type-safe query builder on top of roc-pg. I gave some cool demos, and the core idea seemed promising. Unfortunately, I didn't get to build enough of it to be helpful in a real application, and nowadays, I don't have the time it'd require to make it so.
You can see an example of what that looked like [here](https://github.com/agu-z/roc-pg/blob/6c17c7b438bdb349cebaad1c8e100045ff3f6aeb/examples/store/server.roc).

## Examples

Connecting and performing a query

```haskell
products! : {} => Result (List { name : Str, price : Dec }) _
products! = |_|
    client = Pg.Client.connect!(
        {
            host: "localhost",
            port: 5432,
            user: "postgres",
            auth: None,
            database: "postgres",
        },
    )?

    Pg.Cmd.new("select name, price from products")
    |> Pg.Cmd.expect_n(
        { Pg.Result.combine <-
            name: Pg.Result.str("name"),
            price: Pg.Result.dec("price"),
        },
    )
    |> Pg.Client.command!(client)
```

<details>
<summary>
Parameterized queries
</summary>

```elm
Pg.Cmd.new("select name, price from products where id = $1")
|> Pg.Cmd.bind([ Pg.Cmd.u32(product_id) ])
|> Pg.Cmd.expect1(
    { Pg.Result.combine <-
        name: Pg.Result.str("name"),
        price: Pg.Result.dec("price"),
    },
)
|> Pg.Client.command!(client)?
```

</details>

<details>
<summary>
Prepared statements
</summary>

```elm
select_user =
    "select email from users where id = $1"
    |> Pg.Client.prepare!({ client, name: "select_user" })?

select_user
|> Pg.Cmd.bind([Pg.Cmd.u32(user_id)])
|> Pg.Cmd.expect1(Pg.Result.str("email"))
|> Pg.Client.command!(client)?
```

</details>

<details>
<summary>
Batch commands in a single roundtrip (applicative)
</summary>

```elm
Pg.Batch.succeed(|email| |products| { email, products })
|> Pg.Batch.with(
    (
        select_user
        |> Pg.Cmd.bind([Pg.Cmd.u32(user_id)])
        |> Pg.Cmd.expect1(Pg.Result.str("email"))
    ),
)
|> Pg.Batch.with(
    (
        Pg.Cmd.new(
            """
            select name, price from products
            inner join orders on orders.product_id = products.id
            where orders.id = $1
            """,
        )
        |> Pg.Cmd.bind([Pg.Cmd.u32(order_id)])
        |> Pg.Cmd.expect_n(
            { Pg.Result.combine <-
                name: Pg.Result.str("name"),
                price: Pg.Result.dec("price"),
            },
        )
    ),
)
|> Pg.Client.batch!(client)?
```

Note: `select_user` referes to prepared statement in the previous example

</details>

<details>
<summary>
Batch commands in a single roundtrip (list)
</summary>

```elm
update_cmd = |product|
    Pg.Cmd.new("update products set desc = $1 where id = $2")
    |> Pg.Cmd.bind([Pg.Cmd.str(product.desc), Pg.Cmd.u32(product.id)])

products_to_update
|> List.map(update_cmd)
|> Pg.Batch.sequence
|> Pg.Client.batch!(client)?
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
