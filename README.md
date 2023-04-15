# roc-pg

Interface with PostgreSQL databases from pure Roc.

## Examples

Connecting and performing a query

```haskell
task : Task (List { name: Str, price: U32 }) _
task =
    client <- Pg.Client.withConnect {
              host: "localhost",
              port: 5432,
              user: "postgres",
              database: "postgres",
              auth: Password "password"
          }

    Pg.Cmd.new "select name, price from products"
    |> Pg.Cmd.expectN
        (Pg.Result.succeed (\name -> \price -> { name, price })
            |> Pg.Result.with (Pg.Result.str "name")
            |> Pg.Result.with (Pg.Result.u32 "price")
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
|> Pg.Cmd.expect1
    (Pg.Result.succeed (\name -> \price -> { name, price })
        |> Pg.Result.with (Pg.Result.str "name")
        |> Pg.Result.with (Pg.Result.u32 "price")
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
Pg.Batch.succeed (\email -> \products -> { email, products })
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
      |> Pg.Cmd.expectN
          (Pg.Result.succeed (\name -> \price -> { name, price })
              |> Pg.Result.with (Pg.Result.str "name")
              |> Pg.Result.with (Pg.Result.u32 "price")
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
    Pg.Cmd.new "update products set price = $1 where id = $2"
    |> Pg.Cmd.bind [ Pg.Cmd.u32 product.newPrice, Pg.Cmd.u32 product.id ]

productsToUpdate
|> List.map updateCmd
|> Pg.Batch.sequence
|> Pg.Client.batch client
```

Note: `roc-pg` automatically reuses statements in a batch by only parsing (and describing) once per unique SQL string. This also works with applicative batches.

</details>

## Status

I'd like this to become a stable PostgreSQL interface for Roc eventually, but not unlike Roc itself, this project is a work in progress.

### Features

- [x] Connection handling
- [x] Parameterized queries
- [x] Decoding results
- [x] Decoding errors
- [ ] [`Decode`](https://www.roc-lang.org/builtins/Decode) ability implementation
- [ ] Authentication methods
  - [x] Cleartext password
  - [ ] MD5 password \*
  - [ ] SASL / SCRAM-SHA-256 \*
- [x] Prepared statements
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
- [ ] Platform independence

\* Requires new platform primitives

## Resources

- [PostgreSQL Protocol Flow](https://www.postgresql.org/docs/current/protocol-flow.html)
- [PostgreSQL Message Formats](https://www.postgresql.org/docs/current/protocol-message-formats.html)
