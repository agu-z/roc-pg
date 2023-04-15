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
            |> Pg.Result.apply (Pg.Result.str "name")
            |> Pg.Result.apply (Pg.Result.u32 "price")
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
        |> Pg.Result.apply (Pg.Result.str "name")
        |> Pg.Result.apply (Pg.Result.u32 "price")
    )
|> Pg.Client.command client
```

</details>

<details>
<summary>
Prepared statements
</summary>

```elm
selectProduct <-
    "select name from products where id = $1"
    |> Pg.Client.prepare { client, name: "selectProduct" }
    |> await

selectProduct
|> Pg.Cmd.bind [ Pg.Cmd.u32 productId ]
|> Pg.Cmd.expect1 (Pg.Result.str "name")
|> Pg.Client.command client

```

</details>

<details>
<summary>
Batch commands in a single roundtrip (applicative)
</summary>

```elm
Pg.Batch.succeed (\product -> \user -> { product, user })
|> Pg.Batch.with
    (
      selectProduct
      |> Pg.Cmd.bind [ Pg.Cmd.u32 productId ]
      |> Pg.Cmd.expect1 (Pg.Result.str "name")
    )
|> Pg.Batch.with
    (
      Pg.Cmd.new "select name, phone from user where id = $1"
      |> Pg.Cmd.bind [ Pg.Cmd.u32 userId ]
      |> Pg.Cmd.expect1 (
          Pg.Result.succeed (\name -> \phone -> { name, phone })
          |> Pg.Result.apply (Pg.Result.str "name")
          |> Pg.Result.apply (Pg.Result.str "phone")
      )
    )
|> Pg.Client.batch client
```

`selectProduct` refers to the prepared statement in the previous example

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
