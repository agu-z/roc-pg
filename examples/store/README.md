# roc-pg store example

This example is a simple REST API for a store built using the [basic-webserver](https://github.com/roc-lang/basic-webserver) platform and roc-pg's query builder.

## Setup

To run it, you first need a PostgreSQL server running and database called `roc_pg_example`:

```shell
# Create a new database
$ createdb roc_pg_example

# Restore dump
$ psql -U postgres -d roc_pg_example < example/store/db.sql
```

Now you can generate the schema module from our new database:

```shell
# Build the CLI
$ roc build sql-cli/src/roc-sql.roc

# Generate schema for query builder
$ ./sql-cli/src/roc-sql -h localhost -p 5432 -U postgres -d roc_pg_example --schema public > examples/store/Public.roc
```

You can tweak the connection parameters at [example/store/server.roc](./server.roc).

## Running

Run the server:

``` shell
$ roc dev examples/store/server.roc
```

...and try hitting some of the routes!

- http://localhost:8000/products
- http://localhost:8000/products/1
- http://localhost:8000/orders
- http://localhost:8000/orders/1/products

See the output of the server for the generated SQL:

```
GET /products
SQL: select p.id, p.name from public.products as p


GET /orders/1/products
SQL: select p.id, p.name from public.products as p join public.order_products as op on op.product_id = p.id where op.order_id = $1
$1 = 1
```