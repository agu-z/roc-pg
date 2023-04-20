interface Sql exposes [] imports []

# Table

Table a : {
    schema : Str,
    name : Str,
    fields : Str -> a,
}

tableToSql : Table * -> Str
tableToSql = \table ->
    "\(table.schema).\(table.name) as \(table.name)"

# Query

Query a : {
    from : Table a,
    clauses : Clauses,
}

from : Table a, (a -> Clauses) -> Query a
from = \table, callback -> {
    from: table,
    clauses: callback (table.fields table.name),
}

toSql : Query * -> Str
toSql = \query ->
    selectSql = exprToSql query.clauses.select
    tableSql = tableToSql query.from

    "select \(selectSql) from \(tableSql)"
    |> addCluse "where" query.clauses.where exprToSql

addCluse = \str, name, clause, toSqlFn ->
    when clause is
        Some value ->
            clauseSql = toSqlFn value
            "\(str) \(name) \(clauseSql)"

        None ->
            str

# Clauses

Option a : [None, Some a]

Clauses : {
    where : Option (Expr [Bool]),
    select : Expr {},
}

select : Expr * -> Clauses
select = \expr -> {
    where: None,
    select: untyped expr,
}

where : Clauses, Expr [Bool] -> Clauses
where = \clauses, expr ->
    { clauses & where: Some expr }

# Expr

Expr type : {
    type : type,
    sql : SqlExpr,
}

SqlExpr : [
    Identifier Str Str,
]

untyped = \{ sql } -> { sql, type: {} }

exprToSql : Expr * -> Str
exprToSql = \{ sql } ->
    when sql is
        Identifier t f ->
            "\(t).\(f)"

# Tests

usersTable = {
    schema: "public",
    name: "users",
    fields: \alias -> {
        name: typedId alias "name" Text,
        active: typedId alias "active" Bool,
    },
}

typedId : Str, Str, type -> Expr type
typedId = \table, column, type -> {
    type,
    sql: Identifier table column,
}

simpleSelect =
    users <- from usersTable
    select users.name

expect toSql simpleSelect == "select users.name from public.users as users"

selectWithWhere =
    users <- from usersTable

    select users.name
    |> where users.active

expect toSql selectWithWhere == "select users.name from public.users as users where users.active"
