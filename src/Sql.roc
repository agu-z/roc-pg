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
    where : Option Expr,
    select : Expr,
}

select : Expr -> Clauses
select = \expr -> {
    where: None,
    select: expr,
}

where : Clauses, Expr -> Clauses
where = \clauses, expr ->
    { clauses & where: Some expr }

# Expr

Expr : [
    Identifier Str Str,
]

exprToSql : Expr -> Str
exprToSql = \expr ->
    when expr is
        Identifier t f ->
            "\(t).\(f)"

# Tests

usersTable = {
    schema: "public",
    name: "users",
    fields: \alias -> {
        name: Identifier alias "name",
        active: Identifier alias "active",
    },
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