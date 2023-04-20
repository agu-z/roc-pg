interface Sql exposes [] imports []

Sql : List Part

Part : [
    Param {},
    Raw Str,
]

Compiled : {
    sql : Str,
    params : List {},
}

compileSql : Sql -> Compiled
compileSql = \sql ->
    sql
    |> List.walk
        {
            params: List.withCapacity 8,
            sql: Str.withCapacity (List.len sql * 12),
        }
        addPart

addPart : Compiled, Part -> Compiled
addPart = \{ params, sql }, part ->
    when part is
        Param param ->
            newParams = params |> List.append param
            binding = Num.toStr (List.len newParams)

            { sql: "\(sql)$\(binding)", params: newParams }

        Raw raw ->
            { sql: Str.concat sql raw, params }

# Table

Table a : {
    schema : Str,
    name : Str,
    fields : Str -> a,
}

tableToSql : Table * -> Sql
tableToSql = \table ->
    [Raw "\(table.schema).\(table.name) as \(table.name)"]

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

compile : Query * -> Compiled
compile = \query ->
    selectSql = query.clauses.select.sql
    tableSql = tableToSql query.from

    [Raw "select "]
    |> List.reserve 16
    |> List.concat selectSql
    |> List.append (Raw " from ")
    |> List.concat tableSql
    |> addClause "where" query.clauses.where .sql
    |> compileSql

addClause = \sql, name, clause, toSqlFn ->
    when clause is
        Some value ->
            sql
            |> List.append (Raw " \(name) ")
            |> List.concat (toSqlFn value)

        None ->
            sql

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
    sql : Sql,
}

untyped : Expr * -> Expr {}
untyped = \{ sql } -> { sql, type: {} }

identifier : Str, Str, type -> Expr type
identifier = \table, column, type -> {
    type,
    sql: [Raw "\(table).\(column)"],
}

u8 : U8 -> Expr [Int]
u8 = \_ -> {
    type: Int,
    sql: [Param {}],
}

gt : Expr [Int], Expr [Int] -> Expr [Bool]
gt = cmp ">"

cmp : Str -> (Expr *, Expr * -> Expr [Bool])
cmp = \operator -> \a, b -> {
        type: Bool,
        sql: a.sql
        |> List.append (Raw " \(operator) ")
        |> List.concat b.sql,
    }

# Tests

usersTable = {
    schema: "public",
    name: "users",
    fields: \alias -> {
        name: identifier alias "name" Text,
        active: identifier alias "active" Bool,
        age: identifier alias "age" Int,
    },
}

testSimple =
    users <- from usersTable
    select users.name

expect
    compile testSimple
    == {
        sql: "select users.name from public.users as users",
        params: [],
    }

testWhere =
    users <- from usersTable

    select users.name
    |> where users.active

expect
    compile testWhere
    == {
        sql: "select users.name from public.users as users where users.active",
        params: [],
    }

testExpr =
    users <- from usersTable

    select users.name
    |> where (users.age |> gt (u8 18))

expect
    compile testExpr
    == {
        sql: "select users.name from public.users as users where users.age > $1",
        params: [{}],
    }
