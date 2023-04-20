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

# Query

Query : {
    from : Sql,
    clauses : Clauses,
}

from : Table a, (a -> Clauses) -> Query
from = \table, callback -> 
    alias = findAlias table.name

    {
        from: [Raw " from \(table.schema).\(table.name) as \(alias)"],
        clauses: callback (table.fields alias),
    }

findAlias = \name ->
    # TODO: Conflict check
    
    when name |> Str.graphemes |> List.first is 
        Ok initial ->
            initial

        Err ListWasEmpty ->
            ""
    

compile : Query -> Compiled
compile = \query ->
    selectSql = query.clauses.select.sql

    [Raw "select "]
    |> List.reserve 16
    |> List.concat selectSql
    |> List.concat query.from
    |> List.concat (List.join query.clauses.joins)
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
    joins : List Sql,
    where : Option (Expr [Bool]),
    select : Expr {},
}

select : Expr * -> Clauses
select = \expr -> {
    joins: List.withCapacity 4,
    where: None,
    select: untyped expr,
}

where : Clauses, Expr [Bool] -> Clauses
where = \clauses, expr ->
    { clauses & where: Some expr }

join : Table a, (a -> Expr [Bool]), (a -> Clauses) -> Clauses
join = \table, on, callback ->
    alias = findAlias table.name
    fields = table.fields alias
    clauses = callback fields
    joinSql =
        on fields
        |> .sql
        |> List.prepend (Raw " join \(table.schema).\(table.name) as \(alias) on ")

    { clauses & joins: clauses.joins |> List.append joinSql }

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

u32 : U32 -> Expr [Int]
u32 = \_ -> {
    type: Int,
    sql: [Param {}],
}

eq : Expr a, Expr a -> Expr [Bool]
eq = \a, b -> cmp a "=" b

gt : Expr [Int], Expr [Int] -> Expr [Bool]
gt = \a, b -> cmp a ">" b

cmp : Expr a, Str, Expr a -> Expr [Bool]
cmp = \a, op, b -> {
    type: Bool,
    sql: a.sql
    |> List.append (Raw " \(op) ")
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
        organizationId: identifier alias "organization_id" Int,
    },
}

orgTable = {
    schema: "public",
    name: "organizations",
    fields: \alias -> {
        id: identifier alias "id" Int,
        name: identifier alias "name" Text,
    },
}

expect
    query =
        users <- from usersTable
        select users.name

    compile query
    == {
        sql: "select u.name from public.users as u",
        params: [],
    }

expect
    query =
        users <- from usersTable

        select users.name
        |> where users.active

    compile query
    == {
        sql: "select u.name from public.users as u where u.active",
        params: [],
    }

expect
    query =
        users <- from usersTable

        select users.name
        |> where (users.age |> gt (u8 18))

    compile query
    == {
        sql: "select u.name from public.users as u where u.age > $1",
        params: [{}],
    }

expect
    query =
        users <- from usersTable
        org <- join orgTable \o -> o.id |> eq users.organizationId

        select org.name
        |> where (org.id |> eq (u32 1))

    compile query
    == {
        sql:
        """
        select o.name
        from public.users as u
        join public.organizations as o on o.id = u.organization_id
        where o.id = $1
        """
        |> Str.split "\n"
        |> Str.joinWith " ",
        params: [{}],
    }

