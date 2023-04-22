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

Command := 
    # Only select for now
    {
        from : Sql,
        clauses : SelectClauses,
    }

Table a : {
    schema : Str,
    name : Str,
    fields : Str -> a,
}

from : Table a, (a -> Select) -> Command
from = \table, next ->
    alias = initial table.name
    usedAliases = Dict.withCapacity 4 |> Dict.insert alias 1

    @Select toClauses = next (table.fields alias)

    @Command {
        from: [Raw " from \(table.schema).\(table.name) as \(alias)"],
        clauses: toClauses usedAliases,
    }

initial = \name ->
    when name |> Str.graphemes |> List.first is
        Ok char ->
            char

        Err ListWasEmpty ->
            ""

compile : Command -> Compiled
compile = \@Command query ->
    [Raw "select "]
    |> List.reserve 16
    |> List.concat query.clauses.select
    |> List.concat query.from
    |> List.concat (List.join query.clauses.joins)
    |> List.concat query.clauses.where
    |> List.concat query.clauses.limit
    |> compileSql

# Clauses

Select := Dict Str U8 -> SelectClauses

SelectClauses : {
    joins : List Sql,
    select : Sql,
    where : Sql,
    limit : Sql,
}

join : Table a, (a -> Expr [Bool]), (a -> Select) -> Select
join = \table, on, next -> @Select \aliases -> joinHelp table on next aliases

joinHelp = \table, on, next, aliases ->
    { alias, newAliases } = joinAlias table.name aliases

    fields = table.fields alias
    @Select toClauses = next fields
    clauses = toClauses newAliases
    joinSql =
        on fields
        |> .sql
        |> List.prepend (Raw " join \(table.schema).\(table.name) as \(alias) on ")

    { clauses & joins: clauses.joins |> List.prepend joinSql }


joinAlias : Str, Dict Str U8 -> { alias : Str, newAliases : Dict Str U8 }
joinAlias = \name, aliases ->
    alias = initial name

    when Dict.get aliases alias is
        Ok count ->
            strCount = Num.toStr count
            {
                alias: "\(alias)_\(strCount)",
                newAliases: Dict.insert aliases alias (count + 1),
            }

        Err KeyNotFound ->
            {
                alias,
                newAliases: Dict.insert aliases alias 1,
            }

select : Expr * -> Select
select = \expr ->
    @Select \_ ->  {
        joins: List.withCapacity 4,
        where: [],
        select: expr.sql,
        limit: [],
    }

where : Select, Expr [Bool] -> Select
where =
    clauses, expr <- updateClauses
    { clauses & where: List.prepend expr.sql (Raw " where ") }

limit : Select, Nat -> Select
limit =
    clauses, _ <- updateClauses
    { clauses & limit: [Raw " limit ", Param {}] }

updateClauses = \fn -> 
    \next, arg -> 
        @Select \aliases -> 
            @Select toClauses = next
            fn (toClauses aliases) arg

# Expr

Expr type : {
    type : type,
    sql : Sql,
}

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
    out = compile
        (
            users <- from usersTable
            select users.name
        )

    out
    == {
        sql: "select u.name from public.users as u",
        params: [],
    }

expect
    out = compile
        (
            users <- from usersTable

            select users.name
            |> where users.active
        )

    out
    == {
        sql: "select u.name from public.users as u where u.active",
        params: [],
    }

expect
    out = compile
        (
            users <- from usersTable

            select users.name
            |> where (users.age |> gt (u8 18))
        )

    out
    == {
        sql: "select u.name from public.users as u where u.age > $1",
        params: [{}],
    }

expect
    out = compile
        (
            users <- from usersTable
            org <- join orgTable \o -> o.id |> eq users.organizationId
            org2 <- join orgTable \o -> o.id |> eq users.organizationId

            select org.name
            |> where (org.id |> eq org2.id)
            |> limit 10
        )

    out
    == {
        sql:
        """
        select o.name
        from public.users as u
        join public.organizations as o on o.id = u.organization_id
        join public.organizations as o_1 on o_1.id = u.organization_id
        where o.id = o_1.id
        limit $1
        """
        |> Str.split "\n"
        |> Str.joinWith " ",
        params: [{}],
    }

