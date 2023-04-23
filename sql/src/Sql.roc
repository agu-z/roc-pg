interface Sql
    exposes [
        from,
        select,
        compile,
        identifier,
    ]
    imports [
        Sql.Decode.{Decode},
    ]

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

Command a :=
    # Only select for now
    {
        from : Sql,
        clauses : SelectClauses a,
    }

Table table : {
    schema : Str,
    name : Str,
    fields : Str -> table,
}

from : Table table, (table -> Select a) -> Command a
from = \table, next ->
    alias = initial table.name
    usedAliases = Dict.withCapacity 4 |> Dict.insert alias 1

    (@Select toClauses) = next (table.fields alias)

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

compile : Command a -> Compiled
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

Select a := Dict Str U8 -> SelectClauses a

SelectClauses a : {
    joins : List Sql,
    select : Sql,
    decode : Decode a,
    where : Sql,
    limit : Sql,
}

join : Table table, (table -> Expr Bool), (table -> Select a) -> Select a
join = \table, on, next -> @Select \aliases -> joinHelp table on next aliases

joinHelp = \table, on, next, aliases ->
    { alias, newAliases } = joinAlias table.name aliases

    fields = table.fields alias
    (@Select toClauses) = next fields
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

select : Expr a -> Select a
select = \expr ->
    @Select \_ -> {
        joins: List.withCapacity 4,
        where: [],
        select: expr.sql,
        decode: expr.decode,
        limit: [],
    }

# We have to use _ here because of a type checker bug

where : Select _, Expr Bool -> Select _
where =
    clauses, expr <- updateClauses
    { clauses & where: List.prepend expr.sql (Raw " where ") }

limit : Select _, Nat -> Select _
limit =
    clauses, _ <- updateClauses
    { clauses & limit: [Raw " limit ", Param {}] }

updateClauses : (SelectClauses a, arg -> SelectClauses b) -> (Select a, arg -> Select b)
updateClauses = \fn ->
    \next, arg ->
        @Select \aliases ->
            (@Select toClauses) = next
            fn (toClauses aliases) arg

# Expr

Expr a : {
    sql : Sql,
    decode : Decode a,
}

identifier : Str, Str, Decode a -> Expr a
identifier = \table, column, decode -> {
    sql: [Raw "\(table).\(column)"],
    decode,
}

u8 : U8 -> Expr U8
u8 = \_ -> {
    sql: [Param {}],
    decode: Sql.Decode.decodeU8,
}

eq : Expr a, Expr a -> Expr Bool
eq = \a, b -> cmp a "=" b

gt : Expr (Num a), Expr (Num a) -> Expr Bool
gt = \a, b -> cmp a ">" b

cmp : Expr a, Str, Expr a -> Expr Bool
cmp = \a, op, b -> {
    sql: a.sql
    |> List.append (Raw " \(op) ")
    |> List.concat b.sql,
    decode: Sql.Decode.decodeBool,
}

#

# Selection a : {
#     columns: List Sql,
#     decode: Pg.Result.Decode a {}
# }

# Tests

usersTable = {
    schema: "public",
    name: "users",
    fields: \alias -> {
        name: identifier alias "name" Sql.Decode.decodeText,
        active: identifier alias "active" Sql.Decode.decodeBool,
        age: identifier alias "age" Sql.Decode.decodeU8,
        organizationId: identifier alias "organization_id" Sql.Decode.decodeU32,
    },
}

orgTable = {
    schema: "public",
    name: "organizations",
    fields: \alias -> {
        id: identifier alias "id" Sql.Decode.decodeU32,
        name: identifier alias "name" Sql.Decode.decodeText,
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
    out : Command Str
    out =

        users <- from usersTable

        select users.name
        |> where users.active

    compile out
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

