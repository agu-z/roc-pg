interface Sql
    exposes [
        all,
        compile,
        from,
        select,
        identifier,
        where,
        join,
        limit,
        orderBy,
        eq,
        u8,
        str,
        concat,
        gt,
        and,
        not,
        Selection,
        column,
        with,
        into,
        just,
        map,
        rowArray,
    ]
    imports [
        pg.Pg.Cmd.{ Binding },
        pg.Pg.Result,
        Sql.Decode.{ Decode },
    ]

Query a :=
    # Only select for now
    {
        from : Sql,
        clauses : SelectClauses a,
    }

Table table : {
    schema : Str,
    name : Str,
    columns : Str -> table,
}

from : Table table, (table -> Select a) -> Query a
from = \table, next ->
    alias = initial table.name
    usedAliases = Dict.withCapacity 4 |> Dict.insert alias 1

    (@Select toClauses) = next (table.columns alias)

    @Query {
        from: [Raw " from \(table.schema).\(table.name) as \(alias)"],
        clauses: toClauses usedAliases,
    }

initial = \name ->
    when name |> Str.graphemes |> List.first is
        Ok char ->
            char

        Err ListWasEmpty ->
            ""

all : Query a -> Pg.Cmd.Cmd _ _
all = \@Query query ->
    { sql, params } = compile (@Query query)

    Pg.Cmd.new sql
    |> Pg.Cmd.bind params
    |> Pg.Cmd.withCustomDecode \result ->
        (@Selection selection) = query.clauses.selection

        cells <- Pg.Result.rows result |> List.mapTry

        when selection.decode cells is
            Ok value ->
                Ok value

            Err err ->
                Err (DecodeErr err)

querySql = \@Query query, columnWrapper ->
    (@Selection { columns }) = query.clauses.selection

    columnsSql =
        when columnWrapper is
            Bare ->
                commaJoin columns

            RowArray ->
                [Raw "array_agg(row("]
                |> List.concat (commaJoin columns)
                |> List.append (Raw "))")

    [Raw "select "]
    |> List.reserve 16
    |> List.concat columnsSql
    |> List.concat query.from
    |> List.concat (List.join query.clauses.joins)
    |> List.concat query.clauses.where
    |> List.concat query.clauses.orderBy
    |> List.concat query.clauses.limit

compile = \query ->
    querySql query Bare
    |> compileSql

# Clauses

Select a := Dict Str U8 -> SelectClauses a

SelectClauses a : {
    joins : List Sql,
    selection : Selection a,
    where : Sql,
    orderBy : Sql,
    limit : Sql,
}

join : Table table, (table -> Expr Bool), (table -> Select a) -> Select a
join = \table, on, next -> @Select \aliases -> joinHelp table on next aliases

joinHelp = \table, on, next, aliases ->
    { alias, newAliases } = joinAlias table.name aliases

    columns = table.columns alias
    (@Select toClauses) = next columns
    clauses = toClauses newAliases

    (@Expr { sql: onSql }) = on columns

    joinSql =
        List.prepend onSql (Raw " join \(table.schema).\(table.name) as \(alias) on ")

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

select : Selection a -> Select a
select = \selection ->
    @Select \_ -> {
        joins: List.withCapacity 4,
        selection: selection,
        where: [],
        limit: [],
        orderBy: [],
    }

# We have to use _ here because of a type checker bug

where : Select a, Expr Bool -> Select a
where =
    clauses, (@Expr expr) <- updateClauses
    { clauses & where: List.prepend expr.sql (Raw " where ") }

limit : Select a, Nat -> Select a
limit =
    clauses, max <- updateClauses
    { clauses & limit: [Raw " limit ", Param (Pg.Cmd.nat max)] }

orderBy : Select a, List [Asc (Expr *), Desc (Expr *)] -> Select a
orderBy =
    clauses, order <- updateClauses

    sql =
        order
        |> List.map \dir ->
            when dir is
                Asc (@Expr expr) ->
                    expr.sql |> List.append (Raw " asc")

                Desc (@Expr expr) ->
                    expr.sql |> List.append (Raw " desc")
        |> commaJoin
        |> List.prepend (Raw " order by ")

    { clauses & orderBy: sql }

updateClauses : (SelectClauses a, arg -> SelectClauses b) -> (Select a, arg -> Select b)
updateClauses = \fn ->
    \next, arg ->
        @Select \aliases ->
            (@Select toClauses) = next
            fn (toClauses aliases) arg

# Selection

Selection a := {
    columns : List Sql,
    decode : List (List U8) -> Result a Sql.Decode.DecodeErr,
}

into : a -> Selection a
into = \value -> @Selection {
        columns: [],
        decode: \_ -> Ok value,
    }

column : Selection (a -> b), Expr a -> Selection b
column = \@Selection sel, @Expr expr ->
    index = List.len sel.columns

    decode = \cells ->
        fn <- sel.decode cells |> Result.try
        value <- cells
            |> List.get index
            |> Result.mapErr \OutOfBounds -> MissingColumn index
            |> Result.try
        a <- value
            |> Sql.Decode.decode expr.decode
            |> Result.map
        fn a

    @Selection {
        columns: sel.columns |> List.append expr.sql,
        decode,
    }

with : Selection (a -> b), Selection a -> Selection b
with = \@Selection fnSel, @Selection aSel ->
    count = List.len fnSel.columns

    decode = \cells ->
        fn <- fnSel.decode cells |> Result.try
        a <- cells
            |> List.drop count
            |> aSel.decode
            |> Result.map
        fn a

    @Selection {
        columns: fnSel.columns |> List.concat aSel.columns,
        decode,
    }

rowArray : Selection (List a -> b), Query a -> Selection b
rowArray = \sel, @Query query ->
    sql = querySql (@Query query) RowArray

    wrapped =
        [Raw "("]
        |> List.concat sql
        |> List.append (Raw ")")

    (@Selection selection) = query.clauses.selection

    decode = Sql.Decode.rowArray \items ->
        List.mapTry items selection.decode

    expr = @Expr { sql: wrapped, decode }

    column sel expr

map : Selection a, (a -> b) -> Selection b
map = \@Selection sel, fn ->
    @Selection {
        columns: sel.columns,
        decode: \cells -> cells
            |> sel.decode
            |> Result.map fn,
    }

just : Expr a -> Selection a
just = \@Expr expr ->
    @Selection {
        columns: [expr.sql],
        decode: \cells ->
            value <- cells
                |> List.first
                |> Result.mapErr \ListWasEmpty -> MissingColumn 0
                |> Result.try

            Sql.Decode.decode value expr.decode,
    }

# Expr

Expr a := {
    sql : Sql,
    decode : Decode a,
}

identifier : Str, Str, Decode a -> Expr a
identifier = \table, col, decode -> @Expr {
        sql: [Raw "\(table).\(col)"],
        decode,
    }

u8 : U8 -> Expr I16
u8 = \value -> @Expr {
        sql: [Param (Pg.Cmd.u8 value)],
        decode: Sql.Decode.i16,
    }

str : Str -> Expr Str
str = \value -> @Expr {
        sql: [Param (Pg.Cmd.str value)],
        decode: Sql.Decode.str,
    }

concat : Expr Str, Expr Str -> Expr Str
concat = \@Expr a, @Expr b -> @Expr {
        sql: a.sql
        |> List.append (Raw " || ")
        |> List.concat b.sql,
        decode: Sql.Decode.str,
    }

eq : Expr a, Expr a -> Expr Bool
eq = \a, b -> boolOp a "=" b

gt : Expr (Num a), Expr (Num a) -> Expr Bool
gt = \a, b -> boolOp a ">" b

boolOp : Expr a, Str, Expr a -> Expr Bool
boolOp = \@Expr a, op, @Expr b -> @Expr {
        sql: a.sql
        |> List.append (Raw " \(op) ")
        |> List.concat b.sql,
        decode: Sql.Decode.bool,
    }

and : Expr Bool, Expr Bool -> Expr Bool
and = \a, b -> boolOp a "and" b

not : Expr Bool -> Expr Bool
not = \@Expr a -> @Expr
        { a &
            sql: List.withCapacity (List.len a.sql + 2)
            |> List.append (Raw "not (")
            |> List.concat a.sql
            |> List.append (Raw ")"),
        }

# Sql helpers

Sql : List Part

Part : [
    Param Binding,
    Raw Str,
]

Compiled : {
    sql : Str,
    params : List Binding,
}

joinWith : List Sql, Sql -> Sql
joinWith = \items, sep ->
    items
    |> List.intersperse sep
    |> List.join

commaJoin : List Sql -> Sql
commaJoin = \items ->
    joinWith items [Raw ", "]

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
            paramCount = List.len params

            { newParams, index } =
                # TODO: Use a dict
                indexResult = params |> List.findFirstIndex \p -> p == param

                when indexResult is
                    Ok existingIndex ->
                        {
                            newParams: params,
                            index: existingIndex,
                        }

                    Err NotFound ->
                        {
                            newParams: params |> List.append param,
                            index: paramCount,
                        }

            binding = Num.toStr (index + 1)

            { sql: "\(sql)$\(binding)", params: newParams }

        Raw raw ->
            { sql: Str.concat sql raw, params }

# Tests

# usersTable = {
#     schema: "public",
#     name: "users",
#     columns: \alias -> {
#         name: identifier alias "name" Sql.Decode.text,
#         active: identifier alias "active" Sql.Decode.bool,
#         age: identifier alias "age" Sql.Decode.u8,
#         organizationId: identifier alias "organization_id" Sql.Decode.u32,
#     },
# }

# orgTable = {
#     schema: "public",
#     name: "organizations",
#     columns: \alias -> {
#         id: identifier alias "id" Sql.Decode.u32,
#         name: identifier alias "name" Sql.Decode.text,
#     },
# }

# expect
#     out =
#         users <- from usersTable
#         select (just users.name)

#     compile out
#     == {
#         sql: "select u.name from public.users as u",
#         params: [],
#     }

# expect
#     out =
#         users <- from usersTable

#         select (just users.name)
#         |> where users.active

#     compile out
#     == {
#         sql: "select u.name from public.users as u where u.active",
#         params: [],
#     }

# expect
#     out = compile
#         (
#             users <- from usersTable

#             select (just users.name)
#             |> where (users.age |> gt (u8 18))
#         )

#     out
#     == {
#         sql: "select u.name from public.users as u where u.age > $1",
#         params: [Pg.Cmd.u8 18],
#     }

# expect
#     out = compile
#         (
#             users <- from usersTable
#             org <- join orgTable \o -> o.id |> eq users.organizationId
#             org2 <- join orgTable \o -> o.id |> eq users.organizationId

#             select (just org.name)
#             |> where (org.id |> eq org2.id)
#             |> limit 10
#         )

#     out
#     == {
#         sql:
#         """
#         select o.name
#         from public.users as u
#         join public.organizations as o on o.id = u.organization_id
#         join public.organizations as o_1 on o_1.id = u.organization_id
#         where o.id = o_1.id
#         limit $1
#         """
#         |> Str.split "\n"
#         |> Str.joinWith " ",
#         params: [Pg.Cmd.nat 10],
#     }

