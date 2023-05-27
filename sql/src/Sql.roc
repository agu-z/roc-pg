interface Sql
    exposes [
        Table,
        Expr,
        NullableExpr,
        queryAll,
        queryOne,
        compileQuery,
        from,
        select,
        identifier,
        where,
        join,
        on,
        limit,
        orderBy,
        asc,
        desc,
        Order,
        i16,
        i32,
        i64,
        str,
        null,
        present,
        isNull,
        isNotNull,
        isDistinctFrom,
        isNotDistinctFrom,
        nullableEq,
        nullableNeq,
        lt,
        lte,
        eq,
        neq,
        gt,
        gte,
        in,
        concat,
        and,
        or,
        not,
        add,
        div,
        sub,
        mul,
        Selection,
        column,
        with,
        into,
        rowArray,
    ]
    imports [
        pg.Pg.Cmd.{ Binding },
        pg.Pg.Result,
        Sql.Types.{
            Nullable,
            Decode,
            PgI16,
            PgI32,
            PgI64,
            PgNum,
            PgText,
            PgBool,
            PgCmp,
        },
    ]

Env : {
    aliases : Dict Str U8,
}

emptyEnv : Env
emptyEnv = {
    aliases: Dict.withCapacity 4,
}

Query a := Env -> { from : Sql, options : SelectOptions a }

Table table : {
    schema : Str,
    name : Str,
    alias : Str,
    columns : Str -> table,
}

from : Table table, (table -> Select a) -> Query a
from = \table, next ->
    env <- @Query

    (newEnv, alias) = addAlias env table.alias
    (@Select toOptions) = next (table.columns alias)

    {
        from: [Raw " from \(table.schema).\(table.name) as \(alias)"],
        options: toOptions newEnv,
    }

addAlias : Env, Str -> (Env, Str)
addAlias = \env, wanted ->
    when Dict.get env.aliases wanted is
        Ok count ->
            strCount = Num.toStr count

            newEnv = { env &
                aliases: env.aliases |> Dict.insert wanted (count + 1),
            }

            (newEnv, "\(wanted)_\(strCount)")

        Err KeyNotFound ->
            newEnv = { env &
                aliases: env.aliases |> Dict.insert wanted 1,
            }

            (newEnv, wanted)

queryAll : Query a -> Pg.Cmd.Cmd _ _
queryAll = \@Query toQuery ->
    query = toQuery emptyEnv
    { sql, params } = querySql query Bare |> compileSql

    Pg.Cmd.new sql
    |> Pg.Cmd.bind params
    |> Pg.Cmd.withCustomDecode \result ->
        cells <- Pg.Result.rows result |> List.mapTry

        cells
        |> query.options.decode 
        |> Result.mapErr DecodeErr

queryOne : Query a -> Pg.Cmd.Cmd _ _
queryOne = \@Query toQuery ->
    query = toQuery emptyEnv
    { sql, params } = querySql query Bare |> compileSql

    Pg.Cmd.new sql
    |> Pg.Cmd.bind params
    |> Pg.Cmd.withCustomDecode \result ->
        rows = Pg.Result.rows result

        when rows |> List.takeFirst 1 is 
            [cells] ->
                cells
                |> query.options.decode 
                |> Result.mapErr DecodeErr

            _ ->
                Err EmptyResult

querySql : { from : _, options : SelectOptions a }, [Bare, RowArray] -> Sql
querySql = \query, columnWrapper ->
    columnsSql =
        when columnWrapper is
            Bare ->
                commaJoin query.options.columns

            RowArray ->
                [Raw "array_agg(row("]
                |> List.concat (commaJoin query.options.columns)
                |> List.append (Raw "))")

    [Raw "select "]
    |> List.reserve 16
    |> List.concat columnsSql
    |> List.concat query.from
    |> List.concat (List.join query.options.joins)
    |> List.concat query.options.where
    |> List.concat query.options.orderBy
    |> List.concat query.options.limit

compileQuery = \@Query toQuery ->
    querySql (toQuery emptyEnv) Bare
    |> compileSql

# Options

Select a := Env -> SelectOptions a

SelectOptions a : {
    joins : List Sql,
    columns : List Sql,
    decode : List (List U8) -> Result a Sql.Types.DecodeErr,
    where : Sql,
    orderBy : Sql,
    limit : Sql,
}

join : Table table, (table -> Expr PgBool *), (table -> Select a) -> Select a
join = \table, onExpr, next ->
    env <- @Select

    (newEnv, alias) = addAlias env table.alias
    columns = table.columns alias

    (@Select toOptions) = next columns
    options = toOptions newEnv

    (@Expr { sql: onSql }) = onExpr columns

    joinSql =
        List.prepend onSql (Raw " join \(table.schema).\(table.name) as \(alias) on ")

    { options & joins: options.joins |> List.prepend joinSql }

on = \toA, b -> \table -> toA table |> eq b

select : Selection a -> Select a
select = \@Selection toSelection ->
    env <- @Select
    { columns, decode } = toSelection env

    {
        joins: List.withCapacity 4,
        columns,
        decode,
        where: [],
        limit: [],
        orderBy: [],
    }

where : Select a, Expr PgBool * -> Select a
where =
    options, (@Expr expr) <- updateOptions

    newWhere =
        if List.isEmpty options.where then
            List.prepend expr.sql (Raw " where ")
        else
            options.where
            |> List.reserve (List.len expr.sql + 1)
            |> List.append (Raw " and ")
            |> List.concat expr.sql

    { options & where: newWhere }

limit : Select a, Nat -> Select a
limit =
    options, max <- updateOptions
    { options & limit: [Raw " limit ", Param (Pg.Cmd.nat max)] }

Order := { sql : Sql, direction : [Asc, Desc] }

asc : Expr (PgCmp *) * -> Order
asc = \@Expr expr -> @Order { sql: expr.sql, direction: Asc }

desc : Expr (PgCmp *) * -> Order
desc = \@Expr expr -> @Order { sql: expr.sql, direction: Desc }

orderBy : Select a, List Order -> Select a
orderBy =
    options, order <- updateOptions

    sql =
        order
        |> List.map \@Order level ->
            direction =
                when level.direction is
                    Asc ->
                        " asc"

                    Desc ->
                        " desc"

            level.sql |> List.append (Raw direction)
        |> commaJoin
        |> List.prepend (Raw " order by ")

    { options & orderBy: sql }

updateOptions : (SelectOptions a, arg -> SelectOptions b) -> (Select a, arg -> Select b)
updateOptions = \fn ->
    \next, arg ->
        @Select \env ->
            (@Select toOptions) = next
            fn (toOptions env) arg

# Selection

Selection a := Env
    -> {
        columns : List Sql,
        decode : List (List U8) -> Result a Sql.Types.DecodeErr,
    }

updateSelection = \fn -> \@Selection toSelection ->
        env <- @Selection
        fn (toSelection env)

into : a -> Selection a
into = \value ->
    _ <- @Selection

    {
        columns: [],
        decode: \_ -> Ok value,
    }

column : Expr * a -> (Selection (a -> b) -> Selection b)
column = \@Expr expr ->
    sel <- updateSelection
    apExprSel expr sel

apExprSel = \expr, sel ->
    index = List.len sel.columns

    decode = \cells ->
        fn <- sel.decode cells |> Result.try
        value <- cells
            |> List.get index
            |> Result.mapErr \OutOfBounds -> MissingColumn index
            |> Result.try
        a <- value
            |> Sql.Types.decode expr.decode
            |> Result.map
        fn a

    {
        columns: sel.columns |> List.append expr.sql,
        decode,
    }

with : Selection a -> (Selection (a -> b) -> Selection b)
with = \@Selection toASel -> \@Selection toFnSel ->
        env <- @Selection

        aSel = toASel env
        fnSel = toFnSel env

        count = List.len fnSel.columns

        decode = \cells ->
            fn <- fnSel.decode cells |> Result.try
            a <- cells
                |> List.drop count
                |> aSel.decode
                |> Result.map
            fn a

        {
            columns: fnSel.columns |> List.concat aSel.columns,
            decode,
        }

rowArray : Query a -> (Selection (List a -> b) -> Selection b)
rowArray = \@Query toQuery -> \@Selection toSel ->
        env <- @Selection

        query = toQuery env

        sql = querySql query RowArray

        wrapped =
            [Raw "("]
            |> List.concat sql
            |> List.append (Raw ")")

        decode = Sql.Types.rowArray \items ->
            List.mapTry items query.options.decode

        expr = { sql: wrapped, decode }

        apExprSel expr (toSel env)

# Expr

Expr pg roc := {
    sql : Sql,
    decode : Decode pg roc,
}

NullableExpr pg roc : Expr (Nullable pg) (Nullable roc)

identifier : Str, Str, Decode pg roc -> Expr pg roc
identifier = \table, col, decode -> @Expr {
        sql: [Raw "\(table).\(col)"],
        decode,
    }

# Expr: Literals

i16 : I16 -> Expr PgI16 I16
i16 = \value -> @Expr {
        sql: [Param (Pg.Cmd.i16 value)],
        decode: Sql.Types.i16,
    }

i32 : I32 -> Expr PgI32 I32
i32 = \value -> @Expr {
        sql: [Param (Pg.Cmd.i32 value)],
        decode: Sql.Types.i32,
    }

i64 : I64 -> Expr PgI64 I64
i64 = \value -> @Expr {
        sql: [Param (Pg.Cmd.i64 value)],
        decode: Sql.Types.i64,
    }

str : Str -> Expr PgText Str
str = \value ->
    @Expr {
        sql: [Param (Pg.Cmd.str value)],
        decode: Sql.Types.str,
    }

null : NullableExpr * []
null = @Expr {
    sql: [Raw "null"],
    decode: Sql.Types.nullable (Sql.Types.fail "not null"),
}

present : Expr pg roc -> NullableExpr pg roc
present = \@Expr a ->
    @Expr {
        sql: a.sql,
        decode: Sql.Types.nullable a.decode,
    }

# Expr: Comparison

isNull : NullableExpr * * -> Expr PgBool Bool
isNull = \@Expr a ->
    @Expr {
        sql: a.sql |> List.append (Raw " is null"),
        decode: Sql.Types.bool,
    }

isNotNull : NullableExpr * * -> Expr PgBool Bool
isNotNull = \@Expr a ->
    @Expr {
        sql: a.sql |> List.append (Raw " is not null"),
        decode: Sql.Types.bool,
    }

isDistinctFrom : NullableExpr (PgCmp a) *, NullableExpr (PgCmp a) * -> Expr PgBool Bool
isDistinctFrom = \a, b -> boolOp a "is distinct from" b

isNotDistinctFrom : NullableExpr (PgCmp a) *, NullableExpr (PgCmp a) * -> Expr PgBool Bool
isNotDistinctFrom = \a, b -> boolOp a "is not distinct from" b

nullableEq = isNotDistinctFrom

nullableNeq = isDistinctFrom

concat : Expr PgText *, Expr PgText * -> Expr PgText Str
concat = \@Expr a, @Expr b ->
    @Expr {
        sql: binOp a.sql "||" b.sql,
        decode: Sql.Types.str,
    }

lt : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr PgBool Bool
lt = \a, b -> boolOp a "<" b

lte : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr PgBool Bool
lte = \a, b -> boolOp a "<=" b

eq : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr PgBool Bool
eq = \a, b -> boolOp a "=" b

neq : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr PgBool Bool
neq = \a, b -> boolOp a "<>" b

gte : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr PgBool Bool
gte = \a, b -> boolOp a ">=" b

gt : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr PgBool Bool
gt = \a, b -> boolOp a ">" b

in : Expr (PgCmp a) *, (item -> Expr (PgCmp a) *), List item -> Expr PgBool Bool
in = \@Expr needle, toExpr, haystack ->
    itemToSql = \item ->
        (@Expr { sql }) = toExpr item
        sql

    listItems =
        haystack
        |> List.map itemToSql
        |> commaJoin

    inSql =
        needle.sql
        |> List.reserve (List.len listItems + 2)
        |> List.append (Raw " in (")
        |> List.concat listItems
        |> List.append (Raw ")")

    @Expr {
        sql: inSql,
        decode: Sql.Types.bool,
    }

boolOp : Expr pg *, Str, Expr pg * -> Expr PgBool Bool
boolOp = \@Expr a, op, @Expr b ->
    @Expr {
        sql: binOp a.sql op b.sql,
        decode: Sql.Types.bool,
    }

# Expr: Logical

and : Expr PgBool *, Expr PgBool * -> Expr PgBool Bool
and = \a, b -> boolOp a "and" b

or : Expr PgBool *, Expr PgBool * -> Expr PgBool Bool
or = \@Expr a, @Expr b ->
    @Expr {
        sql: binOpParens a.sql "or" b.sql,
        decode: Sql.Types.bool,
    }

not : Expr PgBool * -> Expr PgBool Bool
not = \@Expr a ->
    @Expr {
        sql: [Raw "not ("]
        |> List.reserve (List.len a.sql + 1)
        |> List.concat a.sql
        |> List.append (Raw ")"),
        decode: Sql.Types.bool,
    }

# Expr: Num

add : Expr (PgNum pg) roc, Expr (PgNum pg) roc -> Expr (PgNum pg) roc
add = \@Expr a, @Expr b ->
    @Expr {
        sql: binOpParens a.sql "+" b.sql,
        decode: a.decode,
    }

sub : Expr (PgNum pg) roc, Expr (PgNum pg) roc -> Expr (PgNum pg) roc
sub = \@Expr a, @Expr b ->
    @Expr {
        sql: binOpParens a.sql "-" b.sql,
        decode: a.decode,
    }

mul : Expr (PgNum pg) roc, Expr (PgNum pg) roc -> Expr (PgNum pg) roc
mul = \@Expr a, @Expr b ->
    @Expr {
        sql: binOp a.sql "*" b.sql,
        decode: a.decode,
    }

div : Expr (PgNum pg) roc, Expr (PgNum pg) roc -> Expr (PgNum pg) roc
div = \@Expr a, @Expr b ->
    @Expr {
        sql: binOp a.sql "/" b.sql,
        decode: a.decode,
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

binOp = \a, op, b ->
    a
    |> List.reserve (List.len b + 1)
    |> List.append (Raw " \(op) ")
    |> List.concat b

binOpParens = \a, op, b ->
    [Raw "("]
    |> List.reserve (List.len a + List.len b + 2)
    |> List.concat a
    |> List.append (Raw " \(op) ")
    |> List.concat b
    |> List.append (Raw ")")

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
#         name: identifier alias "name" Sql.Types.text,
#         active: identifier alias "active" Sql.Types.bool,
#         age: identifier alias "age" Sql.Types.u8,
#         organizationId: identifier alias "organization_id" Sql.Types.u32,
#     },
# }

# orgTable = {
#     schema: "public",
#     name: "organizations",
#     columns: \alias -> {
#         id: identifier alias "id" Sql.Types.u32,
#         name: identifier alias "name" Sql.Types.text,
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

