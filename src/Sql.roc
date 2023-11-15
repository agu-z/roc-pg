interface Sql
    exposes [
        Table,
        Expr,
        NullableExpr,
        Select,
        Query,
        queryAll,
        queryOne,
        querySelection,
        compileQuery,
        from,
        select,
        where,
        join,
        on,
        leftJoin,
        useOuter,
        limit,
        offset,
        orderBy,
        asc,
        desc,
        Order,
        identifier,
        discardPhantom,
        i16,
        i32,
        i64,
        str,
        null,
        asNullable,
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
        like,
        ilike,
        concat,
        and,
        or,
        andList,
        orList,
        not,
        true,
        false,
        add,
        div,
        sub,
        mul,
        Selection,
        column,
        with,
        into,
        map,
        selectionList,
        row,
        rowArray,
        tryMapQuery,
    ]
    imports [
        pg.Pg.Cmd.{ Binding },
        pg.Pg.Result,
        Sql.Types.{
            Decode,
            PgI16,
            PgI32,
            PgI64,
            PgNum,
            PgText,
            PgBool,
            PgCmp,
        },
        Sql.Nullable.{ Nullable },
        State.{ State },
    ]

Env : {
    aliases : Dict Str U8,
}

emptyEnv : Env
emptyEnv = {
    aliases: Dict.withCapacity 4,
}

Query a err := State Env { from : Sql, options : SelectOptions a } err

Table table : {
    schema : Str,
    name : Str,
    alias : Str,
    columns : Str -> table,
}

from : Table table, (table -> Select a err) -> Query a err
from = \table, next -> @Query (fromHelp table next)

fromHelp = \table, next ->
    alias <- addAlias table.alias |> State.bind

    options <- table.columns alias
        |> next
        |> unwrapSelect
        |> State.map

    {
        from: [Raw " from \(table.schema).\(table.name) as \(alias)"],
        options,
    }

addAlias : Str -> State Env Str err
addAlias = \wanted ->
    env <- State.get |> State.bind

    when Dict.get env.aliases wanted is
        Ok count ->
            strCount = Num.toStr count

            newEnv = { env &
                aliases: env.aliases |> Dict.insert wanted (count + 1),
            }

            _ <- State.put newEnv |> State.map

            "\(wanted)_\(strCount)"

        Err KeyNotFound ->
            newEnv = { env &
                aliases: env.aliases |> Dict.insert wanted 1,
            }

            _ <- State.put newEnv |> State.map

            wanted

queryAll : Query (Selection a []) [] -> Pg.Cmd.Cmd _ _
queryAll = \qs ->
    query = buildBareQuery qs emptyEnv
    { sql, params } = compileSql query.sql

    Pg.Cmd.new sql
    |> Pg.Cmd.bind params
    |> Pg.Cmd.withCustomDecode \result ->
        cells <- Pg.Result.rows result |> List.mapTry

        cells
        |> query.decode
        |> Result.mapErr DecodeErr

queryOne : Query (Selection a []) [] -> Pg.Cmd.Cmd _ _
queryOne = \qs ->
    query = buildBareQuery qs emptyEnv
    { sql, params } = compileSql query.sql

    Pg.Cmd.new sql
    |> Pg.Cmd.bind params
    |> Pg.Cmd.withCustomDecode \result ->
        rows = Pg.Result.rows result

        when rows |> List.takeFirst 1 is
            [cells] ->
                cells
                |> query.decode
                |> Result.mapErr DecodeErr

            _ ->
                Err EmptyResult

querySelection : Selection a err -> Result (Pg.Cmd.Cmd _ _) err
querySelection = \@Selection ss ->
    (sel, _) <- State.attempt ss emptyEnv |> Result.map

    { sql, params } =
        # TODO: Simplify to no-op cmd
        if List.isEmpty sel.columns then
            [Raw "select 1"]
            |> compileSql
        else
            commaJoin sel.columns
            |> List.prepend (Raw "select ")
            |> compileSql

    Pg.Cmd.new sql
    |> Pg.Cmd.bind params
    |> Pg.Cmd.withCustomDecode \result ->
        rows = Pg.Result.rows result

        when rows |> List.takeFirst 1 is
            [cells] ->
                cells
                |> sel.decode
                |> Result.mapErr DecodeErr

            _ ->
                Err EmptyResult

buildBareQuery : Query (Selection a []) [],
    Env
    -> {
        sql : Sql,
        decode : List (List U8) -> Result a Sql.Types.DecodeErr,
    }
buildBareQuery = \@Query qs, initEnv ->
    combined =
        query <- State.bind qs
        sel <- query.options.value |> unwrapSelection |> State.map

        { query, sel }

    (result, _) = State.perform combined initEnv

    {
        sql: querySql result.query result.sel.columns Bare,
        decode: result.sel.decode,
    }

querySql = \query, columns, columnWrapper ->
    columnsSql =
        when columnWrapper is
            Bare ->
                commaJoin columns

            Row ->
                [Raw "row("]
                |> List.concat (commaJoin columns)
                |> List.append (Raw ")")

    joinsSql =
        List.join query.options.joins

    [Raw "select "]
    |> List.reserve
        (
            List.len columnsSql
            + List.len query.from
            + List.len joinsSql
            + List.len query.options.where
            + List.len query.options.orderBy
            + List.len query.options.limit
            + List.len query.options.offset
        )
    |> List.concat columnsSql
    |> List.concat query.from
    |> List.concat joinsSql
    |> List.concat query.options.where
    |> List.concat query.options.orderBy
    |> List.concat query.options.limit
    |> List.concat query.options.offset

compileQuery = \qs ->
    buildBareQuery qs emptyEnv
    |> .sql
    |> compileSql

# Advanced: Failable query building

tryMapQuery : Query a err, (a -> Result b err) -> Query b err
tryMapQuery = \@Query qs, fn ->
    @Query (tryMapQueryHelp qs fn)

tryMapQueryHelp = \qs, fn ->
    query <- State.bind qs

    fn query.options.value
    |> Result.map \new -> {
        from: query.from,
        options: {
            joins: query.options.joins,
            where: query.options.where,
            orderBy: query.options.orderBy,
            limit: query.options.limit,
            offset: query.options.offset,
            value: new,
        },
    }
    |> State.fromResult

# Options

Select a err := State Env (SelectOptions a) err

SelectOptions a : {
    joins : List Sql,
    where : Sql,
    orderBy : Sql,
    limit : Sql,
    offset : Sql,
    value : a,
}

unwrapSelect = \@Select state -> state

join : Table table, (table -> Expr (PgBool *) *), (table -> Select a err) -> Select a err
join = \table, onExpr, next ->
    @Select (joinHelp table onExpr next)

joinHelp = \table, onExpr, next ->
    alias <- addAlias table.alias |> State.bind
    columns = table.columns alias

    (@Expr { sql: onSql }) = onExpr columns

    options <-
        next columns
        |> unwrapSelect
        |> State.map

    joinSql =
        List.prepend onSql (Raw " join \(table.schema).\(table.name) as \(alias) on ")

    { options & joins: options.joins |> List.prepend joinSql }

Outer table := table

leftJoin : Table table, (table -> Expr (PgBool *) *), (Outer table -> Select a err) -> Select a err
leftJoin = \table, onExpr, next ->
    @Select (leftJoinHelp table onExpr next)

leftJoinHelp = \table, onExpr, next ->
    alias <- addAlias table.alias |> State.bind
    columns = table.columns alias

    (@Expr { sql: onSql }) = onExpr columns

    options <-
        next (@Outer columns)
        |> unwrapSelect
        |> State.map

    joinSql =
        List.prepend onSql (Raw " left join \(table.schema).\(table.name) as \(alias) on ")

    { options & joins: options.joins |> List.prepend joinSql }

useOuter : Outer table, (table -> Expr pg a) -> NullableExpr pg a
useOuter = \@Outer table, expr ->
    # DESIGN: break into its own module? might help combine multiple outer joins
    asNullable (expr table)

on = \toA, b -> \table -> toA table |> eq b

select : a -> Select a *
select = \a ->
    {
        joins: List.withCapacity 4,
        where: [],
        limit: [],
        offset: [],
        orderBy: [],
        value: a,
    }
    |> State.ok
    |> @Select

where : Select a err, Expr (PgBool *) * -> Select a err
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

limit : Select a err, U64 -> Select a err
limit =
    options, max <- updateOptions
    { options & limit: [Raw " limit ", Param (Pg.Cmd.u64 max)] }

offset : Select a err, U64 -> Select a err
offset =
    options, start <- updateOptions
    { options & offset: [Raw " offset ", Param (Pg.Cmd.u64 start)] }

Order := { sql : Sql, direction : [Asc, Desc] }

asc : Expr (PgCmp *) * -> Order
asc = \@Expr expr -> @Order { sql: expr.sql, direction: Asc }

desc : Expr (PgCmp *) * -> Order
desc = \@Expr expr -> @Order { sql: expr.sql, direction: Desc }

orderBy : Select a err, List Order -> Select a err
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

updateOptions : (SelectOptions a, arg -> SelectOptions b) -> (Select a err, arg -> Select b err)
updateOptions = \fn ->
    \@Select next, arg ->
        @Select (State.map next \options -> fn options arg)

# Selection

Selection a err := State
        Env
        {
            columns : List Sql,
            decode : List (List U8) -> Result a Sql.Types.DecodeErr,
        }
        err

unwrapSelection = \@Selection state -> state

updateSelection = \@Selection sel, fn -> @Selection (State.map sel \options -> fn options)

into : a -> Selection a err
into = \value ->
    {
        columns: [],
        decode: \_ -> Ok value,
    }
    |> State.ok
    |> @Selection

column : Expr * a -> (Selection (a -> b) err -> Selection b err)
column = \@Expr expr -> \ss ->
        sel <- updateSelection ss
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

with : Selection a err -> (Selection (a -> b) err -> Selection b err)
with = \@Selection toASel -> \@Selection toFnSel ->
        @Selection (withHelp toASel toFnSel)

withHelp = \toASel, toFnSel ->
    env <- State.get |> State.bind

    result =
        (aSel, _) <- State.attempt toASel env |> Result.try
        (fnSel, _) <- State.attempt toFnSel env |> Result.map

        count = List.len fnSel.columns

        decode = \cells ->
            fn <- fnSel.decode cells |> Result.try
            a <- cells
                |> List.dropFirst count
                |> aSel.decode
                |> Result.map
            fn a

        {
            columns: fnSel.columns |> List.concat aSel.columns,
            decode,
        }

    State.fromResult result

row : Query (Selection a err) err -> (Selection (a -> b) err -> Selection b err)
row = \@Query qs -> \@Selection ps ->
        @Selection (rowHelp qs ps)

rowHelp = \qs, ps ->
    sel <- State.bind ps
    env <- State.get |> State.bind

    rowState =
        query <- State.bind qs
        rowSel <- query.options.value |> unwrapSelection |> State.map

        limited =
            options = query.options
            { query & options: { options & limit: [Raw " limit 1"] } }

        { query: limited, sel: rowSel }

    State.attempt rowState env
    |> Result.map \(rowQ, _) ->
        sql =
            [Raw "("]
            |> List.concat (querySql rowQ.query rowQ.sel.columns Row)
            |> List.append (Raw ")")

        {
            sql,
            decode: Sql.Types.row rowQ.sel.decode,
        }
        |> apExprSel sel
    |> State.fromResult

rowArray : Query (Selection a err) err -> (Selection (List a -> b) err -> Selection b err)
rowArray = \@Query qs -> \@Selection ps -> @Selection (rowArrayHelp qs ps)

rowArrayHelp = \qs, ps ->
    sel <- State.bind ps
    env <- State.get |> State.bind

    rowState =
        query <- State.bind qs
        rowSel <- query.options.value |> unwrapSelection |> State.map

        {
            sql: querySql query rowSel.columns Row,
            decode: rowSel.decode,
        }

    State.attempt rowState env
    |> Result.map \(rowQ, _) ->
        sql =
            [Raw "(select array("]
            |> List.concat rowQ.sql
            |> List.append (Raw "))")

        decode = Sql.Types.array (Sql.Types.row rowQ.decode)

        expr = { sql, decode }

        apExprSel expr sel
    |> State.fromResult

selectionList : List (Selection a err) -> Selection (List a) err
selectionList = \sels -> @Selection (selectionListHelp sels)

selectionListHelp = \sels ->
    env <- State.get |> State.bind

    sels
    |> List.mapTry \@Selection sel -> State.attempt sel env |> Result.map .0
    |> Result.map \allSels ->
        decode = \cells ->
            allSels
            |> List.walkTry (cells, List.withCapacity (List.len sels)) decodeSel
            |> Result.map .1

        decodeSel = \(remainingCells, decodedSels), sel ->
            { before, others } = List.split remainingCells (List.len sel.columns)

            decoded <- sel.decode before |> Result.map

            (others, List.append decodedSels decoded)

        {
            columns: allSels |> List.joinMap .columns,
            decode,
        }
    |> State.fromResult

map : Selection a err, (a -> b) -> Selection b err
map = \ss, fn ->
    sel <- updateSelection ss

    {
        columns: sel.columns,
        decode: \cells -> Result.map (sel.decode cells) fn,
    }

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

## Discards the phantom type variable that represents the SQL-level type.
##
## This should be used as a last resort, when the query builder is preventing you
## from writing an expression that you know it's valid.
##
## IMPORTANT: The compiled expression won't include a SQL cast!
discardPhantom : Expr * roc -> Expr * roc
discardPhantom = \@Expr expr -> @Expr {
        sql: expr.sql,
        decode: Sql.Types.discardPhantom expr.decode,
    }

# Expr: Literals

i16 : I16 -> Expr (PgI16 *) I16
i16 = \value -> @Expr {
        sql: [Param (Pg.Cmd.i16 value)],
        decode: Sql.Types.i16,
    }

i32 : I32 -> Expr (PgI32 *) I32
i32 = \value -> @Expr {
        sql: [Param (Pg.Cmd.i32 value)],
        decode: Sql.Types.i32,
    }

i64 : I64 -> Expr (PgI64 *) I64
i64 = \value -> @Expr {
        sql: [Param (Pg.Cmd.i64 value)],
        decode: Sql.Types.i64,
    }

str : Str -> Expr (PgText *) Str
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

asNullable : Expr pg roc -> NullableExpr pg roc
asNullable = \@Expr a ->
    @Expr {
        sql: a.sql,
        decode: Sql.Types.nullable a.decode,
    }

# Expr: Comparison

isNull : NullableExpr * * -> Expr (PgBool *) Bool
isNull = \@Expr a ->
    @Expr {
        sql: a.sql |> List.append (Raw " is null"),
        decode: Sql.Types.bool,
    }

isNotNull : NullableExpr * * -> Expr (PgBool *) Bool
isNotNull = \@Expr a ->
    @Expr {
        sql: a.sql |> List.append (Raw " is not null"),
        decode: Sql.Types.bool,
    }

isDistinctFrom : NullableExpr (PgCmp a) *, NullableExpr (PgCmp a) * -> Expr (PgBool *) Bool
isDistinctFrom = \a, b -> boolOp a "is distinct from" b

isNotDistinctFrom : NullableExpr (PgCmp a) *, NullableExpr (PgCmp a) * -> Expr (PgBool *) Bool
isNotDistinctFrom = \a, b -> boolOp a "is not distinct from" b

nullableEq = isNotDistinctFrom

nullableNeq = isDistinctFrom

concat : Expr (PgText *) *, Expr (PgText *) * -> Expr (PgText *) Str
concat = \@Expr a, @Expr b ->
    @Expr {
        sql: binOp a.sql "||" b.sql,
        decode: Sql.Types.str,
    }

lt : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr (PgBool *) Bool
lt = \a, b -> boolOp a "<" b

lte : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr (PgBool *) Bool
lte = \a, b -> boolOp a "<=" b

eq : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr (PgBool *) Bool
eq = \a, b -> boolOp a "=" b

neq : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr (PgBool *) Bool
neq = \a, b -> boolOp a "<>" b

gte : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr (PgBool *) Bool
gte = \a, b -> boolOp a ">=" b

gt : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr (PgBool *) Bool
gt = \a, b -> boolOp a ">" b

in : Expr (PgCmp a) *, (item -> Expr (PgCmp a) *), List item -> Expr (PgBool *) Bool
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

like : Expr (PgText *) *, Expr (PgText *) * -> Expr (PgBool *) Bool
like = \a, b ->
    boolOp a "like" b

ilike : Expr (PgText *) *, Expr (PgText *) * -> Expr (PgBool *) Bool
ilike = \a, b ->
    boolOp a "ilike" b

boolOp : Expr * *, Str, Expr * * -> Expr (PgBool *) Bool
boolOp = \@Expr a, op, @Expr b ->
    @Expr {
        sql: binOp a.sql op b.sql,
        decode: Sql.Types.bool,
    }

# Expr: Logical
# TODO: Test precendence is ok!

and : Expr (PgBool *) *, Expr (PgBool *) * -> Expr (PgBool *) Bool
and = \a, b -> boolOp a "and" b

or : Expr (PgBool *) *, Expr (PgBool a) * -> Expr (PgBool *) Bool
or = \@Expr a, @Expr b ->
    @Expr {
        sql: binOpParens a.sql "or" b.sql,
        decode: Sql.Types.bool,
    }

not : Expr (PgBool *) * -> Expr (PgBool *) Bool
not = \@Expr a ->
    @Expr {
        sql: [Raw "not ("]
        |> List.reserve (List.len a.sql + 1)
        |> List.concat a.sql
        |> List.append (Raw ")"),
        decode: Sql.Types.bool,
    }

andList : List (Expr (PgBool a) Bool) -> Expr (PgBool a) Bool
andList = \exprs ->
    joinBool openTrue and exprs

orList : List (Expr (PgBool a) Bool) -> Expr (PgBool a) Bool
orList = \exprs ->
    joinBool openFalse or exprs

BoolOp a : Expr (PgBool a) Bool, Expr (PgBool a) Bool -> Expr (PgBool a) Bool

joinBool : Expr (PgBool a) Bool, BoolOp a, List (Expr (PgBool a) Bool) -> Expr (PgBool a) Bool
joinBool = \default, operator, exprs ->
    # We could simplify this by using default as the initial value,
    # but in most cases we would produce something like (true and ...),
    # which is not very nice to read.
    result =
        List.walk exprs Empty \acc, rhs ->
            when acc is
                Empty ->
                    NotEmpty rhs

                NotEmpty lhs ->
                    NotEmpty (operator lhs rhs)

    when result is
        Empty ->
            default

        NotEmpty expr ->
            expr

true : Expr (PgBool a) Bool
true = @Expr {
    sql: [Raw "true"],
    decode: Sql.Types.bool,
}

false : Expr (PgBool *) Bool
false = @Expr {
    sql: [Raw "false"],
    decode: Sql.Types.bool,
}

# These are needed for `andList` and `orList`

openTrue = @Expr {
    sql: [Raw "true"],
    decode: Sql.Types.bool,
}

openFalse = @Expr {
    sql: [Raw "false"],
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

