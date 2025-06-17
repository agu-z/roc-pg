module [
    Table,
    Expr,
    NullableExpr,
    Select,
    Query,
    query_all,
    query_one,
    query_selection,
    compile_query,
    from,
    select,
    where,
    join,
    on,
    left_join,
    use_outer,
    limit,
    offset,
    order_by,
    asc,
    desc,
    Order,
    identifier,
    discard_phantom,
    i16,
    i32,
    i64,
    str,
    null,
    as_nullable,
    is_null,
    is_not_null,
    is_distinct_from,
    is_not_distinct_from,
    nullable_eq,
    nullable_neq,
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
    and_,
    or_,
    and_list,
    or_list,
    not_,
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
    selection_list,
    row,
    row_array,
    try_map_query,
]

import pg.Pg.Cmd exposing [Binding]
import pg.Pg.Result
import Sql.Types exposing [
    Decode,
    PgI16,
    PgI32,
    PgI64,
    PgNum,
    PgText,
    PgBool,
    PgCmp,
]
import Sql.Nullable exposing [Nullable]
import State exposing [State]

Env : {
    aliases : Dict Str U8,
}

empty_env : Env
empty_env = {
    aliases: Dict.with_capacity(4),
}

Query a err := State Env { from : Sql, options : SelectOptions a } err

Table table : {
    schema : Str,
    name : Str,
    alias : Str,
    columns : Str -> table,
}

from : Table table, (table -> Select a err) -> Query a err
from = |table, next| @Query(from_help(table, next))

from_help = |table, next|
    add_alias(table.alias)
    |> State.bind(
        |alias|
            table.columns(alias)
            |> next
            |> unwrap_select
            |> State.map(
                |options| {
                    from: [Raw(" from ${table.schema}.${table.name} as ${alias}")],
                    options,
                },
            ),
    )

add_alias : Str -> State Env Str err
add_alias = |wanted|
    State.get
    |> State.bind(
        |env|
            when Dict.get(env.aliases, wanted) is
                Ok(count) ->
                    str_count = Num.to_str(count)

                    new_env = { env &
                        aliases: env.aliases |> Dict.insert(wanted, (count + 1)),
                    }

                    State.put(new_env)
                    |> State.map(
                        |_|
                            "${wanted}_${str_count}",
                    )

                Err(KeyNotFound) ->
                    new_env = { env &
                        aliases: env.aliases |> Dict.insert(wanted, 1),
                    }

                    State.put(new_env)
                    |> State.map(
                        |_|
                            wanted,
                    ),
    )

query_all : Query (Selection a []) [] -> Pg.Cmd.Cmd _ _
query_all = |qs|
    query = build_bare_query(qs, empty_env)
    { sql, params } = compile_sql(query.sql)

    Pg.Cmd.new(sql)
    |> Pg.Cmd.bind(params)
    |> Pg.Cmd.with_custom_decode(
        |result|
            Pg.Result.rows(result)
            |> List.map_try(
                |cells|
                    cells
                    |> query.decode
                    |> Result.map_err(DecodeErr),
            ),
    )

query_one : Query (Selection a []) [] -> Pg.Cmd.Cmd _ _
query_one = |qs|
    query = build_bare_query(qs, empty_env)
    { sql, params } = compile_sql(query.sql)

    Pg.Cmd.new(sql)
    |> Pg.Cmd.bind(params)
    |> Pg.Cmd.with_custom_decode(
        |result|
            rows = Pg.Result.rows(result)

            when rows |> List.take_first(1) is
                [cells] ->
                    cells
                    |> query.decode
                    |> Result.map_err(DecodeErr)

                _ ->
                    Err(EmptyResult),
    )

query_selection : Selection a err -> Result (Pg.Cmd.Cmd _ _) err
query_selection = |@Selection(ss)|
    State.attempt(ss, empty_env)
    |> Result.map_ok(
        |(sel, _)|
            { sql, params } =
                # TODO: Simplify to no-op cmd
                if List.is_empty(sel.columns) then
                    [Raw("select 1")]
                    |> compile_sql
                else
                    comma_join(sel.columns)
                    |> List.prepend(Raw("select "))
                    |> compile_sql

            Pg.Cmd.new(sql)
            |> Pg.Cmd.bind(params)
            |> Pg.Cmd.with_custom_decode(
                |result|
                    rows = Pg.Result.rows(result)

                    when rows |> List.take_first(1) is
                        [cells] ->
                            cells
                            |> sel.decode
                            |> Result.map_err(DecodeErr)

                        _ ->
                            Err(EmptyResult),
            ),
    )

build_bare_query :
    Query (Selection a []) [],
    Env
    -> {
        sql : Sql,
        decode : List (List U8) -> Result a Sql.Types.DecodeErr,
    }
build_bare_query = |@Query(qs), init_env|
    combined =
        State.bind(
            qs,
            |query|
                query.options.value
                |> unwrap_selection
                |> State.map(
                    |sel|
                        { query, sel },
                ),
        )

    (result, _) = State.perform(combined, init_env)

    {
        sql: query_sql(result.query, result.sel.columns, Bare),
        decode: result.sel.decode,
    }

query_sql = |query, columns, column_wrapper|
    columns_sql =
        when column_wrapper is
            Bare ->
                comma_join(columns)

            Row ->
                [Raw("row(")]
                |> List.concat(comma_join(columns))
                |> List.append(Raw(")"))

    joins_sql =
        List.join(query.options.joins)

    [Raw("select ")]
    |> List.reserve(
        (
            List.len(columns_sql)
            + List.len(query.from)
            + List.len(joins_sql)
            + List.len(query.options.where)
            + List.len(query.options.order_by)
            + List.len(query.options.limit)
            + List.len(query.options.offset)
        ),
    )
    |> List.concat(columns_sql)
    |> List.concat(query.from)
    |> List.concat(joins_sql)
    |> List.concat(query.options.where)
    |> List.concat(query.options.order_by)
    |> List.concat(query.options.limit)
    |> List.concat(query.options.offset)

compile_query = |qs|
    build_bare_query(qs, empty_env)
    |> .sql
    |> compile_sql

# Advanced: Failable query building

try_map_query : Query a err, (a -> Result b err) -> Query b err
try_map_query = |@Query(qs), fn|
    @Query(try_map_query_help(qs, fn))

try_map_query_help = |qs, fn|
    State.bind(
        qs,
        |query|
            fn(query.options.value)
            |> Result.map_ok(
                |new| {
                    from: query.from,
                    options: {
                        joins: query.options.joins,
                        where: query.options.where,
                        order_by: query.options.order_by,
                        limit: query.options.limit,
                        offset: query.options.offset,
                        value: new,
                    },
                },
            )
            |> State.from_result,
    )

# Options

Select a err := State Env (SelectOptions a) err

SelectOptions a : {
    joins : List Sql,
    where : Sql,
    order_by : Sql,
    limit : Sql,
    offset : Sql,
    value : a,
}

unwrap_select = |@Select(state)| state

join : Table table, (table -> Expr (PgBool *) *), (table -> Select a err) -> Select a err
join = |table, on_expr, next|
    @Select(join_help(table, on_expr, next))

join_help = |table, on_expr, next|
    add_alias(table.alias)
    |> State.bind(
        |alias|
            columns = table.columns(alias)

            @Expr({ sql: on_sql }) = on_expr(columns)

            next(columns)
            |> unwrap_select
            |> State.map(
                |options|
                    join_sql =
                        List.prepend(on_sql, Raw(" join ${table.schema}.${table.name} as ${alias} on "))

                    { options & joins: options.joins |> List.prepend(join_sql) },
            ),
    )

Outer table := table

left_join : Table table, (table -> Expr (PgBool *) *), (Outer table -> Select a err) -> Select a err
left_join = |table, on_expr, next|
    @Select(left_join_help(table, on_expr, next))

left_join_help = |table, on_expr, next|
    add_alias(table.alias)
    |> State.bind(
        |alias|
            columns = table.columns(alias)

            @Expr({ sql: on_sql }) = on_expr(columns)

            next(@Outer(columns))
            |> unwrap_select
            |> State.map(
                |options|
                    join_sql =
                        List.prepend(on_sql, Raw(" left join ${table.schema}.${table.name} as ${alias} on "))

                    { options & joins: options.joins |> List.prepend(join_sql) },
            ),
    )

use_outer : Outer table, (table -> Expr pg a) -> NullableExpr pg a
use_outer = |@Outer(table), expr|
    # DESIGN: break into its own module? might help combine multiple outer joins
    as_nullable(expr(table))

on = |to_a, b| |table| to_a(table) |> eq(b)

select : a -> Select a *
select = |a|
    {
        joins: List.with_capacity(4),
        where: [],
        limit: [],
        offset: [],
        order_by: [],
        value: a,
    }
    |> State.ok
    |> @Select

where : Select _ _, Expr (PgBool _) _ -> Select _ _
where =
    update_options(
        |options, @Expr(expr)|
            new_where =
                if List.is_empty(options.where) then
                    List.prepend(expr.sql, Raw(" where "))
                else
                    options.where
                    |> List.reserve((List.len(expr.sql) + 1))
                    |> List.append(Raw(" and "))
                    |> List.concat(expr.sql)

            { options & where: new_where },
    )

limit : Select _ _, U64 -> Select _ _
limit =
    update_options(
        |options, max|
            { options & limit: [Raw(" limit "), Param(Pg.Cmd.u64(max))] },
    )

offset : Select _ _, U64 -> Select _ _
offset =
    update_options(
        |options, start|
            { options & offset: [Raw(" offset "), Param(Pg.Cmd.u64(start))] },
    )

Order := { sql : Sql, direction : [Asc, Desc] }

asc : Expr (PgCmp *) * -> Order
asc = |@Expr(expr)| @Order({ sql: expr.sql, direction: Asc })

desc : Expr (PgCmp *) * -> Order
desc = |@Expr(expr)| @Order({ sql: expr.sql, direction: Desc })

order_by : Select _ _, List Order -> Select _ _
order_by =
    update_options(
        |options, order|
            sql =
                order
                |> List.map(
                    |@Order(level)|
                        direction =
                            when level.direction is
                                Asc ->
                                    " asc"

                                Desc ->
                                    " desc"

                        level.sql |> List.append(Raw(direction)),
                )
                |> comma_join
                |> List.prepend(Raw(" order by "))

            { options & order_by: sql },
    )

update_options : (SelectOptions a, arg -> SelectOptions b) -> (Select a err, arg -> Select b err)
update_options = |fn|
    |@Select(next), arg|
        @Select(State.map(next, |options| fn(options, arg)))

# Selection

Selection a err :=
    State
        Env
        {
            columns : List Sql,
            decode : List (List U8) -> Result a Sql.Types.DecodeErr,
        }
        err

unwrap_selection = |@Selection(state)| state

update_selection = |@Selection(sel), fn| @Selection(State.map(sel, |options| fn(options)))

into : a -> Selection a err
into = |value|
    {
        columns: [],
        decode: |_| Ok(value),
    }
    |> State.ok
    |> @Selection

column : Expr * a -> (Selection (a -> b) err -> Selection b err)
column = |@Expr(expr)|
    |ss|
        update_selection(
            ss,
            |sel|
                ap_expr_sel(expr, sel),
        )

ap_expr_sel = |expr, sel|
    index = List.len(sel.columns)

    decode = |cells|
        sel.decode(cells)
        |> Result.try(
            |fn|
                cells
                |> List.get(index)
                |> Result.map_err(|OutOfBounds| MissingColumn(index))
                |> Result.try(
                    |value|
                        value
                        |> Sql.Types.decode(expr.decode)
                        |> Result.map_ok(
                            |a|
                                fn(a),
                        ),
                ),
        )

    {
        columns: sel.columns |> List.append(expr.sql),
        decode,
    }

with : Selection a err -> (Selection (a -> b) err -> Selection b err)
with = |@Selection(to_a_sel)|
    |@Selection(to_fn_sel)|
        @Selection(with_help(to_a_sel, to_fn_sel))

with_help = |to_a_sel, to_fn_sel|
    State.get
    |> State.bind(
        |env|
            result =
                State.attempt(to_a_sel, env)
                |> Result.try(
                    |(a_sel, _)|
                        State.attempt(to_fn_sel, env)
                        |> Result.map_ok(
                            |(fn_sel, _)|
                                count = List.len(fn_sel.columns)

                                decode = |cells|
                                    fn_sel.decode(cells)
                                    |> Result.try(
                                        |fn|
                                            cells
                                            |> List.drop_first(count)
                                            |> a_sel.decode
                                            |> Result.map_ok(
                                                |a|
                                                    fn(a),
                                            ),
                                    )

                                {
                                    columns: fn_sel.columns |> List.concat(a_sel.columns),
                                    decode,
                                },
                        ),
                )

            State.from_result(result),
    )

row : Query (Selection a err) err -> (Selection (a -> b) err -> Selection b err)
row = |@Query(qs)|
    |@Selection(ps)|
        @Selection(row_help(qs, ps))

row_help = |qs, ps|
    State.bind(
        ps,
        |sel|
            State.get
            |> State.bind(
                |env|
                    row_state =
                        State.bind(
                            qs,
                            |query|
                                query.options.value
                                |> unwrap_selection
                                |> State.map(
                                    |row_sel|
                                        limited =
                                            options = query.options
                                            { query & options: { options & limit: [Raw(" limit 1")] } }

                                        { query: limited, sel: row_sel },
                                ),
                        )

                    State.attempt(row_state, env)
                    |> Result.map_ok(
                        |(row_q, _)|
                            sql =
                                [Raw("(")]
                                |> List.concat(query_sql(row_q.query, row_q.sel.columns, Row))
                                |> List.append(Raw(")"))

                            {
                                sql,
                                decode: Sql.Types.row(row_q.sel.decode),
                            }
                            |> ap_expr_sel(sel),
                    )
                    |> State.from_result,
            ),
    )

row_array : Query (Selection a err) err -> (Selection (List a -> b) err -> Selection b err)
row_array = |@Query(qs)| |@Selection(ps)| @Selection(row_array_help(qs, ps))

row_array_help = |qs, ps|
    State.bind(
        ps,
        |sel|
            State.get
            |> State.bind(
                |env|
                    row_state =
                        State.bind(
                            qs,
                            |query|
                                query.options.value
                                |> unwrap_selection
                                |> State.map(
                                    |row_sel| {
                                        sql: query_sql(query, row_sel.columns, Row),
                                        decode: row_sel.decode,
                                    },
                                ),
                        )

                    State.attempt(row_state, env)
                    |> Result.map_ok(
                        |(row_q, _)|
                            sql =
                                [Raw("(select array(")]
                                |> List.concat(row_q.sql)
                                |> List.append(Raw("))"))

                            decode = Sql.Types.array(Sql.Types.row(row_q.decode))

                            expr = { sql, decode }

                            ap_expr_sel(expr, sel),
                    )
                    |> State.from_result,
            ),
    )

selection_list : List (Selection a err) -> Selection (List a) err
selection_list = |sels| @Selection(selection_list_help(sels))

selection_list_help = |sels|
    State.get
    |> State.bind(
        |env|
            sels
            |> List.map_try(|@Selection(sel)| State.attempt(sel, env) |> Result.map_ok(.0))
            |> Result.map_ok(
                |all_sels|
                    decode = |cells|
                        all_sels
                        |> List.walk_try((cells, List.with_capacity(List.len(sels))), decode_sel)
                        |> Result.map_ok(.1)

                    decode_sel = |(remaining_cells, decoded_sels), sel|
                        { before, others } = List.split_at(remaining_cells, List.len(sel.columns))

                        sel.decode(before)
                        |> Result.map_ok(
                            |decoded|
                                (others, List.append(decoded_sels, decoded)),
                        )

                    {
                        columns: all_sels |> List.join_map(.columns),
                        decode,
                    },
            )
            |> State.from_result,
    )

map : Selection a err, (a -> b) -> Selection b err
map = |ss, fn|
    update_selection(
        ss,
        |sel| {
            columns: sel.columns,
            decode: |cells| Result.map_ok(sel.decode(cells), fn),
        },
    )

# Expr

Expr pg roc := {
    sql : Sql,
    decode : Decode pg roc,
}

NullableExpr pg roc : Expr (Nullable pg) (Nullable roc)

identifier : Str, Str, Decode pg roc -> Expr pg roc
identifier = |table, col, decode|
    @Expr(
        {
            sql: [Raw("${table}.${col}")],
            decode,
        },
    )

## Discards the phantom type variable that represents the SQL-level type.
##
## This should be used as a last resort, when the query builder is preventing you
## from writing an expression that you know it's valid.
##
## IMPORTANT: The compiled expression won't include a SQL cast!
discard_phantom : Expr * roc -> Expr * roc
discard_phantom = |@Expr(expr)|
    @Expr(
        {
            sql: expr.sql,
            decode: Sql.Types.discard_phantom(expr.decode),
        },
    )

# Expr: Literals

i16 : I16 -> Expr (PgI16 *) I16
i16 = |value|
    @Expr(
        {
            sql: [Param(Pg.Cmd.i16(value))],
            decode: Sql.Types.i16,
        },
    )

i32 : I32 -> Expr (PgI32 *) I32
i32 = |value|
    @Expr(
        {
            sql: [Param(Pg.Cmd.i32(value))],
            decode: Sql.Types.i32,
        },
    )

i64 : I64 -> Expr (PgI64 *) I64
i64 = |value|
    @Expr(
        {
            sql: [Param(Pg.Cmd.i64(value))],
            decode: Sql.Types.i64,
        },
    )

str : Str -> Expr (PgText *) Str
str = |value|
    @Expr(
        {
            sql: [Param(Pg.Cmd.str(value))],
            decode: Sql.Types.str,
        },
    )

null : NullableExpr _ []
null = @Expr(
    {
        sql: [Raw("null")],
        decode: Sql.Types.nullable(Sql.Types.fail("not null")),
    },
)

as_nullable : Expr pg roc -> NullableExpr pg roc
as_nullable = |@Expr(a)|
    @Expr(
        {
            sql: a.sql,
            decode: Sql.Types.nullable(a.decode),
        },
    )

# Expr: Comparison

is_null : NullableExpr _ _ -> Expr (PgBool _) Bool
is_null = |@Expr(a)|
    @Expr(
        {
            sql: a.sql |> List.append(Raw(" is null")),
            decode: Sql.Types.bool,
        },
    )

is_not_null : NullableExpr _ _ -> Expr (PgBool _) Bool
is_not_null = |@Expr(a)|
    @Expr(
        {
            sql: a.sql |> List.append(Raw(" is not null")),
            decode: Sql.Types.bool,
        },
    )

is_distinct_from : NullableExpr (PgCmp a) *, NullableExpr (PgCmp a) * -> Expr (PgBool *) Bool
is_distinct_from = |a, b| bool_op(a, "is distinct from", b)

is_not_distinct_from : NullableExpr (PgCmp a) *, NullableExpr (PgCmp a) * -> Expr (PgBool *) Bool
is_not_distinct_from = |a, b| bool_op(a, "is not distinct from", b)

nullable_eq = is_not_distinct_from

nullable_neq = is_distinct_from

concat : Expr (PgText *) *, Expr (PgText *) * -> Expr (PgText *) Str
concat = |@Expr(a), @Expr(b)|
    @Expr(
        {
            sql: bin_op(a.sql, "||", b.sql),
            decode: Sql.Types.str,
        },
    )

lt : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr (PgBool *) Bool
lt = |a, b| bool_op(a, "<", b)

lte : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr (PgBool *) Bool
lte = |a, b| bool_op(a, "<=", b)

eq : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr (PgBool *) Bool
eq = |a, b| bool_op(a, "=", b)

neq : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr (PgBool *) Bool
neq = |a, b| bool_op(a, "<>", b)

gte : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr (PgBool *) Bool
gte = |a, b| bool_op(a, ">=", b)

gt : Expr (PgCmp a) *, Expr (PgCmp a) * -> Expr (PgBool *) Bool
gt = |a, b| bool_op(a, ">", b)

in : Expr (PgCmp a) *, (item -> Expr (PgCmp a) *), List item -> Expr (PgBool *) Bool
in = |@Expr(needle), to_expr, haystack|
    item_to_sql = |item|
        @Expr({ sql }) = to_expr(item)
        sql

    list_items =
        haystack
        |> List.map(item_to_sql)
        |> comma_join

    in_sql =
        needle.sql
        |> List.reserve((List.len(list_items) + 2))
        |> List.append(Raw(" in ("))
        |> List.concat(list_items)
        |> List.append(Raw(")"))

    @Expr(
        {
            sql: in_sql,
            decode: Sql.Types.bool,
        },
    )

like : Expr (PgText *) *, Expr (PgText *) * -> Expr (PgBool *) Bool
like = |a, b|
    bool_op(a, "like", b)

ilike : Expr (PgText *) *, Expr (PgText *) * -> Expr (PgBool *) Bool
ilike = |a, b|
    bool_op(a, "ilike", b)

bool_op : Expr * *, Str, Expr * * -> Expr (PgBool *) Bool
bool_op = |@Expr(a), op, @Expr(b)|
    @Expr(
        {
            sql: bin_op(a.sql, op, b.sql),
            decode: Sql.Types.bool,
        },
    )

# Expr: Logical
# TODO: Test precendence is ok!

and_ : Expr (PgBool *) *, Expr (PgBool *) * -> Expr (PgBool *) Bool
and_ = |a, b| bool_op(a, "and", b)

or_ : Expr (PgBool *) *, Expr (PgBool a) * -> Expr (PgBool *) Bool
or_ = |@Expr(a), @Expr(b)|
    @Expr(
        {
            sql: bin_op_parens(a.sql, "or", b.sql),
            decode: Sql.Types.bool,
        },
    )

not_ : Expr (PgBool *) * -> Expr (PgBool *) Bool
not_ = |@Expr(a)|
    @Expr(
        {
            sql: [Raw("not (")]
            |> List.reserve((List.len(a.sql) + 1))
            |> List.concat(a.sql)
            |> List.append(Raw(")")),
            decode: Sql.Types.bool,
        },
    )

and_list : List (Expr (PgBool _) Bool) -> Expr (PgBool _) Bool
and_list = |exprs|
    join_bool(open_true, and_, exprs)

or_list : List (Expr (PgBool _) Bool) -> Expr (PgBool _) Bool
or_list = |exprs|
    join_bool(open_false, or_, exprs)

BoolOp a : Expr (PgBool a) Bool, Expr (PgBool a) Bool -> Expr (PgBool a) Bool

join_bool : Expr (PgBool _) Bool, BoolOp _, List (Expr (PgBool _) Bool) -> Expr (PgBool _) Bool
join_bool = |default, operator, exprs|
    # We could simplify this by using default as the initial value,
    # but in most cases we would produce something like (true and ...),
    # which is not very nice to read.
    result =
        List.walk(
            exprs,
            Empty,
            |acc, rhs|
                when acc is
                    Empty ->
                        NotEmpty(rhs)

                    NotEmpty(lhs) ->
                        NotEmpty(operator(lhs, rhs)),
        )

    when result is
        Empty ->
            default

        NotEmpty(expr) ->
            expr

true : Expr (PgBool _) Bool
true = @Expr(
    {
        sql: [Raw("true")],
        decode: Sql.Types.bool,
    },
)

false : Expr (PgBool _) Bool
false = @Expr(
    {
        sql: [Raw("false")],
        decode: Sql.Types.bool,
    },
)

# These are needed for `andList` and `orList`

open_true = @Expr(
    {
        sql: [Raw("true")],
        decode: Sql.Types.bool,
    },
)

open_false = @Expr(
    {
        sql: [Raw("false")],
        decode: Sql.Types.bool,
    },
)

# Expr: Num

add : Expr (PgNum pg) roc, Expr (PgNum pg) roc -> Expr (PgNum pg) roc
add = |@Expr(a), @Expr(b)|
    @Expr(
        {
            sql: bin_op_parens(a.sql, "+", b.sql),
            decode: a.decode,
        },
    )

sub : Expr (PgNum pg) roc, Expr (PgNum pg) roc -> Expr (PgNum pg) roc
sub = |@Expr(a), @Expr(b)|
    @Expr(
        {
            sql: bin_op_parens(a.sql, "-", b.sql),
            decode: a.decode,
        },
    )

mul : Expr (PgNum pg) roc, Expr (PgNum pg) roc -> Expr (PgNum pg) roc
mul = |@Expr(a), @Expr(b)|
    @Expr(
        {
            sql: bin_op(a.sql, "*", b.sql),
            decode: a.decode,
        },
    )

div : Expr (PgNum pg) roc, Expr (PgNum pg) roc -> Expr (PgNum pg) roc
div = |@Expr(a), @Expr(b)|
    @Expr(
        {
            sql: bin_op(a.sql, "/", b.sql),
            decode: a.decode,
        },
    )

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

join_with : List Sql, Sql -> Sql
join_with = |items, sep|
    items
    |> List.intersperse(sep)
    |> List.join

comma_join : List Sql -> Sql
comma_join = |items|
    join_with(items, [Raw(", ")])

bin_op = |a, op, b|
    a
    |> List.reserve((List.len(b) + 1))
    |> List.append(Raw(" ${op} "))
    |> List.concat(b)

bin_op_parens = |a, op, b|
    [Raw("(")]
    |> List.reserve((List.len(a) + List.len(b) + 2))
    |> List.concat(a)
    |> List.append(Raw(" ${op} "))
    |> List.concat(b)
    |> List.append(Raw(")"))

compile_sql : Sql -> Compiled
compile_sql = |sql|
    sql
    |> List.walk(
        {
            params: List.with_capacity(8),
            sql: Str.with_capacity((List.len(sql) * 12)),
        },
        add_part,
    )

add_part : Compiled, Part -> Compiled
add_part = |{ params, sql }, part|
    when part is
        Param(param) ->
            param_count = List.len(params)

            { new_params, index } =
                # TODO: Use a dict
                index_result = params |> List.find_first_index(|p| p == param)

                when index_result is
                    Ok(existing_index) ->
                        {
                            new_params: params,
                            index: existing_index,
                        }

                    Err(NotFound) ->
                        {
                            new_params: params |> List.append(param),
                            index: param_count,
                        }

            binding = Num.to_str((index + 1))

            { sql: "${sql}$${binding}", params: new_params }

        Raw(raw) ->
            { sql: Str.concat(sql, raw), params }

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

