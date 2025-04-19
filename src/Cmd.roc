module [
    Cmd,
    Params,
    Limit,
    Kind,
    from_sql,
    prepared,
    params,
    with_limit,
    decode,
    with_decode,
    map,
    bind,
    Binding,
    encode_bindings,
]

import Protocol.Frontend exposing [FormatCode]
import Protocol.Backend exposing [RowField, ParameterField]
import Pg.Result exposing [CmdResult]

Cmd a err := Params { decode : CmdResult -> Result a err } []

Limit : [None, Limit I32]

Params p k : {
    kind : Kind k,
    bindings : List Binding,
    limit : Limit,
}p

Kind k : [
    SqlCmd Str,
    PreparedCmd
        {
            name : Str,
            fields : List RowField,
            parameters : List ParameterField,
        },
]k

from_sql : Str -> Cmd CmdResult []
from_sql = |sql|
    new(SqlCmd(sql))

prepared : { name : Str, fields : List RowField, parameters : List ParameterField } -> Cmd CmdResult []
prepared = |prep|
    new(PreparedCmd(prep))

new : Kind [] -> Cmd CmdResult []
new = |kind|
    @Cmd(
        {
            kind,
            limit: None,
            bindings: [],
            decode: Ok,
        },
    )

params : Cmd a err -> Params {} []
params = |@Cmd({ kind, bindings, limit })|
    { kind, bindings, limit }

with_limit : Cmd a err, I32 -> Cmd a err
with_limit = |@Cmd(cmd), limit|
    @Cmd({ cmd & limit: Limit(limit) })

decode : CmdResult, Cmd a err -> Result a err
decode = |r, @Cmd(cmd)|
    cmd.decode(r)

with_decode : Cmd * *, (CmdResult -> Result a err) -> Cmd a err
with_decode = |@Cmd(cmd), fn|
    @Cmd(
        {
            kind: cmd.kind,
            limit: cmd.limit,
            bindings: cmd.bindings,
            decode: fn,
        },
    )

map : Cmd a err, (a -> b) -> Cmd b err
map = |@Cmd(cmd), fn|
    @Cmd(
        {
            kind: cmd.kind,
            limit: cmd.limit,
            bindings: cmd.bindings,
            decode: |r| cmd.decode(r) |> Result.map_ok(fn),
        },
    )

bind : Cmd a err, List Binding -> Cmd a err
bind = |@Cmd(cmd), bindings|
    @Cmd({ cmd & bindings })

Binding : [
    Null,
    Text Str,
    Binary (List U8),
]

encode_bindings :
    List Binding
    -> {
        format_codes : List FormatCode,
        param_values : List [Null, Value (List U8)],
    }
encode_bindings = |bindings|
    count = List.len(bindings)

    empty = {
        format_codes: List.with_capacity(count),
        param_values: List.with_capacity(count),
    }

    List.walk(
        bindings,
        empty,
        |state, binding|
            { format, value } = encode_single(binding)

            {
                format_codes: state.format_codes |> List.append(format),
                param_values: state.param_values |> List.append(value),
            },
    )

encode_single = |binding|
    when binding is
        Null ->
            {
                value: Null,
                format: Binary,
            }

        Binary(value) ->
            {
                value: Value(value),
                format: Binary,
            }

        Text(value) ->
            {
                value: Value(Str.to_utf8(value)),
                format: Text,
            }

