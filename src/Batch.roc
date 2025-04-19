module [
    Batch,
    BatchedCmd,
    succeed,
    with,
    params,
    reuse_name,
    sequence,
]

import Cmd exposing [Cmd]
import Pg.Result exposing [CmdResult]

Batch a err :=
    Params {
        decode : List CmdResult
        ->
        Result
            {
                value : a,
                rest : List CmdResult,
            }
            [
                MissingCmdResult U64,
                ExpectErr err,
            ],
    }

Params a : {
    commands : List BatchedCmd,
    seen_sql : SeenSql,
}a

SeenSql : Dict Str { index : U64, reused : Bool }

BatchedCmd : Cmd.Params {} [ReuseSql U64]

reuse_name : U64 -> Str
reuse_name = |index|
    index_str = Num.to_str(index)
    "b[${index_str}]"

succeed : _ -> Batch _ _
succeed = |value|
    @Batch(
        {
            commands: List.with_capacity(5),
            seen_sql: Dict.with_capacity(5),
            decode: |rest| Ok({ value, rest }),
        },
    )

with : Batch (a -> b) err, Cmd a err -> Batch b err
with = |@Batch(batch), cmd|
    { seen_sql, new_cmd, new_index } = add_cmd(batch, cmd)
    commands = batch.commands |> List.append(new_cmd)

    decode = |results|
        batch.decode(results)
        |> Result.try(
            |{ value: fn, rest }|
                when rest is
                    [next, ..] ->
                        Cmd.decode(next, cmd)
                        |> Result.map_err(ExpectErr)
                        |> Result.try(
                            |a|
                                Ok(
                                    {
                                        value: fn(a),
                                        rest: List.drop_first(rest, 1),
                                    },
                                ),
                        )

                    _ ->
                        Err(MissingCmdResult(new_index)),
        )

    @Batch({ commands, seen_sql, decode })

sequence : List (Cmd a err) -> Batch (List a) err
sequence = |cmds|
    count = List.len(cmds)

    init = {
        commands: List.with_capacity(count),
        seen_sql: Dict.with_capacity(smallest(10, count)),
    }

    batch =
        cmds
        |> List.walk(
            init,
            |b, cmd|
                { seen_sql, new_cmd } = add_cmd(b, cmd)

                {
                    seen_sql,
                    commands: b.commands |> List.append(new_cmd),
                },
        )

    decode = |results|
        List.map2(results, cmds, Cmd.decode)
        |> List.map_try(|r| r)
        |> Result.map_ok(|value| { value, rest: [] })
        |> Result.map_err(ExpectErr)

    @Batch(
        {
            commands: batch.commands,
            seen_sql: batch.seen_sql,
            decode,
        },
    )

smallest : Num a, Num a -> Num a
smallest = |a, b|
    if a < b then
        a
    else
        b

add_cmd : Params *, Cmd * * -> { seen_sql : SeenSql, new_cmd : BatchedCmd, new_index : U64 }
add_cmd = |batch, cmd|
    cmd_params = Cmd.params(cmd)
    new_index = List.len(batch.commands)

    when cmd_params.kind is
        SqlCmd(sql) ->
            when Dict.get(batch.seen_sql, sql) is
                Err(KeyNotFound) ->
                    entry = {
                        index: new_index,
                        reused: Bool.false,
                    }
                    seen_sql = batch.seen_sql |> Dict.insert(sql, entry)
                    new_cmd = SqlCmd(sql) |> batched_cmd(cmd_params)

                    { seen_sql, new_cmd, new_index }

                Ok({ index, reused }) ->
                    seen_sql =
                        if reused then
                            batch.seen_sql
                        else
                            entry = { index, reused: Bool.true }
                            batch.seen_sql |> Dict.insert(sql, entry)

                    new_cmd = ReuseSql(index) |> batched_cmd(cmd_params)

                    { seen_sql, new_cmd, new_index }

        PreparedCmd(prep) ->
            new_cmd = PreparedCmd(prep) |> batched_cmd(cmd_params)

            { seen_sql: batch.seen_sql, new_cmd, new_index }

batched_cmd : Cmd.Kind [ReuseSql U64], Cmd.Params {} [] -> BatchedCmd
batched_cmd = |kind, cmd_params| {
    kind,
    bindings: cmd_params.bindings,
    limit: cmd_params.limit,
}

params : Batch a err -> Params _
params = |@Batch(batch)| batch
