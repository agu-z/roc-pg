module [
    Batch,
    BatchedCmd,
    succeed,
    with,
    params,
    reuseName,
    sequence,
]

import Cmd exposing [Cmd]
import Pg.Result exposing [CmdResult]

Batch a err := Params {
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
    seenSql : SeenSql,
}a

SeenSql : Dict Str { index : U64, reused : Bool }

BatchedCmd : Cmd.Params {} [ReuseSql U64]

reuseName : U64 -> Str
reuseName = \index ->
    indexStr = Num.toStr index
    "b[$(indexStr)]"

succeed : a -> Batch a err
succeed = \value ->
    @Batch {
        commands: List.withCapacity 5,
        seenSql: Dict.withCapacity 5,
        decode: \rest -> Ok { value, rest },
    }

with : Batch (a -> b) err, Cmd a err -> Batch b err
with = \@Batch batch, cmd ->
    { seenSql, newCmd, newIndex } = addCmd batch cmd
    commands = batch.commands |> List.append newCmd

    decode = \results ->
        { value: fn, rest } = batch.decode? results

        when rest is
            [next, ..] ->
                a = Cmd.decode next cmd |> Result.mapErr? ExpectErr

                Ok {
                    value: fn a,
                    rest: List.dropFirst rest 1,
                }

            _ ->
                Err (MissingCmdResult newIndex)

    @Batch { commands, seenSql, decode }

sequence : List (Cmd a err) -> Batch (List a) err
sequence = \cmds ->
    count = List.len cmds

    init = {
        commands: List.withCapacity count,
        seenSql: Dict.withCapacity (smallest 10 count),
    }

    batch =
        cmds
        |> List.walk init \b, cmd ->
            { seenSql, newCmd } = addCmd b cmd

            {
                seenSql,
                commands: b.commands |> List.append newCmd,
            }

    decode = \results ->
        List.map2 results cmds Cmd.decode
        |> List.mapTry \r -> r
        |> Result.map \value -> { value, rest: [] }
        |> Result.mapErr ExpectErr

    @Batch {
        commands: batch.commands,
        seenSql: batch.seenSql,
        decode,
    }

smallest : Num a, Num a -> Num a
smallest = \a, b ->
    if a < b then
        a
    else
        b

addCmd : Params *, Cmd * * -> { seenSql : SeenSql, newCmd : BatchedCmd, newIndex : U64 }
addCmd = \batch, cmd ->
    cmdParams = Cmd.params cmd
    newIndex = List.len batch.commands

    when cmdParams.kind is
        SqlCmd sql ->
            when Dict.get batch.seenSql sql is
                Err KeyNotFound ->
                    entry = {
                        index: newIndex,
                        reused: Bool.false,
                    }
                    seenSql = batch.seenSql |> Dict.insert sql entry
                    newCmd = SqlCmd sql |> batchedCmd cmdParams

                    { seenSql, newCmd, newIndex }

                Ok { index, reused } ->
                    seenSql =
                        if reused then
                            batch.seenSql
                        else
                            entry = { index, reused: Bool.true }
                            batch.seenSql |> Dict.insert sql entry

                    newCmd = ReuseSql index |> batchedCmd cmdParams

                    { seenSql, newCmd, newIndex }

        PreparedCmd prep ->
            newCmd = PreparedCmd prep |> batchedCmd cmdParams

            { seenSql: batch.seenSql, newCmd, newIndex }

batchedCmd : Cmd.Kind [ReuseSql U64], Cmd.Params {} [] -> BatchedCmd
batchedCmd = \kind, cmdParams -> {
    kind,
    bindings: cmdParams.bindings,
    limit: cmdParams.limit,
}

params : Batch a err -> Params _
params = \@Batch batch -> batch
