interface Batch
    exposes [
        Batch,
        BatchedCmd,
        succeed,
        with,
        params,
        reuseName,
        sequence,
    ]
    imports [
        Cmd.{ Cmd },
        Pg.Result.{ CmdResult },
    ]

Batch a err := {
    commands : List BatchedCmd,
    seenSql : Dict Str { index : Nat, reused : Bool },
    decode : List CmdResult
    ->
    Result
        {
            value : a,
            rest : List CmdResult,
        }
        [
            MissingCmdResult,
            ExpectErr err,
        ],
}

BatchedCmd : Cmd.Params {} [ReuseSql Nat]

succeed : a -> Batch a err
succeed = \value ->
    @Batch {
        commands: List.withCapacity 5,
        seenSql: Dict.withCapacity 5,
        decode: \rest -> Ok { value, rest },
    }

with : Batch (a -> b) err, Cmd a err -> Batch b err
with = \@Batch batch, cmd ->
    { seenSql, newCmd } = addCmd batch cmd
    commands = batch.commands |> List.append newCmd

    decode = \results ->
        { value: fn, rest } <- batch.decode results |> Result.try

        when rest is
            [next, ..] ->
                a <- Cmd.decode next cmd
                    |> Result.mapErr ExpectErr
                    |> Result.try

                Ok {
                    value: fn a,
                    rest: List.dropFirst rest,
                }

            _ ->
                Err MissingCmdResult

    @Batch { commands, seenSql, decode }

addCmd = \batch, cmd ->
    cmdParams = Cmd.params cmd

    when cmdParams.kind is
        SqlCmd sql ->
            when Dict.get batch.seenSql sql is
                Err KeyNotFound ->
                    entry = {
                        index: List.len batch.commands,
                        reused: Bool.false,
                    }
                    seenSql = batch.seenSql |> Dict.insert sql entry
                    newCmd = SqlCmd sql |> batchCmd cmdParams

                    { seenSql, newCmd }

                Ok { index, reused } ->
                    seenSql =
                        if reused then
                            batch.seenSql
                        else
                            entry = { index, reused: Bool.true }
                            batch.seenSql |> Dict.insert sql entry

                    newCmd = ReuseSql index |> batchCmd cmdParams

                    { seenSql, newCmd }

        PreparedCmd prep ->
            newCmd = PreparedCmd prep |> batchCmd cmdParams

            { seenSql: batch.seenSql, newCmd }

batchCmd = \kind, cmdParams -> {
    kind,
    bindings: cmdParams.bindings,
    limit: cmdParams.limit,
}

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

smallest = \a, b ->
    if a < b then
        a
    else
        b

params = \@Batch batch -> batch

reuseName : Nat -> Str
reuseName = \index ->
    indexStr = Num.toStr index
    "b[\(indexStr)]"
