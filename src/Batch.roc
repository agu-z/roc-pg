interface Batch
    exposes [Batch, succeed, with, unwrap]
    imports [
        Cmd.{ Cmd },
        Pg.Result.{ CmdResult },
    ]

Batch a err := {
    commands : List (Cmd CmdResult []),
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

succeed : a -> Batch a err
succeed = \value ->
    @Batch {
        commands: List.withCapacity 5,
        decode: \rest -> Ok { value, rest },
    }

with : Batch (a -> b) err, Cmd a err -> Batch b err
with = \@Batch batch, cmd ->
    commands = batch.commands |> List.append (eraseDecode cmd)

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

    @Batch { commands, decode }

eraseDecode = \cmd -> Cmd.withDecode cmd Ok

unwrap = \@Batch batch -> batch
