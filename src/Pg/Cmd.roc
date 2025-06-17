module [
    Binding,
    Cmd,
    bind,
    bool,
    bytes,
    expect1,
    expect_n,
    f32,
    f64,
    i128,
    i16,
    i32,
    i64,
    i8,
    inspect,
    map,
    new,
    null,
    str,
    u128,
    u16,
    u32,
    u64,
    u8,
    with_custom_decode,
]

import Cmd
import Pg.Result exposing [CmdResult]

Cmd a err : Cmd.Cmd a err

new : Str -> Cmd CmdResult []
new = Cmd.from_sql

# Result

expect_n : Cmd CmdResult [], Pg.Result.Decode a err -> Cmd (List a) [FieldNotFound Str]err
expect_n = |cmd, decoder|
    Cmd.with_decode(cmd, |r| Pg.Result.decode(r, decoder))

expect1 : Cmd CmdResult [], Pg.Result.Decode a [EmptyResult]err -> Cmd a [EmptyResult, FieldNotFound Str]err
expect1 = |cmd, decoder|
    cmd
    |> Cmd.with_limit(1)
    |> Cmd.with_decode(
        |cmd_result|
            Pg.Result.decode(cmd_result, decoder)
            |> Result.try(
                |rows|
                    when rows is
                        [row] ->
                            Ok(row)

                        _ ->
                            Err(EmptyResult),
            ),
    )

map : Cmd _ _, (_ -> _) -> Cmd _ _
map = Cmd.map

with_custom_decode : Cmd _ _, (CmdResult -> Result _ _) -> Cmd _ _
with_custom_decode = Cmd.with_decode

inspect : Cmd a err -> Str
inspect = |cmd|
    { kind, bindings } = Cmd.params(cmd)

    kind_str = inspect_kind(kind)
    bindings_str =
        bindings
        |> List.map_with_index(
            |val, index|
                n = index + 1
                "$${Num.to_str(n)} = ${inspect_binding(val)}",
        )
        |> Str.join_with("\n")

    "${kind_str}\n${bindings_str}"

inspect_kind = |kind|
    when kind is
        SqlCmd(sql) ->
            "SQL: ${sql}"

        PreparedCmd({ name }) ->
            "Prepared: ${name}"

inspect_binding = |binding|
    when binding is
        Null ->
            "NULL"

        Text(text) ->
            text

        Binary(bin) ->
            bin
            |> List.map(Num.to_str)
            |> Str.join_with(",")

# Bindings

Binding := Cmd.Binding implements [Eq]

bind : Cmd a err, List Binding -> Cmd a err
bind = |cmd, bindings|
    Cmd.bind(cmd, (bindings |> List.map(|@Binding(binding)| binding)))

null : Binding
null = @Binding(Null)

str : Str -> Binding
str = |value|
    @Binding(Text(value))

u8 = num
u16 = num
u32 = num
u64 = num
u128 = num
i8 = num
i16 = num
i32 = num
i64 = num
i128 = num
f32 = num
f64 = num

num : Num * -> Binding
num = |value|
    @Binding(Text(Num.to_str(value)))

bool : Bool -> Binding
bool = |value|
    @Binding(Binary([(if value then 1 else 0)]))

bytes : List U8 -> Binding
bytes = |value|
    @Binding(Binary(value))
