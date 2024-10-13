module [
    Cmd,
    new,
    expectN,
    expect1,
    map,
    bind,
    Binding,
    null,
    str,
    u8,
    u16,
    u32,
    u64,
    u128,
    i8,
    i16,
    i32,
    i64,
    i128,
    f32,
    f64,
    bytes,
    bool,
    withCustomDecode,
    inspect,
]

import Cmd
import Pg.Result exposing [CmdResult]

Cmd a err : Cmd.Cmd a err

new : Str -> Cmd CmdResult []
new = Cmd.fromSql

# Result

expectN : Cmd CmdResult [], Pg.Result.Decode a err -> Cmd (List a) [FieldNotFound Str]err
expectN = \cmd, decoder ->
    Cmd.withDecode cmd \r -> Pg.Result.decode r decoder

expect1 : Cmd CmdResult [], Pg.Result.Decode a [EmptyResult]err -> Cmd a [EmptyResult, FieldNotFound Str]err
expect1 = \cmd, decoder ->
    cmdResult <- cmd |> Cmd.withLimit 1 |> Cmd.withDecode
    rows <- Pg.Result.decode cmdResult decoder |> Result.try

    when rows is
        [row] ->
            Ok row

        _ ->
            Err EmptyResult

map : Cmd a err, (a -> b) -> Cmd b err
map = Cmd.map

withCustomDecode : Cmd * *, (CmdResult -> Result a err) -> Cmd a err
withCustomDecode = Cmd.withDecode

inspect : Cmd a err -> Str
inspect = \cmd ->
    { kind, bindings } = Cmd.params cmd

    kindStr = inspectKind kind
    bindingsStr =
        bindings
        |> List.mapWithIndex
            \val, index ->
                n = index + 1
                "$$(Num.toStr n) = $(inspectBinding val)"
        |> Str.joinWith "\n"

    "$(kindStr)\n$(bindingsStr)"

inspectKind = \kind ->
    when kind is
        SqlCmd sql ->
            "SQL: $(sql)"

        PreparedCmd { name } ->
            "Prepared: $(name)"

inspectBinding = \binding ->
    when binding is
        Null ->
            "NULL"

        Text text ->
            text

        Binary bin ->
            bin
            |> List.map Num.toStr
            |> Str.joinWith ","

# Bindings

Binding := Cmd.Binding implements [Eq]

bind : Cmd a err, List Binding -> Cmd a err
bind = \cmd, bindings ->
    Cmd.bind cmd (bindings |> List.map \@Binding binding -> binding)

null : Binding
null = @Binding Null

str : Str -> Binding
str = \value ->
    @Binding (Text value)

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
num = \value ->
    @Binding (Text (Num.toStr value))

bool : Bool -> Binding
bool = \value ->
    @Binding (Binary [(if value then 1 else 0)])

bytes : List U8 -> Binding
bytes = \value ->
    @Binding (Binary value)
