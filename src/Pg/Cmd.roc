interface Pg.Cmd
    exposes [
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
    ]
    imports [
        Cmd.{ makeBinding },
        pg.Pg.Result.{ CmdResult },
    ]

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

# Bindings

Binding : Cmd.Binding

bind : Cmd a err, List Binding -> Cmd a err
bind = Cmd.bind

null : Binding
null = makeBinding Null

str : Str -> Binding
str = \value ->
    makeBinding (Text value)

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
    makeBinding (Text (Num.toStr value))

bytes : List U8 -> Binding
bytes = \value ->
    makeBinding (Binary value)

