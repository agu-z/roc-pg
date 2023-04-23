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
        nat,
        bytes,
    ]
    imports [
        Cmd,
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

Binding := Cmd.Binding has [Eq]

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
nat = num

num : Num * -> Binding
num = \value ->
    @Binding (Text (Num.toStr value))

bytes : List U8 -> Binding
bytes = \value ->
    @Binding (Binary value)

