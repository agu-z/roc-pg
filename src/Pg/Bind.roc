interface Pg.Bind
    exposes [
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
        encode,
    ]
    imports [
        Protocol.Frontend.{ FormatCode },
    ]

Binding := [
    Null,
    Text Str,
    Binary (List U8),
]

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

bytes : List U8 -> Binding
bytes = \value ->
    @Binding (Binary value)

encode : List Binding
    -> {
        formatCodes : List FormatCode,
        paramValues : List [Null, Value (List U8)],
    }
encode = \bindings ->
    empty = {
        formatCodes: [],
        paramValues: [],
    }

    List.walk bindings empty \state, binding ->
        { format, value } = encodeSingle binding

        {
            formatCodes: state.formatCodes |> List.append format,
            paramValues: state.paramValues |> List.append value,
        }

encodeSingle = \@Binding binding ->
    when binding is
        Null ->
            {
                value: Null,
                format: Binary,
            }

        Binary value ->
            {
                value: Value value,
                format: Binary,
            }

        Text value ->
            {
                value: Value (Str.toUtf8 value),
                format: Text,
            }

