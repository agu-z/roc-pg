interface Cmd exposes
    [
        Cmd,
        fromSql,
        prepared,
        unwrap,
        withLimit,
        decode,
        withDecode,
        map,
        bind,
        Binding,
        makeBinding,
        encodeBindings,
    ] imports [
        Protocol.Frontend.{ FormatCode },
        Protocol.Backend.{ RowField },
        Pg.Result.{ CmdResult },
    ]

Cmd a err := {
    kind : Kind,
    bindings : List Binding,
    limit : [None, Limit I32],
    decode : CmdResult -> Result a err,
}

Kind : [
    SqlCmd Str,
    PreparedCmd
        {
            name : Str,
            fields : List RowField,
        },
]

fromSql : Str -> Cmd CmdResult []
fromSql = \sql ->
    new (SqlCmd sql)

prepared : { name : Str, fields : List RowField } -> Cmd CmdResult []
prepared = \prep ->
    new (PreparedCmd prep)

new : Kind -> Cmd CmdResult []
new = \kind ->
    @Cmd {
        kind,
        limit: None,
        bindings: [],
        decode: Ok,
    }

unwrap : Cmd * * -> { kind : Kind, limit : [None, Limit I32] }
unwrap = \@Cmd { kind, limit } ->
    { kind, limit }

withLimit : Cmd a err, I32 -> Cmd a err
withLimit = \@Cmd cmd, limit ->
    @Cmd { cmd & limit: Limit limit }

decode : CmdResult, Cmd a err -> Result a err
decode = \r, @Cmd cmd ->
    cmd.decode r

withDecode : Cmd CmdResult [], (CmdResult -> Result a err) -> Cmd a err
withDecode = \@Cmd cmd, fn ->
    @Cmd {
        kind: cmd.kind,
        limit: cmd.limit,
        bindings: cmd.bindings,
        decode: fn,
    }

map : Cmd a err, (a -> b) -> Cmd b err
map = \@Cmd cmd, fn ->
    @Cmd {
        kind: cmd.kind,
        limit: cmd.limit,
        bindings: cmd.bindings,
        decode: \r -> cmd.decode r |> Result.map fn,
    }

bind : Cmd a err, List Binding -> Cmd a err
bind = \@Cmd cmd, bindings ->
    @Cmd { cmd & bindings }

Binding := [
    Null,
    Text Str,
    Binary (List U8),
]

makeBinding = @Binding

encodeBindings : Cmd a err
    -> {
        formatCodes : List FormatCode,
        paramValues : List [Null, Value (List U8)],
    }
encodeBindings = \@Cmd { bindings } ->
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

