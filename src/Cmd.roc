interface Cmd exposes
    [
        Cmd,
        getState,
        unprepared,
        prepared,
        bind,
        Binding,
        makeBinding,
        encodeBindings,
    ] imports [
        Protocol.Frontend.{ FormatCode },
        Protocol.Backend.{ RowField },
    ]

Cmd := {
    state : State,
    bindings : List Binding,
}

State : [
    Unprepared Str,
    Prepared
        {
            name : Str,
            fields : List RowField,
        },
]

getState : Cmd -> State
getState = \@Cmd { state } ->
    state

bind : Cmd, List Binding -> Cmd
bind = \@Cmd { state }, bindings ->
    @Cmd { state, bindings }

unprepared : Str -> Cmd
unprepared = \sql ->
    @Cmd { state: Unprepared sql, bindings: [] }

prepared : { name : Str, fields : List RowField } -> Cmd
prepared = \prep ->
    @Cmd { state: Prepared prep, bindings: [] }

# Bindings

Binding := [
    Null,
    Text Str,
    Binary (List U8),
]

makeBinding = @Binding

encodeBindings : Cmd
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

