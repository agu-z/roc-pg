interface Pg.Result
    exposes [
        QueryResult,
        create,
        len,
        decode,
        str,
        i32,
        apply,
        succeed,
    ]
    imports [Protocol.Backend]

QueryResult := {
    fields : List Protocol.Backend.RowField,
    rows : List (List (List U8)),
}

create = @QueryResult

len : QueryResult -> Nat
len = \@QueryResult result ->
    List.len result.rows

Decode a err := List Protocol.Backend.RowField -> Result (List (List U8) -> Result a [FieldNotFound]err) [FieldNotFound]

decode : QueryResult, Decode a err -> Result (List a) [FieldNotFound]err
decode = \@QueryResult r, @Decode getDecode ->
    when getDecode r.fields is
        Ok fn ->
            List.mapTry r.rows fn

        Err FieldNotFound ->
            Err FieldNotFound

str = decoder Ok

i32 = decoder Str.toI32

decoder = \fn -> \name ->
        fields <- @Decode

        when List.findFirstIndex fields \f -> f.name == name is
            Ok index ->
                row <- Ok

                when List.get row index is
                    Ok bytes ->
                        when Str.fromUtf8 bytes is
                            Ok strValue ->
                                fn strValue

                            Err err ->
                                Err err

                    Err OutOfBounds ->
                        Err FieldNotFound

            Err NotFound ->
                Err FieldNotFound

map2 = \@Decode a, @Decode b, cb ->
    fields <- @Decode

    decodeA <- Result.try (a fields)
    decodeB <- Result.try (b fields)

    row <- Ok

    valueA <- Result.try (decodeA row)
    valueB <- Result.try (decodeB row)

    Ok (cb valueA valueB)

succeed = \value ->
    @Decode (\_ -> Ok (\_ -> Ok value))

apply = \a, b -> map2 a b (\fn, val -> fn val)
