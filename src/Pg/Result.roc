interface Pg.Result
    exposes [
        CmdResult,
        RowField,
        create,
        len,
        fields,
        decode,
        Decode,
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
        dec,
        nat,
        with,
        apply,
        succeed,
    ]
    imports [Protocol.Backend]

RowField : Protocol.Backend.RowField

CmdResult := {
    fields : List RowField,
    rows : List (List (List U8)),
}

create = @CmdResult

fields : CmdResult -> List RowField
fields = \@CmdResult result -> result.fields

len : CmdResult -> Nat
len = \@CmdResult result ->
    List.len result.rows

Decode a err :=
    List RowField
    ->
    Result
        (List (List U8)
        ->
        Result a [FieldNotFound Str]err)
        [FieldNotFound Str]

decode : CmdResult, Decode a err -> Result (List a) [FieldNotFound Str]err
decode = \@CmdResult r, @Decode getDecode ->
    when getDecode r.fields is
        Ok fn ->
            List.mapTry r.rows fn

        Err (FieldNotFound name) ->
            Err (FieldNotFound name)

str = decoder Ok

u8 = decoder Str.toU8

u16 = decoder Str.toU16

u32 = decoder Str.toU32

u64 = decoder Str.toU64

u128 = decoder Str.toU128

i8 = decoder Str.toI8

i16 = decoder Str.toI8

i32 = decoder Str.toI32

i64 = decoder Str.toI64

i128 = decoder Str.toI128

f32 = decoder Str.toF32

f64 = decoder Str.toF64

dec = decoder Str.toDec

nat = decoder Str.toNat

decoder = \fn -> \name ->
        rowFields <- @Decode

        when List.findFirstIndex rowFields \f -> f.name == name is
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
                        Err (FieldNotFound name)

            Err NotFound ->
                Err (FieldNotFound name)

map2 = \@Decode a, @Decode b, cb ->
    rowFields <- @Decode

    decodeA <- Result.try (a rowFields)
    decodeB <- Result.try (b rowFields)

    row <- Ok

    valueA <- Result.try (decodeA row)
    valueB <- Result.try (decodeB row)

    Ok (cb valueA valueB)

succeed = \value ->
    @Decode (\_ -> Ok (\_ -> Ok value))

with = \a, b -> map2 a b (\fn, val -> fn val)

apply = \a -> \fn -> with fn a
