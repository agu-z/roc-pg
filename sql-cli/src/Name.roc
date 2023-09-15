interface Name
    exposes [
        module,
        def,
        tableAlias,
        sqlName,
    ]
    imports [
        Keyword,
    ]

def : Str -> Str
def = \name ->
    chars = pascalCase name |> Str.toUtf8

    when chars is
        [initial, ..] ->
            chars
            |> List.set 0 (toLower initial)
            |> Str.fromUtf8
            |> Result.withDefault ""

        _ ->
            name

expect def "Product_users" == "productUsers"
expect def "order_Products" == "orderProducts"
expect def "human string stuff!" == "humanstringstuff"
expect def "abc_x234" == "abcX234"
expect def "123" == "n123"

pascalCase : Str -> Str
pascalCase = \name ->
    # TODO: Handle camelCase input
    name
    |> Str.split "_"
    |> List.map \word ->
        utf8 =
            word
            |> Str.toUtf8
            |> List.keepIf isAlphaNum

        when utf8 is
            [initial, ..] ->
                adjusted =
                    utf8
                    |> List.map toLower
                    |> List.set 0 (toUpper initial)

                prefixed =
                    if isNumeric initial then
                        List.prepend adjusted 'N'
                    else
                        adjusted

                prefixed
                |> Str.fromUtf8
                |> Result.withDefault ""

            _ ->
                ""
    |> Str.joinWith ""

module : Str -> Str
module = pascalCase

expect module "product_users" == "ProductUsers"
expect module "order_Products" == "OrderProducts"
expect module "123" == "N123"

tableAlias : Str -> Str
tableAlias = \name ->
    # TODO: Handle camelCase input
    name
    |> Str.split "_"
    |> List.map \word ->
        utf8 =
            word
            |> Str.toUtf8
            |> List.keepIf isAlphaNum

        when utf8 is
            [initial, ..] ->
                initial
                |> toLower
                |> List.single
                |> Str.fromUtf8
                |> Result.withDefault ""

            _ ->
                ""
    |> Str.joinWith ""

expect tableAlias "Users" == "u"
expect tableAlias "product_users" == "pu"

sqlName : Str -> Str
sqlName = \value ->
    if isValidSqlName value then
        quote value
    else
        value
        |> sqlQuote
        |> quote

quote : Str -> Str
quote = \value ->
    "\"\(value)\""

sqlQuote : Str -> Str
sqlQuote = \value ->
    doubled =
        value
        |> Str.replaceEach "\"" "\\\"\\\""

    "\\\"\(doubled)\\\""

expect sqlName "products" == "\"products\""
expect sqlName "product_orders" == "\"product_orders\""
expect sqlName "p2" == "\"p2\""
expect sqlName "2p" == "\"\\\"2p\\\"\""
expect sqlName "productOrders" == "\"\\\"productOrders\\\"\""
expect sqlName "where" == "\"\\\"where\\\"\""
expect sqlName "users_where" == "\"users_where\""
expect sqlName "class-name" == "\"\\\"class-name\\\"\""
expect
    generated = sqlName "User \"Bond\""

    expected =
        """
        "\\"User \\"\\"Bond\\"\\"\\""
        """
    generated == expected

isValidSqlName : Str -> Bool
isValidSqlName = \value ->
    bytes = Str.toUtf8 value

    when bytes is
        [] ->
            Bool.false

        [first, ..] ->
            (isLowerAlpha first || first == '_')
            && (
                bytes
                |> List.dropFirst
                |> List.all \char ->
                    isLowerAlpha char || char == '_' || isNumeric char
            )
            && !(Keyword.isReserved bytes)

caseDiff : U8
caseDiff = 'a' - 'A'

toLower : U8 -> U8
toLower = \char ->
    if isUpperAlpha char then
        char + caseDiff
    else
        char

expect toLower 'G' == 'g'
expect toLower 'm' == 'm'

toUpper : U8 -> U8
toUpper = \char ->
    if isLowerAlpha char then
        char - caseDiff
    else
        char

expect toUpper 'l' == 'L'
expect toUpper 'E' == 'E'

isAlphaNum : U8 -> Bool
isAlphaNum = \char ->
    isUpperAlpha char || isLowerAlpha char || isNumeric char

isUpperAlpha : U8 -> Bool
isUpperAlpha = \char ->
    char >= 'A' && char <= 'Z'

isLowerAlpha : U8 -> Bool
isLowerAlpha = \char ->
    char >= 'a' && char <= 'z'

isNumeric : U8 -> Bool
isNumeric = \char ->
    char >= '0' && char <= '9'
