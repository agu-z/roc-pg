module [
    module,
    type,
    identifier,
    tableAlias,
    sqlName,
]

import Keyword

# Roc

module : Str -> Str
module = to upperCamelCase

type : Str -> Str
type = to upperCamelCase

identifier : Str -> Str
identifier = to lowerCamelCase

# SQL

tableAlias : Str -> Str
tableAlias =
    wordList <- to

    List.keepOks wordList \word ->
        when word is
            [initial, ..] ->
                Ok initial

            _ ->
                Err {}

expect tableAlias "Product" == "p"
expect tableAlias "product_users" == "pu"
expect tableAlias "order_Products" == "op"

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
    "\"$(value)\""

sqlQuote : Str -> Str
sqlQuote = \value ->
    doubled =
        value
        |> Str.replaceEach "\"" "\\\"\\\""

    "\\\"$(doubled)\\\""

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
                |> List.dropFirst 1
                |> List.all \char ->
                    isLowerAlpha char || char == '_' || isNumeric char
            )
            && !(Keyword.isReserved bytes)

# Casing

to : (List (List U8) -> List U8) -> (Str -> Str)
to = \fn -> \name ->
        name
        |> words
        |> fn
        |> Str.fromUtf8
        |> Result.withDefault ""

upperCamelCase : List (List U8) -> List U8
upperCamelCase = \wordList ->
    List.joinMap wordList \word ->
        when word is
            [initial, ..] ->
                List.set word 0 (toUpper initial)

            _ ->
                word

expect (to upperCamelCase) "Product" == "Product"
expect (to upperCamelCase) "product_users" == "ProductUsers"
expect (to upperCamelCase) "order_Products" == "OrderProducts"
expect (to upperCamelCase) "OrderProducts" == "OrderProducts"
expect (to upperCamelCase) "orderProducts" == "OrderProducts"
expect (to upperCamelCase) "123" == "N123"

lowerCamelCase : List (List U8) -> List U8
lowerCamelCase = \wordList ->
    when wordList is
        [] ->
            []

        [first, ..] ->
            rest =
                wordList
                |> List.dropFirst 1
                |> upperCamelCase

            List.concat first rest

expect (to lowerCamelCase) "Product" == "product"
expect (to lowerCamelCase) "product_users" == "productUsers"
expect (to lowerCamelCase) "order_Products" == "orderProducts"
expect (to lowerCamelCase) "OrderProducts" == "orderProducts"
expect (to lowerCamelCase) "orderProducts" == "orderProducts"
expect (to lowerCamelCase) "123" == "n123"

# Helpers

words : Str -> List (List U8)
words = \value ->
    initial = {
        word: List.withCapacity 16,
        wordList: List.withCapacity 4,
    }

    addWord = \state ->
        lower = List.map state.word toLower
        List.append state.wordList lower

    value
    |> Str.toUtf8
    |> List.walk initial \state, char ->
        if char == '_' || char == ' ' then
            {
                word: List.withCapacity 16,
                wordList: addWord state,
            }
        else if isUpperAlpha char && endsWithLower state.word then
            {
                word: List.reserve [char] 15,
                wordList: addWord state,
            }
        else if isNumeric char && List.isEmpty state.word then
            {
                word: state.word |> List.append 'n' |> List.append char,
                wordList: state.wordList,
            }
        else if isAlphaNum char then
            {
                word: List.append state.word char,
                wordList: state.wordList,
            }
        else
            code = Str.toUtf8 "c$(Num.toStr char)"

            {
                word: List.concat state.word code,
                wordList: state.wordList,
            }
    |> addWord

endsWithLower : List U8 -> Bool
endsWithLower = \word ->
    when List.last word is
        Ok last ->
            isLowerAlpha last

        Err ListWasEmpty ->
            Bool.false

wordsStr : Str -> List Str
wordsStr = \value ->
    words value
    |> List.map \word ->
        word
        |> Str.fromUtf8
        |> Result.withDefault ""

expect wordsStr "product" == ["product"]
expect wordsStr "product_id" == ["product", "id"]
expect wordsStr "product_user_id" == ["product", "user", "id"]
expect wordsStr "_product_id" == ["", "product", "id"]
expect wordsStr "product_id_" == ["product", "id", ""]
expect wordsStr "Product" == ["product"]
expect wordsStr "productId" == ["product", "id"]
expect wordsStr "productID" == ["product", "id"]
expect wordsStr "ProductId" == ["product", "id"]
expect wordsStr "productUserId" == ["product", "user", "id"]
expect wordsStr "productUser_Id" == ["product", "user", "id"]
expect wordsStr "productUser__Id" == ["product", "user", "", "id"]
expect wordsStr "countA" == ["count", "a"]
expect wordsStr "123_x" == ["n123", "x"]
expect wordsStr "nice day!" == ["nice", "dayc33"]

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
