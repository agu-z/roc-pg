module [
    module,
    type,
    identifier,
    table_alias,
    sql_name,
]

import Keyword

# Roc

module : Str -> Str
module = to(upper_camel_case)

type : Str -> Str
type = to(upper_camel_case)

identifier : Str -> Str
identifier = to(lower_camel_case)

# SQL

table_alias : Str -> Str
table_alias =
    to(
        |word_list|
            List.keep_oks(
                word_list,
                |word|
                    when word is
                        [initial, ..] ->
                            Ok(initial)

                        _ ->
                            Err({}),
            ),
    )

expect table_alias("Product") == "p"
expect table_alias("product_users") == "pu"
expect table_alias("order_Products") == "op"

sql_name : Str -> Str
sql_name = |value|
    if is_valid_sql_name(value) then
        quote(value)
    else
        value
        |> sql_quote
        |> quote

quote : Str -> Str
quote = |value|
    "\"${value}\""

sql_quote : Str -> Str
sql_quote = |value|
    doubled =
        value
        |> Str.replace_each("\"", "\\\"\\\"")

    "\\\"${doubled}\\\""

expect sql_name("products") == "\"products\""
expect sql_name("product_orders") == "\"product_orders\""
expect sql_name("p2") == "\"p2\""
expect sql_name("2p") == "\"\\\"2p\\\"\""
expect sql_name("productOrders") == "\"\\\"productOrders\\\"\""
expect sql_name("where") == "\"\\\"where\\\"\""
expect sql_name("users_where") == "\"users_where\""
expect sql_name("class-name") == "\"\\\"class-name\\\"\""
expect
    generated = sql_name("User \"Bond\"")

    expected =
        """
        "\\"User \\"\\"Bond\\"\\"\\""
        """
    generated == expected

is_valid_sql_name : Str -> Bool
is_valid_sql_name = |value|
    bytes = Str.to_utf8(value)

    when bytes is
        [] ->
            Bool.false

        [first, ..] ->
            (is_lower_alpha(first) or first == '_')
            and (
                bytes
                |> List.drop_first(1)
                |> List.all(
                    |char|
                        is_lower_alpha(char) or char == '_' or is_numeric(char),
                )
            )
            and !(Keyword.is_reserved(bytes))

# Casing

to : (List (List U8) -> List U8) -> (Str -> Str)
to = |fn|
    |name|
        name
        |> words
        |> fn
        |> Str.from_utf8
        |> Result.with_default("")

upper_camel_case : List (List U8) -> List U8
upper_camel_case = |word_list|
    List.join_map(
        word_list,
        |word|
            when word is
                [initial, ..] ->
                    List.set(word, 0, to_upper(initial))

                _ ->
                    word,
    )

expect (to(upper_camel_case))("Product") == "Product"
expect (to(upper_camel_case))("product_users") == "ProductUsers"
expect (to(upper_camel_case))("order_Products") == "OrderProducts"
expect (to(upper_camel_case))("OrderProducts") == "OrderProducts"
expect (to(upper_camel_case))("orderProducts") == "OrderProducts"
expect (to(upper_camel_case))("123") == "N123"

lower_camel_case : List (List U8) -> List U8
lower_camel_case = |word_list|
    when word_list is
        [] ->
            []

        [first, ..] ->
            rest =
                word_list
                |> List.drop_first(1)
                |> upper_camel_case

            List.concat(first, rest)

expect (to(lower_camel_case))("Product") == "product"
expect (to(lower_camel_case))("product_users") == "productUsers"
expect (to(lower_camel_case))("order_Products") == "orderProducts"
expect (to(lower_camel_case))("OrderProducts") == "orderProducts"
expect (to(lower_camel_case))("orderProducts") == "orderProducts"
expect (to(lower_camel_case))("123") == "n123"

# Helpers

words : Str -> List (List U8)
words = |value|
    initial = {
        word: List.with_capacity(16),
        word_list: List.with_capacity(4),
    }

    add_word = |state|
        lower = List.map(state.word, to_lower)
        List.append(state.word_list, lower)

    value
    |> Str.to_utf8
    |> List.walk(
        initial,
        |state, char|
            if char == '_' or char == ' ' then
                {
                    word: List.with_capacity(16),
                    word_list: add_word(state),
                }
            else if is_upper_alpha(char) and ends_with_lower(state.word) then
                {
                    word: List.reserve([char], 15),
                    word_list: add_word(state),
                }
            else if is_numeric(char) and List.is_empty(state.word) then
                {
                    word: state.word |> List.append('n') |> List.append(char),
                    word_list: state.word_list,
                }
            else if is_alpha_num(char) then
                {
                    word: List.append(state.word, char),
                    word_list: state.word_list,
                }
            else
                code = Str.to_utf8("c${Num.to_str(char)}")

                {
                    word: List.concat(state.word, code),
                    word_list: state.word_list,
                },
    )
    |> add_word

ends_with_lower : List U8 -> Bool
ends_with_lower = |word|
    when List.last(word) is
        Ok(last) ->
            is_lower_alpha(last)

        Err(ListWasEmpty) ->
            Bool.false

words_str : Str -> List Str
words_str = |value|
    words(value)
    |> List.map(
        |word|
            word
            |> Str.from_utf8
            |> Result.with_default(""),
    )

expect words_str("product") == ["product"]
expect words_str("product_id") == ["product", "id"]
expect words_str("product_user_id") == ["product", "user", "id"]
expect words_str("_product_id") == ["", "product", "id"]
expect words_str("product_id_") == ["product", "id", ""]
expect words_str("Product") == ["product"]
expect words_str("productId") == ["product", "id"]
expect words_str("productID") == ["product", "id"]
expect words_str("ProductId") == ["product", "id"]
expect words_str("productUserId") == ["product", "user", "id"]
expect words_str("productUser_Id") == ["product", "user", "id"]
expect words_str("productUser__Id") == ["product", "user", "", "id"]
expect words_str("countA") == ["count", "a"]
expect words_str("123_x") == ["n123", "x"]
expect words_str("nice day!") == ["nice", "dayc33"]

case_diff : U8
case_diff = 'a' - 'A'

to_lower : U8 -> U8
to_lower = |char|
    if is_upper_alpha(char) then
        char + case_diff
    else
        char

expect to_lower('G') == 'g'
expect to_lower('m') == 'm'

to_upper : U8 -> U8
to_upper = |char|
    if is_lower_alpha(char) then
        char - case_diff
    else
        char

expect to_upper('l') == 'L'
expect to_upper('E') == 'E'

is_alpha_num : U8 -> Bool
is_alpha_num = |char|
    is_upper_alpha(char) or is_lower_alpha(char) or is_numeric(char)

is_upper_alpha : U8 -> Bool
is_upper_alpha = |char|
    char >= 'A' and char <= 'Z'

is_lower_alpha : U8 -> Bool
is_lower_alpha = |char|
    char >= 'a' and char <= 'z'

is_numeric : U8 -> Bool
is_numeric = |char|
    char >= '0' and char <= '9'
