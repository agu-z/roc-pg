module [
    Schema,
    ColumnId,
    Table,
    Column,
    new,
    get_name,
    get_tables,
    primary_column,
]

Nullable a : [Null, NotNull a]

Schema := {
    name : Str,
    tables : List Table,
    references : Dict ColumnId ColumnId,
    primary_columns : Dict ColumnId { table_name : Str, column_name : Str },
}

ColumnId : (I32, I16)

Table : {
    id : I32,
    name : Str,
    columns : List Column,
    constraints : List Constraint,
}

Column : {
    num : I16,
    name : Str,
    data_type : Str,
    type_category : Str,
    elem_data_type : Nullable Str,
    is_nullable : Bool,
}

Constraint : {
    type : Str,
    columns : List I16,
    foreign_table : I32, # pg sets this to 0 if not foreign key
    foreign_columns : List I16,
}

new : Str, List Table -> Schema
new = |schema_name, tables|
    tables_len : U64
    tables_len = List.len(tables)

    keys : {
        primary_columns : Dict ColumnId { table_name : Str, column_name : Str },
        references : Dict ColumnId ColumnId,
    }
    keys =
        List.walk(
            tables,
            {
                primary_columns: Dict.with_capacity(tables_len),
                references: Dict.with_capacity((tables_len * 4)),
            },
            |state_table, table|
                List.walk(
                    table.constraints,
                    state_table,
                    |state, constraint|
                        when constraint.type is
                            "p" ->
                                new_primary_columns =
                                    constraint.columns
                                    |> List.map(
                                        |col_num|

                                            names =
                                                when List.find_first(table.columns, |col| col.num == col_num) is
                                                    Ok({ name }) ->
                                                        {
                                                            table_name: table.name,
                                                            column_name: name,
                                                        }

                                                    Err(NotFound) ->
                                                        {
                                                            table_name: "${Num.to_str(table.id)}",
                                                            column_name: "${Num.to_str(col_num)}",
                                                        }

                                            ((table.id, col_num), names),
                                    )
                                    |> Dict.from_list

                                { state & primary_columns: Dict.insert_all(state.primary_columns, new_primary_columns) }

                            "f" ->
                                new_references =
                                    constraint.columns
                                    |> List.map2(
                                        constraint.foreign_columns,
                                        |col_num, foreign_column|
                                            ((table.id, col_num), (constraint.foreign_table, foreign_column)),
                                    )
                                    |> Dict.from_list

                                { state & references: Dict.insert_all(state.references, new_references) }

                            _ ->
                                state,
                ),
        )
    @Schema(
        {
            name: schema_name,
            tables,
            primary_columns: keys.primary_columns,
            references: keys.references,
        },
    )

get_tables : Schema -> List Table
get_tables = |@Schema(schema)| schema.tables

get_name : Schema -> Str
get_name = |@Schema(schema)| schema.name

## Recursively find the final column referenced by another column.
##
## Returns itself if it's part of a primary key and no foreign key.
primary_column :
    Schema,
    ColumnId
    -> Result
        {
            id : ColumnId,
            table_name : Str,
            column_name : Str,
        }
        [KeyNotFound]
primary_column = |@Schema(schema), column|
    when Dict.get(schema.references, column) is
        Ok(ref_column) ->
            primary_column(@Schema(schema), ref_column)

        Err(KeyNotFound) ->
            Dict.get(schema.primary_columns, column)
            |> Result.map_ok(
                |names| {
                    id: column,
                    table_name: names.table_name,
                    column_name: names.column_name,
                },
            )

expect primary_column(test_schema, (1, 1)) == Ok({ id: (1, 1), table_name: "users", column_name: "id" })
expect primary_column(test_schema, (1, 2)) == Err(KeyNotFound)
expect primary_column(test_schema, (2, 1)) == Ok({ id: (2, 1), table_name: "posts", column_name: "id" })
expect primary_column(test_schema, (2, 2)) == Ok({ id: (1, 1), table_name: "users", column_name: "id" })
expect primary_column(test_schema, (2, 3)) == Err(KeyNotFound)

test_schema =
    new(
        "public",
        [
            {
                id: 1,
                name: "users",
                columns: [
                    {
                        num: 1,
                        name: "id",
                        data_type: "int4",
                        type_category: "N",
                        elem_data_type: Null,
                        is_nullable: Bool.false,
                    },
                    {
                        num: 2,
                        name: "name",
                        data_type: "text",
                        type_category: "S",
                        elem_data_type: Null,
                        is_nullable: Bool.false,
                    },
                ],
                constraints: [
                    {
                        type: "p",
                        columns: [1],
                        foreign_table: 0,
                        foreign_columns: [],
                    },
                ],
            },
            {
                id: 2,
                name: "posts",
                columns: [
                    {
                        num: 1,
                        name: "id",
                        data_type: "int4",
                        type_category: "N",
                        elem_data_type: Null,
                        is_nullable: Bool.false,
                    },
                    {
                        num: 2,
                        name: "user_id",
                        data_type: "int4",
                        type_category: "N",
                        elem_data_type: Null,
                        is_nullable: Bool.false,
                    },
                    {
                        num: 3,
                        name: "title",
                        data_type: "text",
                        type_category: "S",
                        elem_data_type: Null,
                        is_nullable: Bool.false,
                    },
                ],
                constraints: [
                    {
                        type: "p",
                        columns: [1],
                        foreign_table: 0,
                        foreign_columns: [],
                    },
                    {
                        type: "f",
                        columns: [2],
                        foreign_table: 1,
                        foreign_columns: [1],
                    },
                ],
            },
        ],
    )

