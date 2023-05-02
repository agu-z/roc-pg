interface Schema
    exposes [tables, columns]
    imports [sql.Sql, sql.Sql.Decode]

tables = {
    schema: "information_schema",
    name: "tables",
    columns: \alias -> {
        name: Sql.identifier alias "table_name" Sql.Decode.str,
        schema: Sql.identifier alias "table_schema" Sql.Decode.str,
    },
}

columns = {
    schema: "information_schema",
    name: "columns",
    columns: \alias -> {
        tableName: Sql.identifier alias "table_name" Sql.Decode.str,
        schema: Sql.identifier alias "table_schema" Sql.Decode.str,
        name: Sql.identifier alias "column_name" Sql.Decode.str,
        dataType: Sql.identifier alias "data_type" Sql.Decode.str,
        isNullable: Sql.identifier alias "is_nullable" Sql.Decode.str,
    },
}
