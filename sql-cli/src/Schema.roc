interface Schema
    exposes [tables, columns]
    imports [sql.Sql, sql.Sql.Decode]

tables = {
    schema: "information_schema",
    name: "tables",
    fields: \alias -> {
        name: Sql.identifier alias "table_name" Sql.Decode.text,
        schema: Sql.identifier alias "table_schema" Sql.Decode.text,
    },
}

columns = {
    schema: "information_schema",
    name: "columns",
    fields: \alias -> {
        tableName: Sql.identifier alias "table_name" Sql.Decode.text,
        schema: Sql.identifier alias "table_schema" Sql.Decode.text,
        name: Sql.identifier alias "column_name" Sql.Decode.text,
        dataType: Sql.identifier alias "data_type" Sql.Decode.text,
    },
}
