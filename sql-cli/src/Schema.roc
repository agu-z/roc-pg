interface Schema
    exposes [tables, columns]
    imports [sql.Sql, sql.Sql.Types]

tables = {
    schema: "information_schema",
    name: "tables",
    alias: "t",
    columns: \alias -> {
        name: Sql.identifier alias "table_name" Sql.Types.str,
        schema: Sql.identifier alias "table_schema" Sql.Types.str,
    },
}

columns = {
    schema: "information_schema",
    name: "columns",
    alias: "c",
    columns: \alias -> {
        tableName: Sql.identifier alias "table_name" Sql.Types.str,
        schema: Sql.identifier alias "table_schema" Sql.Types.str,
        name: Sql.identifier alias "column_name" Sql.Types.str,
        dataType: Sql.identifier alias "data_type" Sql.Types.str,
        isNullable: Sql.identifier alias "is_nullable" Sql.Types.str,
    },
}
