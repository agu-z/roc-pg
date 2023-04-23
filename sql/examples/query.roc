app "query"
    packages {
        pf: "https://github.com/agu-z/roc-basic-cli/releases/download/0.5.0/S8r4wytSGYKi-iMrjaRZxv2Hope_CX7dF6rMdciYja8.tar.gz",
        pg: "../../src/main.roc",
        sql: "../src/main.roc",
    }
    imports [
        pf.Task.{ Task, await },
        pf.Process,
        pf.Stdout,
        pf.Stderr,
        pg.Pg.Cmd,
        pg.Pg.Client,
        pg.Pg.Result,
        sql.Sql,
        sql.Sql.Decode,
    ]
    provides [main] to pf

task =
    client <- Pg.Client.withConnect {
            host: "localhost",
            port: 5432,
            user: "aguz",
            auth: Password "the_password",
            database: "aguz",
        }

    _ <- Stdout.line "Connected!" |> await

    usersTable = {
        schema: "public",
        name: "users",
        fields: \alias -> {
            name: Sql.identifier alias "name" Sql.Decode.decodeText,
            active: Sql.identifier alias "active" Sql.Decode.decodeBool,
            age: Sql.identifier alias "age" Sql.Decode.decodeU8,
            organizationId: Sql.identifier alias "organization_id" Sql.Decode.decodeU32,
        },
    }

    compiled = Sql.compile
        (
            users <- Sql.from usersTable
            Sql.select users.name
        )

    _ <- Stdout.line compiled.sql |> await

    rows <-
        Pg.Cmd.new
            """
            select $1 as name, $2 as age
            union all
            select 'Julio' as name, 23 as age
            """
        |> Pg.Cmd.bind [Pg.Cmd.str "John", Pg.Cmd.u8 32]
        |> Pg.Cmd.expectN
            (
                Pg.Result.succeed
                    (\name -> \age ->
                            ageStr = Num.toStr age

                            "\(name): \(ageStr)"
                    )
                |> Pg.Result.with (Pg.Result.str "name")
                |> Pg.Result.with (Pg.Result.u8 "age")
            )
        |> Pg.Client.command client
        |> await

    Stdout.line (Str.joinWith rows "\n")

main : Task {} []
main =
    Task.attempt task \result ->
        when result is
            Ok _ ->
                Process.exit 0

            Err (TcpPerformErr (PgErr err)) ->
                _ <- Stderr.line (Pg.Client.errorToStr err) |> await
                Process.exit 2

            Err err ->
                dbg
                    err

                _ <- Stderr.line "Something went wrong" |> await
                Process.exit 1
