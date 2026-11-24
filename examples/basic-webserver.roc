app [Context, program] {
	http: "https://github.com/roc-lang/http/releases/download/2.0.0/6ZUwqYhCS8PU9Mo6MF7oV82ET2o7KYb57CLKDq4cq4sS.tar.zst",
	pf: platform "https://github.com/roc-lang/basic-webserver/releases/download/0.22.0/BM2BUXA8mwbcoKPUfRV8cLtsUdnsSBPwjNLPRUJQAQ7Q.tar.zst",
	pg: "../src/main.roc",
}

## Wrapper around platform's Tcp.Connection to adapt API to what Pg.Client
## needs
TcpConnection :: Tcp.Connection.{
	from_pf = |connection| TcpConnection.(connection)

	write_bytes! : TcpConnection, List(U8) => _
	write_bytes! = |TcpConnection.(connection), bytes| connection.write!(bytes)

	read_exactly! : TcpConnection, U64 => _
	read_exactly! = |TcpConnection.(connection), len| connection.read_exactly!(len)
}

import pf.Stdout
import pf.Tcp
import pf.Server

import http.Response
import pg.Pg

Context : { client : Pg.Client(TcpConnection) }

program = { init!, respond!, shutdown!: |_, _| Ok({}) }

init! = || {
	_ = Stdout.line!("Start!")

	client = Pg.Client.connect!({
		connection: TcpConnection.from_pf(Tcp.connect!("localhost", 5432)?),
		user: "postgres",
		auth: None,
		database: "postgres",
	})?

	_ = Stdout.line!("Connected!")

	Ok({
		config: Server.default_config,
		context: { client: client },
	})
}

respond! = |_request, { client }| {
	rows =
		Pg.Cmd.new(
			\\ select $1 as name, $2 as age
			\\ union all
			\\ select 'Julio' as name, 23 as age
		)
			.bind([Pg.Cmd.str("John"), Pg.Cmd.u8(32)])
			.expect_n(
				{
					name: Pg.Result.str({})("name"),
					age: Pg.Result.u8({})("age"),
				}.Pg.Result,
			)
			.send!(client)?

	response_body =
		rows.map(Str.inspect)
			|> Str.join_with("\n")

	_ = Stdout.line!("Responding with: ${response_body}")

	Server.respond(
		Response.from_status(200)
			.with_body(response_body.to_utf8()),
	)
		|> Ok
}
