app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	pg: "../src/main.roc",
}

import pf.Stdout
import pf.Tcp
import pf.Duration

import pg.Pg
import pg.Result

main! = |_| {
	client = Pg.Client.connect!({
		connection: Tcp.connect!(
			"localhost",
			5432,
			{
				connect_timeout: Duration.millis(10_000),
				io_timeout: Duration.millis(5_000),
			},
		)?,
		user: "postgres",
		auth: None,
		database: "postgres",
	})?
	Stdout.line!("Connected!")?

	rows = Pg.Cmd.new(
		\\ select $1 as name, $2 as age
		\\ union all
		\\ select 'Julio' as name, 23 as age
	)
		.bind([Pg.Cmd.str("John"), Pg.Cmd.u8(32)])
		.expect_n(
			{
				name: Result.str("name"),
				age: Result.u8("age"),
			}.Result,
		)
		.send!(client)?

	str = rows
		.map(Str.inspect)
		|> Str.join_with("\n")

	Stdout.line!("Got records:\n${str}")?

	Ok({})
}
