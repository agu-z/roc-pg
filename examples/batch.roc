app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	pg: "../src/main.roc",
}

import pf.Stdout
import pf.Tcp
import pf.Duration

import pg.Pg

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

	result = Pg.Batch.succeed(
		|hi| |eleven| |thirty_one| { hi, forty_two: eleven + thirty_one },
	)
		.with_cmd(
			Pg.Cmd.new("select 'hi' as value")
				.expect_1(Pg.Result.str({})("value")),
		)
		.with_cmd(
			Pg.Cmd.new("select $1::int as value")
				.bind([Pg.Cmd.u8(11)])
				.expect_1(Pg.Result.u8({})("value")),
		)
		.with_cmd(
			Pg.Cmd.new("select $1::int as value")
				.bind([Pg.Cmd.u8(31)])
				.expect_1(Pg.Result.u8({})("value")),
		)
		.send!(client)?

	str42 = result.forty_two.to_str()
	Stdout.line!("${result.hi} ${str42}")?

	result_seq = Pg.Batch.sequence(
		(0..=20).iter().map(
			|num|
				Pg.Cmd.new("select $1::int as value")
					.bind([Pg.Cmd.u8(num)])
					.expect_1(Pg.Result.u8({})("value")),
		).collect(),
	).send!(client)?

	result_seq_str = result_seq
		.map(|num| num.to_str())
		|> Str.join_with(", ")

	Stdout.line!(result_seq_str)?

	Ok({})
}
