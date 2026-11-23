import Result
import Cmd as InternalCmd
import Batch as InternalBatch
import Protocol
import Bytes

tcp_timeout = 5_000

Pg :: [].{

	Result : Result

	Cmd(a, err) :: InternalCmd(a, err).{

		send! = |cmd, client| Client.command!(client, cmd)

		new : Str -> Cmd(Result, [])
		new = |sql| Cmd.(InternalCmd.from_sql(sql))

		expect_n : Cmd(Result, []),
		Result.Decode(a, err) -> Cmd(
			List(a),
			[FieldNotFound(Str), ..err],
		)
		expect_n = |Cmd.(cmd), decoder| Cmd.(
			cmd.with_decode(|r| r.decode(decoder)),
		)

		expect_1 : Cmd(Result, []), Result.Decode(a, [EmptyResult, ..err]) -> Cmd(a, [EmptyResult, FieldNotFound(Str), ..err])
		expect_1 = |Cmd.(cmd), decoder| Cmd.(
			cmd.with_limit(1).with_decode(
				|cmd_result| match cmd_result.decode(decoder)? {
					[row] => Ok(row)
					_ => Err(EmptyResult)
				},
			),
		)

		map : Cmd(a, err), (a -> b) -> Cmd(b, err)
		map = |Cmd.(cmd), f| Cmd.(cmd.map(f))

		with_custom_decode : Cmd(_, _), (Result -> Try(_, _)) -> Cmd(_, _)
		with_custom_decode = |Cmd.(cmd), f| Cmd.(cmd.with_decode(f))

		to_inspect : Cmd(a, err) -> Str
		to_inspect = |Cmd.(cmd)| {
			{ kind, bindings, limit: _ } = cmd.params()
			kind_str = inspect_kind(kind)
			bindings_str = bindings.map_with_index(
				|val, index| {
					n = index + 1
					"$${n.to_str()} = ${inspect_binding(val)}"
				},
			)
				|> Str.join_with("\n")

			"${kind_str}\n${bindings_str}"
		}

		Binding :: InternalCmd.Binding

		bind : Cmd(a, err), List(Binding) -> Cmd(a, err)
		bind = |Cmd.(cmd), bindings| {
			Cmd.(cmd.bind(bindings.map(|Binding.(b)| b)))
		}

		null : Binding
		null = Binding.(Null)

		str : Str -> Binding
		str = |value| Binding.(Text(value))

		u8 : U8 -> Binding
		u8 = |num| Binding.(Text(num.to_str()))

		u16 : U16 -> Binding
		u16 = |num| Binding.(Text(num.to_str()))

		u32 : U32 -> Binding
		u32 = |num| Binding.(Text(num.to_str()))

		u64 : U64 -> Binding
		u64 = |num| Binding.(Text(num.to_str()))

		u128 : U128 -> Binding
		u128 = |num| Binding.(Text(num.to_str()))

		i8 : I8 -> Binding
		i8 = |num| Binding.(Text(num.to_str()))

		i16 : I16 -> Binding
		i16 = |num| Binding.(Text(num.to_str()))

		i32 : I32 -> Binding
		i32 = |num| Binding.(Text(num.to_str()))

		i64 : I64 -> Binding
		i64 = |num| Binding.(Text(num.to_str()))

		i128 : I128 -> Binding
		i128 = |num| Binding.(Text(num.to_str()))

		f32 : F32 -> Binding
		f32 = |num| Binding.(Text(num.to_str()))

		f64 : F64 -> Binding
		f64 = |num| Binding.(Text(num.to_str()))

		bool : Bool -> Binding
		bool = |value| Binding.(Binary([if (value) 1 else 0]))

		bytes : List(U8) -> Binding
		bytes = |b| Binding.(Binary(b))
	}

	Batch(a, err) :: InternalBatch(a, err).{
		succeed : a -> Batch(a, err)
		succeed = |value| Batch.(InternalBatch.succeed(value))

		with_cmd : Batch((a -> b), err), Cmd(a, err) -> Batch(b, err)
		with_cmd = |Batch.(batch), Cmd.(cmd)|
			Batch.(InternalBatch.with_cmd(batch, cmd))

		sequence : List(Cmd(a, err)) -> Batch(List(a), err)
		sequence = |cmds| Batch.(
			InternalBatch.sequence(cmds.map(|Cmd.(cmd)| cmd)),
		)

		send! = |batch, client| Client.batch!(client, batch)
	}

	Client(tcp_stream) :: {
		backend_key : Try(Protocol.Backend.KeyData, [Pending]),
		stream : tcp_stream,
	}.{
		Error : Protocol.Backend.Error

		connect! : {
			stream : tcp_stream,
			user : Str,
			auth : [None, Password(Str)],
			database : Str,
		} => Try(Client, _)
			where [
				tcp_stream.write! : tcp_stream, List(U8), U64 => Try({}, _),
				tcp_stream.read_exactly! : tcp_stream, U64, U64 => Try(List(U8), _),
			]
		connect! = |{ stream, user, auth, database }| {
			stream.write!(
				Protocol.Frontend.startup({ user, database }),
				tcp_timeout,
			)?

			message_loop!(
				stream,
				{
					parameters: Dict.empty(),
					backend_key: Err(Pending),
				},
				|msg, state| match msg {
					AuthOk => next(state)

					AuthCleartextPassword => match auth {
						None => Err(PasswordRequired)
						Password(pwd) => {
							stream.write!(
								Protocol.Frontend.password_message(pwd),
								tcp_timeout,
							)?
							next(state)
						}
					}

					AuthUnsupported => Err(UnsupportedAuth)

					BackendKeyData(backend_key) => next({
						..state,
						backend_key: Ok(backend_key),
					})

					ReadyForQuery(_) => {
						client = Client.(
							{
								stream,
								backend_key: state.backend_key,
							},
						)

						Ok(Done(client))
					}

					_ => unexpected(msg)
				},
			)
		}

		command! : Client(tcp_stream),
		Cmd(a, err) => Try(
			a,
			[
				PgExpectErr(err),
				PgErr(Error),
				PgProtoErr(_),
				TcpReadErr(_),
				TcpUnexpectedEOF,
				TcpWriteErr(_),
			],
		)

			where [
				tcp_stream.write! : tcp_stream, List(U8), U64 => Try({}, _),
				tcp_stream.read_exactly! : tcp_stream, U64, U64 => Try(List(U8), _),
			]
		command! = |Client.({ stream, backend_key: _ }), Cmd.(cmd)| {
			{ kind, limit, bindings } = cmd.params()
			{ format_codes, param_values } = InternalCmd.encode_bindings(bindings)

			init = match kind {
				SqlCmd(sql) => {
					messages: Bytes.Encode.sequence([
						Protocol.Frontend.parse({
							sql,
							name: "",
							param_type_ids: [],
						}),
						Protocol.Frontend.bind({
							format_codes,
							param_values,
							portal: "",
							prepared_statement: "",
							column_format_codes: [],
						}),
						Protocol.Frontend.describe_portal(""),
						Protocol.Frontend.execute({ limit, portal: "" }),
					]),
					fields: [],
				}

				PreparedCmd(prepared) => {
					messages: Bytes.Encode.sequence([
						Protocol.Frontend.bind({
							format_codes,
							param_values,
							portal: "",
							prepared_statement: prepared.name,
							column_format_codes: [],
						}),
						Protocol.Frontend.execute({ limit, portal: "" }),
					]),
					fields: prepared.fields,
				}
			}

			send_with_sync!(stream, init.messages)?

			result = read_cmd_result!(init.fields, stream)?

			decoded = InternalCmd.decode(result, cmd).map_err(|e| PgExpectErr(e))?

			read_ready_for_query!(stream)?

			Ok(decoded)
		}

		batch! : Client(tcp_stream),
		Batch(a, err) => Try(
			a,
			[
				PgExpectErr(err),
				PgErr(Error),
				PgProtoErr(_),
				TcpReadErr(_),
				TcpUnexpectedEOF,
				TcpWriteErr(_),
			],
		)
			where [
				tcp_stream.write! : tcp_stream, List(U8), U64 => Try({}, _),
				tcp_stream.read_exactly! : tcp_stream, U64, U64 => Try(List(U8), _),
			]
		batch! = |Client.({ stream, .. }), Batch.(cmd_batch)| {
			{ commands, seen_sql, decode: batch_decode } = cmd_batch.params()

			reused_indexes = seen_sql.fold(
				Set.empty(),
				|set, _, { index, reused }|
					if reused {
						set.insert(index)
					} else {
						set
					},
			)

			inits = commands.map_with_index(|cmd, ix|
				init_batched_cmd(reused_indexes, cmd, ix))

			command_messages = inits.map(|init| init.messages)
				|> Bytes.Encode.sequence

			close_messages = reused_indexes.to_list().map(
				|ix| Protocol.Frontend.close_statement(
					InternalBatch.reuse_name(ix),
				),
			)
				|> Bytes.Encode.sequence

			messages = command_messages.concat(close_messages)

			_ = send_with_sync!(stream, messages)

			loop!(
				{
					remaining: inits,
					results: List.with_capacity(commands.len()),
				},
				|state| batch_read_step!(batch_decode, stream, state),
			)
		}

		prepare! : Client(tcp_stream),
		{ name : Str, sql : Str } => Try(
			Cmd(Result, []),
			[
				PgErr(Error),
				PgProtoErr(_),
				TcpReadErr(_),
				TcpUnexpectedEOF,
				TcpWriteErr(_),
			],
		)
			where [
				tcp_stream.write! : tcp_stream, List(U8), U64 => Try({}, _),
				tcp_stream.read_exactly! : tcp_stream, U64, U64 => Try(List(U8), _),
			]
		prepare! = |Client.({ stream, .. }), { name, sql }| {
			parse_and_describe = Bytes.Encode.sequence([
				Protocol.Frontend.parse({ name, sql, param_type_ids: [] }),
				Protocol.Frontend.describe_statement(name),
				Protocol.Frontend.sync,
			])

			stream.write!(parse_and_describe, tcp_timeout)?

			message_loop!(
				stream,
				{ fields: [], parameters: [] },
				|msg, state| match msg {
					ParseComplete | NoData =>
						next(state)

					ParameterDescription(parameters) =>
						next({ ..state, parameters })

					RowDescription(fields) =>
						next({ ..state, fields })

					ReadyForQuery(_) =>
						return_(
							InternalCmd.prepared({
								name,
								fields: state.fields,
								parameters: state.parameters,
							}),
						)

					_ => unexpected(msg)
				},
			).map_ok(|cmd| Cmd.(cmd))
		}

		error_to_str : Error -> Str
		error_to_str = |err| {
			add_field = |str, name, result| match result {
				Ok(value) => "${str}\n${name}: ${value}"
				Err({}) => str
			}

			fields_str = ""
				|> add_field("Detail", err.detail)
				|> add_field("Hint", err.hint)
				|> add_field(
					"Position",
					err.position.map_ok(|pos| pos.to_str()),
				)
				|> add_field(
					"Internal Position",
					err.internal_position.map_ok(|pos| pos.to_str()),
				)
				|> add_field("Internal Query", err.internal_query)
				|> add_field("Where", err.ewhere)
				|> add_field("Schema", err.schema_name)
				|> add_field("Table", err.table_name)
				|> add_field("Data type", err.data_type_name)
				|> add_field("Constraint", err.constraint_name)
				|> add_field("File", err.file)
				|> add_field("Line", err.line)
				|> add_field("Routine", err.line)

			"${err.localized_severity} (${err.code}): ${err.message}\n${fields_str}"
				.trim()
		}
	}

}

inspect_kind = |kind| match kind {
	SqlCmd(sql) => "SQL: ${sql}"
	PreparedCmd(prep) => "Prepared: ${prep.name}"
}

inspect_binding = |binding| match binding {
	Null => "NULL"
	Text(text) => text
	Binary(bin) => bin.map(|b| b.to_str()) |> Str.join_with(",")
}

init_batched_cmd : Set(U64),
InternalBatch.BatchedCmd,
U64 -> {
	messages : List(U8),
	fields : [
		Describe,
		ReuseFrom(U64),
		Known(List(Result.RowField)),
	],
}
init_batched_cmd = |reused_indexes, cmd, cmd_index| {
	{ format_codes, param_values } = InternalCmd.encode_bindings(cmd.bindings)

	match cmd.kind {
		SqlCmd(sql) => {
			name =
				if (reused_indexes.contains(cmd_index))
					InternalBatch.reuse_name(cmd_index)
				else ""

			{
				messages: Bytes.Encode.sequence([
					Protocol.Frontend.parse({ sql, name, param_type_ids: [] }),
					Protocol.Frontend.bind({
						format_codes,
						param_values,
						prepared_statement: name,
						portal: "",
						column_format_codes: [],
					}),
					Protocol.Frontend.describe_portal(""),
					Protocol.Frontend.execute({ limit: cmd.limit, portal: "" }),
				]),
				fields: Describe,
			}
		}

		ReuseSql(index) => {
			messages: Bytes.Encode.sequence([
				Protocol.Frontend.bind({
					format_codes,
					param_values,
					prepared_statement: InternalBatch.reuse_name(index),
					portal: "",
					column_format_codes: [],
				}),
				Protocol.Frontend.execute({ limit: cmd.limit, portal: "" }),
			]),
			fields: ReuseFrom(index),
		}

		PreparedCmd(prepared) => {
			messages: Bytes.Encode.sequence([
				Protocol.Frontend.bind({
					format_codes,
					param_values,
					prepared_statement: prepared.name,
					portal: "",
					column_format_codes: [],
				}),
				Protocol.Frontend.execute({ limit: cmd.limit, portal: "" }),
			]),
			fields: Known(prepared.fields),
		}
	}
}

# batch_read_step! : _, tcp_stream, _ => _
# 	where [tcp_stream.read_exactly! : tcp_stream, U64 => Try(List(U8), _)]
batch_read_step! = |batch_decode, stream, { remaining, results }| match remaining {
	[] => match batch_decode(results) {
		Ok({ value, .. }) => {
			_ = read_ready_for_query!(stream)?
			return_(value)
		}

		Err(MissingCmdResult(index)) => Err(
			PgProtoErr(MissingBatchedCmdResult(index)),
		)

		Err(ExpectErr(err)) => Err(PgExpectErr(err))
	}

	[first, ..] => {
		fields =
			# match first.fields {
			# 	Describe => Ok([])
			# 	ReuseFrom(index) => match results.get(index) {
			# 		Ok(result) => Ok(Result.fields(result))
			# 		# TODO: better name
			# 		Err(OutOfBounds) => Err(PgProtoErr(ResultOutOfBounds))
			# 	}
			# 	Known(fields) => Ok(fields)
			# }?
			batched_cmd_fields(results, first.fields)?
		result = read_cmd_result!(fields, stream)?

		next({
			remaining: remaining.drop_first(1),
			results: results.append(result),
		})
	}
}

batched_cmd_fields = |results, fields_method|
	match fields_method {
		Describe => Ok([])
		ReuseFrom(index) => match results.get(index) {
			Ok(result) => Ok(Result.fields(result))
			# TODO: better name
			Err(OutOfBounds) => Err(PgProtoErr(ResultOutOfBounds))
		}
		Known(fields) => Ok(fields)
	}

read_message! : tcp_stream => Try(Protocol.Backend.Message, _)
	where [tcp_stream.read_exactly! : tcp_stream, U64, U64 => Try(List(U8), _)]
read_message! = |stream| {
	header_bytes = stream.read_exactly!(5, tcp_timeout)?

	proto_decode = |bytes, dec|
		Bytes.Decode.decode(bytes, dec).map_err(|e| PgProtoErr(e))

	meta = proto_decode(header_bytes, Protocol.Backend.header({}))?

	if meta.len > 0 {
		len_u64 = meta.len.to_u64_try().ok_or(1)
		payload = stream.read_exactly!(len_u64, tcp_timeout)?
		proto_decode(payload, Protocol.Backend.message(meta.msg_type))
	} else {
		proto_decode([], Protocol.Backend.message(meta.msg_type))
	}
}

loop! : state, (state => Try([Step(state), Done(done)], err)) => Try(done, err)
loop! = |state, fn!| match fn!(state) {
	Err(err) => Err(err)
	Ok(Done(done)) => Ok(done)
	Ok(Step(next_state)) => loop!(next_state, fn!)
}

message_loop! : tcp_stream,
state,
(
	Protocol.Backend.Message,
	state => Try(
		[
			Done(done),
			Step(state),
		],
		_,
	)) => Try(done, _)
		where [tcp_stream.read_exactly! : tcp_stream, U64, U64 => Try(List(U8), _)]
message_loop! = |stream, init_state, step_fn!|
	loop!(
		init_state,
		|state| {
			message = read_message!(stream)?

			match message {
				ErrorResponse(error) => Err(PgErr(error))
				ParameterStatus(_) => Ok(Step(state))
				_ => step_fn!(message, state)
			}
		},
	)

send_with_sync! : tcp_stream, List(U8) => Try({}, _)
	where [tcp_stream.write! : tcp_stream, List(U8), U64 => Try({}, _)]
send_with_sync! = |stream, bytes|
	stream.write!(
		Bytes.Encode.sequence([bytes, Protocol.Frontend.sync]),
		tcp_timeout,
	)

get_bytes = |num| match num {
	One => [1]
	Two => [2]
	Three => [3]
	_ => [0]
}

read_cmd_result! : List(Protocol.Backend.RowField), tcp_stream => Try(Result, _)
	where [tcp_stream.read_exactly! : tcp_stream, U64, U64 => Try(List(U8), _)]
read_cmd_result! = |init_fields, stream|
	message_loop!(
		stream,
		{
			fields: init_fields,
			rows: [],
			parameters: [],
		},
		|msg, state|
			match msg {
				ParseComplete | BindComplete | NoData | NoticeResponse(_) =>
					next(state)

				ParameterDescription(parameters) =>
					next({ ..state, parameters })

				RowDescription(fields) =>
					next({ ..state, fields })

				DataRow(row) =>
					next({ ..state, rows: state.rows.append(row) })

				CommandComplete(_) | EmptyQueryResponse | PortalSuspended =>
					return_(Result.create(state))

				_ => unexpected(msg)
			},
	)

read_ready_for_query! = |stream|
	message_loop!(
		stream,
		{},
		|msg, {}| match msg {
			CloseComplete => next({})
			ReadyForQuery(_) => return_({})
			_ => unexpected(msg)
		},
	)

next : a -> Try([Step(a)], _)
next = |state| Ok(Step(state))

return_ : a -> Try([Done(a)], _)
return_ = |result| Ok(Done(result))

unexpected : msg -> Try(_, [PgProtoErr([UnexpectedMsg(msg)])])
unexpected = |msg| Err(PgProtoErr(UnexpectedMsg(msg)))
