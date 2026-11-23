import Bytes exposing [Decode, Encode]

Protocol :: [].{

	Frontend :: [].{

		FormatCode : [Text, Binary]

		startup : { user : Str, database : Str } -> List(U8)
		startup = |{ user, database }| [
			# Version number
			Encode.i16(3),
			Encode.i16(0),
			# Encoding
			[
				encode_param("client_encoding", "utf_8"),
				encode_param("user", user),
				encode_param("database", database),
			]
				|> Encode.sequence
				|> Encode.null_terminate,
		]
			|> Encode.sequence
			|> prepend_length

		password_message : Str -> List(U8)
		password_message = |pwd| encode_message('p', [Encode.c_str(pwd)])

		terminate : List(U8)
		terminate = encode_message('X', [])

		parse : {
			sql : Str,
			name : Str, # optional
			param_type_ids : List(I32), # optional
		} -> List(U8)
		parse = |{ sql, name, param_type_ids }| encode_message(
			'P',
			[
				Encode.c_str(name),
				Encode.c_str(sql),
				encode_array(param_type_ids, Encode.i32),
			],
		)

		bind : {
			portal : Str, # Optional default ""
			prepared_statement : Str, # optional default ""
			format_codes : List(FormatCode), # optional
			param_values : List([Null, Value(List(U8))]),
			column_format_codes : List(FormatCode), # optional
		} -> List(U8)
		bind = |
			{
				portal,
				prepared_statement,
				format_codes,
				param_values,
				column_format_codes,
			},
		| encode_message(
			'B',
			[
				Encode.c_str(portal),
				Encode.c_str(prepared_statement),
				encode_array(format_codes, format_code),
				encode_array(
					param_values,
					|value| match value {
						Null => Encode.i32(-1)
						Value(b) => encode_bytes(b)
					},
				),
				encode_array(column_format_codes, format_code),
			],
		)

		describe_portal : Str -> List(U8)
		describe_portal = |name| encode_message('D', [Encode.u8('P'), Encode.c_str(name)])

		describe_statement : Str -> List(U8)
		describe_statement = |name| encode_message(
			'D',
			[
				Encode.u8('S'),
				Encode.c_str(name),
			],
		)

		execute : { portal : Str, limit : [None, Limit(I32)] } -> List(U8)
		execute = |{ portal, limit }| {
			limit_or_zero = match limit {
				None => 0
				Limit(n) => n
			}

			encode_message(
				'E',
				[
					Encode.c_str(portal),
					Encode.i32(limit_or_zero),
				],
			)
		}

		close_statement : Str -> List(U8)
		close_statement = |name| encode_message(
			'C',
			[
				Encode.u8('S'),
				Encode.c_str(name),
			],
		)

		sync : List(U8)
		sync = encode_message('S', [])

	}

	Backend :: [].{

		Error : {
			localized_severity : Str,
			severity : Try(ErrorSeverity, {}),
			code : Str,
			message : Str,
			detail : Try(Str, {}),
			hint : Try(Str, {}),
			position : Try(U32, {}),
			internal_position : Try(U32, {}),
			internal_query : Try(Str, {}),
			ewhere : Try(Str, {}),
			schema_name : Try(Str, {}),
			table_name : Try(Str, {}),
			column_name : Try(Str, {}),
			data_type_name : Try(Str, {}),
			constraint_name : Try(Str, {}),
			file : Try(Str, {}),
			line : Try(Str, {}),
			routine : Try(Str, {}),
		}

		KeyData : { process_id : I32, secret_key : I32 }

		Message : [
			AuthOk,
			AuthCleartextPassword,
			AuthUnsupported,
			ParameterStatus({ name : Str, value : Str }),
			BackendKeyData(KeyData),
			ReadyForQuery(Status),
			ErrorResponse(Error),
			ParseComplete,
			BindComplete,
			NoticeResponse(List({ code : U8, value : Str })),
			NoData,
			RowDescription(List(RowField)),
			ParameterDescription(List(ParameterField)),
			DataRow(List(List(U8))),
			PortalSuspended,
			CommandComplete(Str),
			EmptyQueryResponse,
			CloseComplete,
		]

		ParameterField : {
			data_type_oid : I32,
		}

		RowField : {
			name : Str,
			column : Try({ table_oid : I32, attribute_number : I16 }, [NotAColumn]),
			data_type_oid : I32,
			data_type_size : I16,
			type_modifier : I32,
			format_code : I16,
		}

		Status : [Idle, TransactionBlock, FailedTransactionBlock]

		header : Decode({ msg_type : U8, len : I32 }, _)
		header = Decode.await(
			Decode.u8({}),
			|msg_type| Decode.map(
				Decode.i32({}),
				|len| { msg_type, len: len - 4 },
			),
		)

		message : U8 -> Decode(Message, _)
		message = |msg_type| match msg_type {
			'R' => auth_request
			'S' => param_status
			'K' => backend_key_data
			'Z' => ready_for_query
			'E' => error_response
			'1' => Decode.succeed(ParseComplete)
			'2' => Decode.succeed(BindComplete)
			'N' => notice_response
			'n' => Decode.succeed(NoData)
			'T' => row_description
			't' => parameter_description
			'D' => data_row
			's' => Decode.succeed(PortalSuspended)
			'C' => command_complete
			'I' => Decode.succeed(EmptyQueryResponse)
			'3' => Decode.succeed(CloseComplete)
			_ => Decode.fail(UnrecognizedBackendMessage(msg_type))
		}
	}
}

# Frontend Helpers

encode_param : Str, Str -> List(U8)
encode_param = |key, value| Encode.sequence([
	Encode.c_str(key),
	Encode.c_str(value),
])

format_code : Frontend.FormatCode -> List(U8)
format_code = |code| match code {
	Text => Encode.i16(0)
	Binary => Encode.i16(1)
}

encode_array : List(item), (item -> List(U8)) -> List(U8)
encode_array = |items, item_encode| Encode.sequence([
	Encode.i16(items.len().to_i16_wrap()),
	Encode.sequence(items.map(item_encode)),
])

encode_bytes : List(U8) -> List(U8)
encode_bytes = |value| Encode.sequence([
	Encode.i32(value.len().to_i32_wrap()),
	value,
])

encode_message : U8, List((List(U8))) -> List(U8)
encode_message = |msg_type, content| Encode.sequence([
	Encode.u8(msg_type),
	prepend_length(Encode.sequence(content)),
])

prepend_length : List(U8) -> List(U8)
prepend_length = |msg| {
	total_length = msg.len().to_i32_wrap() + 4
	Encode.i32(total_length).concat(msg)
}

# Backend  Helpers

backend_key_data : Decode(Backend.Message, _)
backend_key_data = Decode.await(
	Decode.i32({}),
	|process_id| Decode.await(
		Decode.i32({}),
		|secret_key|
			Decode.succeed(BackendKeyData({ process_id, secret_key })),
	),
)

ready_for_query : Decode(Backend.Message, _)
ready_for_query = Decode.await(
	Decode.u8({}),
	|status| match status {
		'I' => Decode.succeed(ReadyForQuery(Idle))
		'T' => Decode.succeed(ReadyForQuery(TransactionBlock))
		'E' => Decode.succeed(ReadyForQuery(FailedTransactionBlock))
		_ => Decode.fail(UnrecognizedBackendStatus(status))
	},
)

read_notice_responses : Decode(List({ code : U8, value : Str }), _)
read_notice_responses = Decode.loop(
	[],
	|collected|
		Decode.await(
			Decode.u8({}),
			|code|
				if (code == 0)
					Decode.succeed(Done(collected))
				else
					Decode.map(
						Decode.c_str({}),
						|value|
							Loop(List.append(collected, { code, value })),
					),
		),
)

notice_response = Decode.await(
	read_notice_responses,
	|notices|
		Decode.succeed(NoticeResponse(notices)),
)

# TODO
error_response : Decode(Backend.Message, _)
error_response = known_str_fields.await(
	|dict| 'S'
		|> required_field(
			dict,
			|localized_severity| 'V'
				|> optional_field_with(
					dict,
					decode_severity,
					|severity| 'C'
						|> required_field(
							dict,
							|code| 'M'
								|> required_field(
									dict,
									|msg| 'P'
										|> optional_field_with(
											dict,
											U32.from_str,
											|position| 'p'
												|> optional_field_with(
													dict,
													U32.from_str,
													|internal_position| ErrorResponse({
														localized_severity,
														severity,
														code,
														message: msg,
														detail: 'D' |> optional_field(dict),
														hint: 'H' |> optional_field(dict),
														position,
														internal_position,
														internal_query: 'q' |> optional_field(dict),
														ewhere: 'W' |> optional_field(dict),
														schema_name: 's' |> optional_field(dict),
														table_name: 't' |> optional_field(dict),
														column_name: 'c' |> optional_field(dict),
														data_type_name: 'd' |> optional_field(dict),
														constraint_name: 'n' |> optional_field(dict),
														file: 'F' |> optional_field(dict),
														line: 'L' |> optional_field(dict),
														routine: 'R' |> optional_field(dict),
													})
														|> Decode.succeed,
												),
										),
								),
						),
				),
		),
)

optional_field = |field_id, dict|
	match Dict.get(dict, field_id) {
		Ok(value) =>
			Ok(value)

		Err(_) =>
			Err({})
	}

optional_field_with = |field_id, dict, validate, callback| {
	result = Dict.get(dict, field_id)
	match result {
		Ok(value) => match validate(value) {
			Ok(validated) => callback(Ok(validated))
			Err(err) => Decode.fail(err)
		}
		Err(_) => callback(Err({}))
	}
}

required_field = |field_id, dict, callback| {
	result = Dict.get(dict, field_id)
	match result {
		Ok(value) => callback(value)
		Err(_) => Decode.fail(MissingField(field_id))
	}
}

ErrorSeverity : [
	Error,
	Fatal,
	Panic,
	Warning,
	Notice,
	Debug,
	Info,
	Log,
]

decode_severity : Str -> Try(ErrorSeverity, [InvalidSeverity(Str)])
decode_severity = |str|
	match str {
		"ERROR" => Ok(Error)
		"FATAL" => Ok(Fatal)
		"PANIC" => Ok(Panic)
		"WARNING" => Ok(Warning)
		"NOTICE" => Ok(Notice)
		"DEBUG" => Ok(Debug)
		"INFO" => Ok(Info)
		"LOG" => Ok(Log)
		_ => Err(InvalidSeverity(str))
	}

known_str_fields : Decode(Dict(U8, Str), _)
known_str_fields = Decode.loop(
	Dict.empty(),
	|collected| Decode.await(
		Decode.u8({}),
		|field_id| if (field_id == 0) Decode.succeed(Done(collected)) else Decode.map(
			Decode.c_str({}),
			|value| collected |> Dict.insert(field_id, value) |> Loop,
		),
	),
)

parameter_description : Decode(Backend.Message, _)
parameter_description = Decode.await(
	Decode.i16({}),
	|field_count|
		if (field_count == 0) Decode.succeed(ParameterDescription([]))
		else fixed_list(field_count, parameter_field)
			|> Decode.map(
				|d| ParameterDescription(d),
			),
)

parameter_field : Decode(Backend.ParameterField, _)
parameter_field = Decode.await(
	Decode.i32({}),
	|data_type_oid| Decode.succeed({ data_type_oid: data_type_oid }),
)

row_description : Decode(Backend.Message, _)
row_description =
	Decode.await(
		Decode.i16({}),
		|field_count|
			fixed_list(field_count, row_field) |> Decode.map(|d| RowDescription(d)),
	)

row_field : Decode(Backend.RowField, _)
row_field = Decode.c_str({}).await(
	|name| Decode.i32({}).await(
		|table_oid| Decode.i16({}).await(
			|attribute_number| Decode.i32({}).await(
				|data_type_oid| Decode.i16({}).await(
					|data_type_size| Decode.i32({}).await(
						|type_modifier| Decode.i16({}).map(
							|fmt_code| {
								column =
									if (table_oid != 0 and attribute_number != 0)
										Ok({ table_oid, attribute_number })
									else
										Err(NotAColumn)

								{
									name,
									column,
									data_type_oid,
									data_type_size,
									type_modifier,
									format_code: fmt_code,
								}
							},
						),
					),
				),
			),
		),
	),
)

data_row : Decode(Backend.Message, _)
data_row = Decode.await(
	Decode.i16({}),
	|column_count| fixed_list(
		column_count,
		Decode.await(
			Decode.i32({}),
			|value_len|
				if (value_len == -1)
					Decode.succeed([])
				else
					Decode.take(value_len.to_u64_wrap(), |x| x),
		),
	)
		|> Decode.map(|r| DataRow(r)),
)

fixed_list = |count, item_decode| Decode.loop(
	List.with_capacity(count.to_u64_wrap()),
	|collected| Decode.map(
		item_decode,
		|item| {
			added = List.append(collected, item)

			if (List.len(added) == (count.to_u64_wrap()))
				Done(added)
			else
				Loop(added)
		},
	),
)

command_complete : Decode(Backend.Message, _)
command_complete = Decode.map(Decode.c_str({}), |str| CommandComplete(str))

Msg : []

auth_request : Decode(Backend.Message, _)
auth_request = Decode.map(
	Decode.i32({}),
	|auth_type| match auth_type {
		0 => AuthOk
		3 => AuthCleartextPassword
		_ => AuthUnsupported
	},
)

param_status : Decode(Backend.Message, _)
param_status = Decode.await(
	Decode.c_str({}),
	|name| Decode.await(
		Decode.c_str({}),
		|value|
			Decode.succeed(ParameterStatus({ name, value })),
	),
)
