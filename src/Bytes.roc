Bytes :: [].{
	Encode :: [].{
		sequence : List(List(U8)) -> List(U8)
		sequence = |list| list.fold([], |acc, bytes| acc.concat(bytes))

		# Unsigned integers

		u8 : U8 -> List(U8)
		u8 = |value| List.single(value)

		u16 : U16 -> List(U8)
		u16 = |value| sized(value, 16)

		u32 : U32 -> List(U8)
		u32 = |value| sized(value, 32)

		u64 : U64 -> List(U8)
		u64 = |value| sized(value, 64)

		# Signed integers

		i8 : I8 -> List(U8)
		i8 = |value| List.single(value.to_u8_wrap())

		i16 : I16 -> List(U8)
		i16 = |value| sized(value, 16)

		i32 : I32 -> List(U8)
		i32 = |value| sized(value, 32)

		i64 : I64 -> List(U8)
		i64 = |value| sized(value, 64)

		# Strings

		c_str : Str -> List(U8)
		c_str = |value| value.to_utf8() |> null_terminate

		null_terminate : List(U8) -> List(U8)
		null_terminate = |bytes| bytes.append(0)
	}

	Decode(value, err) :: List(U8) -> Try(
		{
			decoded : value,
			remaining : List(U8),
		},
		err,
	).{
		decode : List(U8), Decode(value, err) -> Try(value, err)
		decode = |bytes, Decode.(decode_decoder)|
			decode_decoder(bytes).map_ok(|ok| ok.decoded)

		# Unsigned Integers

		u8 : {} -> Decode(U8, [UnexpectedEnd])
		u8 = |{}| Decode.(
			|bytes| match bytes {
				[byte, ..] => Ok({
					decoded: byte,
					remaining: bytes.drop_first(1),
				})
				_ => Err(UnexpectedEnd)
			},
		)

		u16 : {} -> Decode(U16, [UnexpectedEnd])
		u16 = |{}| Decode.(
			|bytes| match bytes {
				[b0, b1, ..] => Ok({
					decoded: b0.to_u16().shl_wrap(8)
						.bitwise_or(b1.to_u16()),
					remaining: bytes.drop_first(2),
				})
				_ => Err(UnexpectedEnd)
			},
		)

		u32 : {} -> Decode(U32, [UnexpectedEnd])
		u32 = |{}| Decode.(
			|bytes| match bytes {
				[b0, b1, b2, b3, ..] => Ok({
					decoded: b0.to_u32().shl_wrap(24)
						.bitwise_or(b1.to_u32().shl_wrap(16))
						.bitwise_or(b2.to_u32().shl_wrap(8))
						.bitwise_or(b3.to_u32()),
					remaining: bytes.drop_first(4),
				})
				_ => Err(UnexpectedEnd)
			},
		)

		u64 : {} -> Decode(U64, [UnexpectedEnd])
		u64 = |{}| Decode.(
			|bytes| match bytes {
				[b0, b1, b2, b3, b4, b5, b6, b7, ..] => Ok({
					decoded: b0.to_u64().shl_wrap(56)
						.bitwise_or(b1.to_u64().shl_wrap(48))
						.bitwise_or(b2.to_u64().shl_wrap(40))
						.bitwise_or(b3.to_u64().shl_wrap(32))
						.bitwise_or(b4.to_u64().shl_wrap(24))
						.bitwise_or(b5.to_u64().shl_wrap(16))
						.bitwise_or(b6.to_u64().shl_wrap(8))
						.bitwise_or(b7.to_u64()),
					remaining: bytes.drop_first(8),
				})
				_ => Err(UnexpectedEnd)
			},
		)

		# Signed Integers

		i8 : {} -> Decode(I8, [UnexpectedEnd])
		i8 = |{}| map(u8({}), U8.to_i8_wrap)

		i16 : {} -> Decode(I16, [UnexpectedEnd])
		i16 = |{}| map(u16({}), U16.to_i16_wrap)

		i32 : {} -> Decode(I32, [UnexpectedEnd])
		i32 = |{}| map(u32({}), U32.to_i32_wrap)

		i64 : {} -> Decode(I64, [UnexpectedEnd])
		i64 = |{}| map(u64({}), U64.to_i64_wrap)

		take : U64, (List(U8) -> value) -> Decode(value, [UnexpectedEnd])
		take = |count, callback| Decode.(
			|bytes| {
				{ before, others } = bytes.split_at(count)
				if (before.len() == count)
					Ok({ decoded: callback(before), remaining: others })
				else
					Err(UnexpectedEnd)
			},
		)

		# Strings

		c_str : {} -> Decode(Str, [TerminatorNotFound, Utf8DecodeError(_)])
		c_str = |{}| Decode.(
			|bytes| match bytes.split_first(0) {
				Ok({ before, after }) => match Str.from_utf8(before) {
					Ok(value) => Ok({ decoded: value, remaining: after })
					Err(err) => Err(Utf8DecodeError(err))
				}
				Err(_) => Err(TerminatorNotFound)
			},
		)

		# Bools

		bool : {} -> Decode(Bool, [UnexpectedEnd])
		bool = |{}| Decode.(
			|bytes| match bytes {
				[byte, ..] => Ok({
					decoded: byte == 1,
					remaining: bytes.drop_first(1),
				})
				_ => Err(UnexpectedEnd)
			},
		)

		succeed : value -> Decode(value, err)
		succeed = |value| Decode.(
			|bytes| Ok({
				decoded: value,
				remaining: bytes,
			}),
		)

		fail : err -> Decode(value, err)
		fail = |err| Decode.(|_| Err(err))

		await : Decode(a, err), (a -> Decode(b, err)) -> Decode(b, err)
		await = |Decode.(decoder_a), callback| Decode.(
			|bytes| {
				a = decoder_a(bytes)?
				# TODO: Decode.(decoder_b) = callback(a.decoded) should work
				match callback(a.decoded) {
					Decode.(decoder_b) => decoder_b(a.remaining)
				}
			},
		)

		map : Decode(a, err), (a -> b) -> Decode(b, err)
		map = |Decode.(map_decoder), map_fn| Decode.(
			|bytes| map_decoder(bytes).map_ok(
				|{ decoded, remaining }| {
					decoded: map_fn(decoded),
					remaining,
				},
			),
		)

		# Loop

		loop : state, (state -> Decode(Step(state, a), err)) -> Decode(a, err)
		loop = |state, step| Decode.(|bytes| loop_help(step, state, bytes))

	}
}

sized : int, U8 -> List(U8)
	where [
		int.shr_zf_wrap : int, U8 -> int,
		int.to_u8_wrap : int -> U8,
	]
sized = |value, size| sized_help(value, (size - 8), [])

sized_help : int, U8, List(U8) -> List(U8)
	where [
		int.shr_zf_wrap : int, U8 -> int,
		int.to_u8_wrap : int -> U8,
	]
sized_help = |value, offset, collected| {
	part = value.shr_zf_wrap(offset).to_u8_wrap()
	added = collected.append(part)

	if offset == 0 {
		added
	} else {
		sized_help(value, (offset - 8), added)
	}
}

Step(state, a) : [Loop(state), Done(a)]

loop_help : (state -> Decode(Step(state, a), err)), state, List(U8) -> _
loop_help = |step, state, bytes| {
	Decode.(loop_help_decoder) = step(state)

	{ decoded, remaining } = loop_help_decoder(bytes)?

	match decoded {
		Loop(new_state) =>
			loop_help(step, new_state, remaining)
		Done(result) =>
			Ok({ decoded: result, remaining })
	}
}
