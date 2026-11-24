import Protocol

Result :: {
	fields : List(RowField),
	rows : List(List(List(U8))),
	parameters : List(ParameterField),
}.{
	RowField : Protocol.Backend.RowField
	ParameterField : Protocol.Backend.ParameterField

	create = |x| Result.(x)

	fields : Result -> List(RowField)
	fields = |Result.(result)| result.fields

	rows : Result -> List(List(List(U8)))
	rows = |Result.(result)| result.rows

	len : Result -> U64
	len = |Result.(result)| result.rows.len()

	Decode(a, err) :: List(RowField) -> Try(
		List(List(U8)) -> Try(a, [FieldNotFound(Str), ..err]),
		[FieldNotFound(Str)],
	)

	decode : Result, Decode(a, err) -> Try(List(a), [FieldNotFound(Str), ..err])
	decode = |Result.(r), Decode.(get_decode)|
		match get_decode(r.fields) {
			Ok(fn) => r.rows.map_try(fn)
			Err(FieldNotFound(name)) => Err(FieldNotFound(name))
		}

	str = |{}| decoder(|s| Ok(s))

	u8 = |{}| decoder(U8.from_str)

	u16 = |{}| decoder(U16.from_str)

	u32 = |{}| decoder(U32.from_str)

	u64 = |{}| decoder(U64.from_str)

	u128 = |{}| decoder(U128.from_str)

	i8 = |{}| decoder(I8.from_str)

	i16 = |{}| decoder(I16.from_str)

	i32 = |{}| decoder(I32.from_str)

	i64 = |{}| decoder(I64.from_str)

	i128 = |{}| decoder(I128.from_str)

	f32 = |{}| decoder(F32.from_str)

	f64 = |{}| decoder(F64.from_str)

	dec = |{}| decoder(Dec.from_str)

	bool = |{}| decoder(
		|v| match v {
			"t" => Ok(Bool.True)
			"f" => Ok(Bool.False)
			_ => Err(InvalidBoolStr)
		},
	)

	succeed = |value|
		Decode.(|_| Ok(|_| Ok(value)))

	result_with = |a, b| map2(a, b, |fn, val| fn(val))

	apply = |a| |fn| result_with(fn, a)

	map2 : Decode(a, err), Decode(b, err), (a, b -> c) -> Decode(c, err)
	map2 = |Decode.(a), Decode.(b), cb| Decode.(
		|row_fields| {
			decode_a = a(row_fields)?
			decode_b = b(row_fields)?
			Ok(
				|row| {
					value_a = decode_a(row)?
					value_b = decode_b(row)?
					Ok(cb(value_a, value_b))
				},
			)
		},
	)
}

# decoder : (Str -> Try(_, _)) -> (Str -> Result.Decode(_, _))
decoder = |fn| |name| Decode.(
	|row_fields|
		match row_fields.find_first_index(|f| f.name == name) {
			Ok(index) => Ok(
				|row| match row.get(index) {
					Ok(bytes) => {
						str_value = Str.from_utf8(bytes)?
						fn(str_value)
					}
					Err(OutOfBounds) => Err(FieldNotFound(name))
				},
			)

			Err(NotFound) => Err(FieldNotFound(name))
		},
)
