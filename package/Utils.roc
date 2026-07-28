Utils :: {}.{
	nanos_to_frac_str : I32 -> Str
	nanos_to_frac_str = |nanos| {
		length = count_frac_width(nanos)
		num_str = trim_to_last_sig_fig(nanos).drop_prefix("-")->pad_left_ascii('0', length)
		untrimmed_str = if nanos == 0 {
			""
		} else {
			Str.concat(",", num_str)
		}
		untrimmed_str.to_utf8().take_first(length + 1)->Str.from_utf8_lossy()
	}

	replace_fx_format : Str, I32 -> Str
	replace_fx_format = |str, nanos| {
		frac_fmt = get_frac_format(str)
		if frac_fmt == "" {
			str
		} else {
			len = parse_frac_fmt(frac_fmt)
			frac_str = nanos_to_frac_str(nanos).drop_prefix(",").to_utf8().take_first(len)->Str.from_utf8_lossy()
			str->replace_first(frac_fmt, frac_str)
		}
	}

	validate_utf8_single_bytes : List(U8) -> Bool
	validate_utf8_single_bytes = |u8_list| u8_list.all(|b| b < 128)

	utf8_to_int : List(U8) -> Try(U64, [InvalidBytes])
	utf8_to_int = |u8_list| {
		u8_list
			.rev()
			.fold_with_index(
				Ok(0),
				|num_result, byte, index| {
					num = num_result?
					if 0x30 <= byte and byte <= 0x39 {
						Ok(num + (byte.to_u64() - 0x30) * (10->pow_int(index)))
					} else {
						Err(InvalidBytes)
					}
				},
			)
	}

	utf8_to_int_signed : List(U8) -> Try(I64, [InvalidBytes])
	utf8_to_int_signed = |u8_list| {
		match u8_list {
			['-', .. as xs] => {
				num = utf8_to_int(xs)?
				Ok(-1 * num.to_i64_wrap())
			}
			['+', .. as xs] => {
				num = utf8_to_int(xs)?
				Ok(num.to_i64_wrap())
			}
			_ => {
				num = utf8_to_int(u8_list)?
				Ok(num.to_i64_wrap())
			}
		}
	}

	utf8_to_frac : List(U8) -> Try(F64, [InvalidBytes])
	utf8_to_frac = |u8_list| {
		match split_with_delims(u8_list, |b| b == ',' or b == '.') {
			[head, [byte], tail] if byte == ',' or byte == '.' => {
				int_part = utf8_to_int(head)?
				frac_part = utf8_to_int(tail)?
				decimal_shift = tail.len().to_u8_wrap()
				Ok(int_part.to_f64() + move_decimal_point(frac_part.to_f64(), decimal_shift))
			}

			[[','], tail] => { # if byte == ',' || byte == '.' -> # crashes when using byte comparison (TODO: check this is still true)
				frac_part = utf8_to_int(tail)?
				decimal_shift = tail.len().to_u8_wrap()
				Ok(move_decimal_point(frac_part.to_f64(), decimal_shift))
			}

			[['.'], tail] => { # if byte == ',' || byte == '.' -> # crashes when using byte comparison (TODO: check this is still true)
				frac_part = utf8_to_int(tail)?
				decimal_shift = tail.len().to_u8_wrap()
				Ok(move_decimal_point(frac_part.to_f64(), decimal_shift))
			}

			[head, [byte]] if byte == ',' or byte == '.' => {
				int_part = utf8_to_int(head)?
				Ok(int_part.to_f64())
			}

			_ => {
				int_part = utf8_to_int(u8_list)?
				Ok(int_part.to_f64())
			}
		}
	}

	expand_int_with_zeros = |num, target_length| {
		num.to_str().pad_left_ascii('0', target_length)
	}

	split_at_indices = |list, indices| {
		help = |output, remaining_items, remaining_indices, current_index| {
			match remaining_indices {
				[] => output.append(remaining_items)
				[index, .. as other_indices] => {
					split_index = if index >= current_index {
						index - current_index
					} else {
						0
					}
					{ before, others } = remaining_items.split_at(split_index)
					help(output.append(before), others, other_indices, current_index + index)
				}
			}
		}
		help([], list, indices.sort_with(|a, b| a.compare(b)), 0)
	}

	split_with_delims = |list, is_delim| {
		split_with_delims_help(list, is_delim, [], [])
	}
}

split_with_delims_help = |list, check_delim, acc, curr| {
	match list {
		[] => {
			if curr.len() == 0 {
				acc
			} else {
				acc.concat([curr])
			}
		}
		[x, .. as xs] => {
			if check_delim(x) {
				new_acc = 
					if curr.len() == 0 {
						acc.concat([[x]])
					} else {
						acc.concat([curr, [x]])
					}
				split_with_delims_help(xs, check_delim, new_acc, [])
			} else {
				split_with_delims_help(xs, check_delim, acc, curr.append(x))
			}
		}
	}
}

pad_left_ascii : Str, U8, U64 -> Str
pad_left_ascii = |str, char, target_length| {
	str_len = str.to_utf8().len()
	if str_len >= target_length {
		str
	} else {
		pad_len = target_length - str_len
		padding = List.repeat(char, pad_len)
		padding_str = padding->Str.from_utf8_lossy()
		padding_str.concat(str)
	}
}

trim_to_last_sig_fig : I32 -> Str
trim_to_last_sig_fig = |num| {
	num.to_str()->drop_trailing_zeros()
}

drop_trailing_zeros : Str -> Str
drop_trailing_zeros = |str| {
	str.to_utf8()->drop_trailing_zeros_help()->Str.from_utf8_lossy()
}

drop_trailing_zeros_help : List(U8) -> List(U8)
drop_trailing_zeros_help = |bytes| {
	match bytes {
		[.. as head, '0'] => drop_trailing_zeros_help(head)
		_ => bytes
	}
}

count_frac_width = |num| {
	9 - count_frac_width_help(num, 0)
}

count_frac_width_help = |num, width| {
	if num == 0 {
		0
	} else if num % 10 == 0 {
		count_frac_width_help((num // 10), (width + 1))
	} else {
		width
	}
}

get_frac_format : Str -> Str
get_frac_format = |str| {
	bytes = str.to_utf8()
	(first, last, _) = 
		bytes.fold_with_index_until(
			(0, 0, Bool.False),
			|(start, end, is_frac), c, i| {
				if c == '{' {
					match (bytes.get(i + 1), bytes.get(i + 2)) {
						(Ok('f'), Ok(':')) => Continue((i, i, Bool.True))
						_ => Continue((start, end, is_frac))
					}
				} else if c == '}' and is_frac {
					Break((start, i, is_frac))
				} else {
					Continue((start, end, is_frac))
				}
			},
		)
	if first != last {
		bytes.sublist({ start: first, len: last - first + 1 })->Str.from_utf8_lossy()
	} else {
		""
	}
}

parse_frac_fmt : Str -> U64
parse_frac_fmt = |fmt| {
	fmt.drop_prefix("{f:").drop_suffix("}")->U64.from_str() ?? 9
}

move_decimal_point : F64, U8 -> F64
move_decimal_point = |num, digits| {
	match digits {
		0 => num
		_ => (move_decimal_point(num, digits - 1)) / 10
	}
}

pow_int = |base, exp| {
	pow_int_help(base, exp, 1)
}

pow_int_help = |base, exp, acc| {
	match exp {
		0 => acc
		_ => pow_int_help(base, exp - 1, acc * base)
	}
}

replace_first = |str, from, to| {
	match str.split_on(from) {
		[] => str
		[_] => str
		[first, .. as rest] => {
			after = rest->Str.join_with(from)
			[first, after]->Str.join_with(to)
		}
	}
}
