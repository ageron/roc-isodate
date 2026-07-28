import Const
import Utils

Duration :: { days : I64, hours : I8, minutes : I8, seconds : I8, nanoseconds : I32 }.{
	add : Duration, Duration -> Duration
	add = |d1, d2| {
		nanos1 = to_nanoseconds(d1)
		nanos2 = to_nanoseconds(d2)
		nanos1->add_saturated_i128(nanos2)->from_nanoseconds()
	}

	format : Duration, Str -> Str
	format = |d, fmt| {
		(
			fmt
				.replace_first("{d}", d.days.to_str())
				.replace_first("{hh}", Utils.expand_int_with_zeros(d.hours, 2))
				.replace_first("{h}", d.hours.to_str())
				.replace_first("{mm}", Utils.expand_int_with_zeros(d.minutes, 2))
				.replace_first("{m}", d.minutes.to_str())
				.replace_first("{ss}", Utils.expand_int_with_zeros(d.seconds, 2))
				.replace_first("{s}", d.seconds.to_str())
				.replace_first("{f}", Utils.nanos_to_frac_str(d.nanoseconds).drop_prefix(","))
				->Utils.replace_fx_format(d.nanoseconds.to_i64()),
		).replace_first("{n}", d.nanoseconds.to_str())
	}

	format_unsigned : Duration, Str -> Str
	format_unsigned = |d, fmt| {
		(
			fmt
				.replace_first("{d}", d.days.to_str())
				.replace_first("{hh}", Utils.expand_int_with_zeros(d.hours, 2))
				.replace_first("{h}", d.hours.to_str())
				.replace_first("{mm}", Utils.expand_int_with_zeros(d.minutes, 2))
				.replace_first("{m}", d.minutes.to_str())
				.replace_first("{ss}", Utils.expand_int_with_zeros(d.seconds, 2))
				.replace_first("{s}", d.seconds.to_str())
				.replace_first("{f}", Utils.nanos_to_frac_str(d.nanoseconds).drop_prefix(","))
				->Utils.replace_fx_format(d.nanoseconds.to_i64()),
		).replace_first("{n}", d.nanoseconds.to_str())
			.replace_each("-", "")
	}

	from_days : d -> Duration where [d.to_i64 : d -> I64]
	from_days = |days| {
		days_saturated = 
			if days.to_i128() > I64.highest.to_i128() {
				I64.highest.to_i128()
			} else if days.to_i128() < I64.lowest.to_i128() {
				I64.lowest.to_i128()
			} else {
				days.to_i128()
			}
		{
			days: days_saturated.to_i64(),
			hours: 0,
			minutes: 0,
			seconds: 0,
			nanoseconds: 0,
		}
	}

	from_hms : h, m, s -> Duration where [h.to_i64 : h -> I64, m.to_i64 : m -> I64, s.to_i64 : s -> I64]
	from_hms = |hours, minutes, seconds| {
		from_hours(hours)->add(from_minutes(minutes))->add(from_seconds(seconds))
	}

	from_hmsn : h, m, s, n -> Duration where [h.to_i64 : h -> I64, m.to_i64 : m -> I64, s.to_i64 : s -> I64, n.to_i64 : n -> I64]
	from_hmsn = |hours, minutes, seconds, nanoseconds| {
		from_hours(hours)->add(from_minutes(minutes))->add(from_seconds(seconds))->add(from_nanoseconds(nanoseconds))
	}

	from_hours : h -> Duration where [h.to_i64 : h -> I64]
	from_hours = |hours| {
		hours_saturated = 
			if (hours.to_i128() // 24) > I64.highest.to_i128() {
				I64.highest.to_i128() * 24
			} else if (hours.to_i128() // 24) < I64.lowest.to_i128() {
				I64.lowest.to_i128() * 24
			} else {
				hours.to_i128()
			}
		{
			days: (hours_saturated // 24).to_i64(),
			hours: (hours_saturated % 24).to_i8(),
			minutes: 0,
			seconds: 0,
			nanoseconds: 0,
		}
	}

	from_minutes : m -> Duration where [m.to_i64 : m -> I64]
	from_minutes = |minutes| {
		minutes_saturated = 
			if (minutes.to_i128() // Const.minutes_per_day.to_i128()) > I64.highest.to_i128() {
				I64.highest.to_i128() * Const.minutes_per_day.to_i128()
			} else if (minutes.to_i128() // Const.minutes_per_day.to_i128()) < I64.lowest.to_i128() {
				I64.lowest.to_i128() * Const.minutes_per_day.to_i128()
			} else {
				minutes.to_i128()
			}
		{
			days: (minutes_saturated // Const.minutes_per_day.to_i128()).to_i64(),
			hours: ((minutes_saturated % Const.minutes_per_day.to_i128()) // Const.minutes_per_hour.to_i128()).to_i8(),
			minutes: (minutes_saturated % Const.minutes_per_hour.to_i128()).to_i8(),
			seconds: 0,
			nanoseconds: 0,
		}
	}

	from_nanoseconds : n -> Duration where [n.to_i64 : n -> I64]
	from_nanoseconds = |nanos| {
		nanos_saturated = 
			if (nanos.to_i128() // Const.nanos_per_day.to_i128()) > I64.highest.to_i128() {
				I64.highest.to_i128() * Const.nanos_per_day.to_i128()
			} else if (nanos.to_i128() // Const.nanos_per_day.to_i128()) < I64.lowest.to_i128() {
				I64.lowest.to_i128() * Const.nanos_per_day.to_i128()
			} else {
				nanos.to_i128()
			}
		{
			days: (nanos_saturated // Const.nanos_per_day.to_i128()).to_i64(),
			hours: ((nanos_saturated % Const.nanos_per_day.to_i128()) // Const.nanos_per_hour.to_i128()).to_i8(),
			minutes: ((nanos_saturated % Const.nanos_per_hour.to_i128()) // Const.nanos_per_minute.to_i128()).to_i8(),
			seconds: ((nanos_saturated % Const.nanos_per_minute.to_i128()) // Const.nanos_per_second.to_i128()).to_i8(),
			nanoseconds: (nanos_saturated % Const.nanos_per_second.to_i128()).to_i32(),
		}
	}

	from_seconds : s -> Duration where [s.to_i64 : s -> I64]
	from_seconds = |seconds| {
		seconds_saturated = 
			if (seconds.to_i128() // Const.seconds_per_day.to_i128()) > I64.highest.to_i128() {
				I64.highest.to_i128() * Const.seconds_per_day.to_i128()
			} else if (seconds.to_i128() // Const.seconds_per_day.to_i128()) < I64.lowest.to_i128() {
				I64.lowest.to_i128() * Const.seconds_per_day.to_i128()
			} else {
				seconds.to_i128()
			}
		{
			days: (seconds_saturated // Const.seconds_per_day.to_i128()).to_i64(),
			hours: ((seconds_saturated % Const.seconds_per_day.to_i128()) // Const.seconds_per_hour.to_i128()).to_i8(),
			minutes: ((seconds_saturated % Const.seconds_per_hour.to_i128()) // Const.seconds_per_minute.to_i128()).to_i8(),
			seconds: (seconds_saturated % Const.seconds_per_minute.to_i128()).to_i8(),
			nanoseconds: 0,
		}
	}

	sub : Duration, Duration -> Duration
	sub = |d1, d2| {
		nanos1 = to_nanoseconds(d1)
		nanos2 = to_nanoseconds(d2)
		nanos1->sub_saturated_i128(nanos2)->from_nanoseconds()
	}

	to_days : Duration -> I64
	to_days = |duration| duration.days

	to_hours : Duration -> I64
	to_hours = |duration| {
		duration.hours.to_i64()
			->add_saturated_i64(duration.days.to_i64()->mul_saturated_i64(24))
	}

	to_minutes : Duration -> I64
	to_minutes = |duration| {
		duration.minutes.to_i64()
			->add_saturated_i64(duration.hours.to_i64()->mul_saturated_i64(Const.minutes_per_hour.to_i64()))
			->add_saturated_i64(duration.days->mul_saturated_i64(Const.minutes_per_day.to_i64()))
	}

	to_nanoseconds : Duration -> I128
	to_nanoseconds = |duration| {
		duration.nanoseconds.to_i128()
			->add_saturated_i128(duration.seconds.to_i128()->mul_saturated_i128(Const.nanos_per_second.to_i128()))
			->add_saturated_i128(duration.minutes.to_i128()->mul_saturated_i128(Const.nanos_per_minute.to_i128()))
			->add_saturated_i128(duration.hours.to_i128()->mul_saturated_i128(Const.nanos_per_hour.to_i128()))
			->add_saturated_i128(duration.days.to_i128()->mul_saturated_i128(Const.nanos_per_day.to_i128()))
	}

	to_seconds : Duration -> I64
	to_seconds = |duration| {
		duration.seconds.to_i64()
			->add_saturated_i64(duration.minutes.to_i64()->mul_saturated_i64(Const.seconds_per_minute.to_i64()))
			->add_saturated_i64(duration.hours.to_i64()->mul_saturated_i64(Const.seconds_per_hour.to_i64()))
			->add_saturated_i64(duration.days->mul_saturated_i64(Const.seconds_per_day.to_i64()))
	}
}

add_saturated_i64 = |a, b| {
	if b > 0 and a > I64.highest - b {
		I64.highest
	} else if b < 0 and a < I64.lowest - b {
		I64.lowest
	} else {
		a + b
	}
}

mul_saturated_i64 = |a, b| {
	if a == 0 or b == 0 {
		0
	} else if a > 0 {
		if b > 0 {
			if a > I64.highest // b {
				I64.highest
			} else {
				a * b
			}
		} else {
			if b < I64.lowest // a {
				I64.lowest
			} else {
				a * b
			}
		}
	} else {
		if b > 0 {
			if a < I64.lowest // b {
				I64.lowest
			} else {
				a * b
			}
		} else {
			if a < I64.highest // b {
				I64.highest
			} else {
				a * b
			}
		}
	}
}

add_saturated_i128 = |a, b| {
	max_i128 = 170141183460469231731687303715884105727
	min_i128 = -170141183460469231731687303715884105728
	if b > 0 and a > max_i128 - b {
		max_i128
	} else if b < 0 and a < min_i128 - b {
		min_i128
	} else {
		a + b
	}
}

sub_saturated_i128 = |a, b| {
	max_i128 = 170141183460469231731687303715884105727
	min_i128 = -170141183460469231731687303715884105728
	if b < 0 and a > max_i128 + b {
		max_i128
	} else if b > 0 and a < min_i128 + b {
		min_i128
	} else {
		a - b
	}
}

mul_saturated_i128 = |a, b| {
	max_i128 = 170141183460469231731687303715884105727
	min_i128 = -170141183460469231731687303715884105728
	if a == 0 or b == 0 {
		0
	} else if a > 0 {
		if b > 0 {
			if a > max_i128 // b {
				max_i128
			} else {
				a * b
			}
		} else {
			if b < min_i128 // a {
				min_i128
			} else {
				a * b
			}
		}
	} else {
		if b > 0 {
			if a < min_i128 // b {
				min_i128
			} else {
				a * b
			}
		} else {
			if a < max_i128 // b {
				max_i128
			} else {
				a * b
			}
		}
	}
}

# <===== TESTS ====>
# <---- add ---->
expect {
	d1 = Duration.from_days(I64.highest // 2)
	d2 = Duration.from_days(I64.highest // 2)
	d3 = Duration.from_days((I64.highest // 2) * 2)
	Duration.add(d1, d2) == d3
}

expect {
	d1 = Duration.from_days(I64.lowest)
	d2 = Duration.from_days(I64.highest)
	Duration.add(d1, d2) == Duration.from_days(-1)
}

expect {
	days = I64.highest
	duration = Duration.from_days(days)
	Duration.add(duration, duration) == Duration.from_days(days)
}

# <---- from_hmsn ---->
expect {
	res = Duration.from_hmsn(I64.highest, 0, 0, 0)
	res == Duration.from_days(384307168202282325)->Duration.add(Duration.from_hours(7))
}

expect {
	res = Duration.from_hmsn(0, I64.highest, 0, 0)
	res == Duration.from_days(6405119470038038)->Duration.add(Duration.from_hours(18))->Duration.add(Duration.from_minutes(7))
}

expect {
	res = Duration.from_hmsn(0, 0, I64.highest, 0)
	res == Duration.from_days(106751991167300)->Duration.add(Duration.from_hours(15))->Duration.add(Duration.from_minutes(30))->Duration.add(Duration.from_seconds(7))
}

expect {
	res = Duration.from_hmsn(0, 0, 0, I64.highest)
	res == Duration.from_days(106751)->Duration.add(Duration.from_hours(23))->Duration.add(Duration.from_minutes(47))->Duration.add(Duration.from_seconds(16))->Duration.add(Duration.from_nanoseconds(854775807))
}

expect {
	res = Duration.from_hmsn(I64.highest, I64.highest, I64.highest, I64.highest)
	res == Duration.from_days(390819039663594414)->Duration.add(Duration.from_hours(63))->Duration.add(Duration.from_minutes(84))->Duration.add(Duration.from_seconds(23))->Duration.add(Duration.from_nanoseconds(854775807))
}

# <---- sub ---->
expect {
	d = Duration.from_days(I64.highest)
	Duration.sub(d, d) == Duration.from_days(0)
}

expect {
	d = Duration.from_days(I64.lowest)
	Duration.sub(d, d) == Duration.from_days(0)
}

expect {
	d1 = Duration.from_days(1)
	d2 = Duration.from_days(2)
	Duration.sub(d1, d2) == Duration.from_days(-1)
}

# <---- saturation test ---->
expect {
	days_over = I64.highest.to_i128() + 1
	duration = Duration.from_days(days_over)
	duration.to_days() == I64.highest
}
