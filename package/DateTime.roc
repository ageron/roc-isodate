import Const
import Date
import Date exposing [Date]
import Duration
import Duration exposing [Duration]
import Time
import Time exposing [Time]
import Utils exposing [split_with_delims]

DateTime :: { date : Date, time : Time }.{
	Interval : { start : DateTime, end : DateTime }

	add : DateTime, Duration -> DateTime
	add = add_duration

	add_days : DateTime, d -> DateTime where [d.to_i16 : d -> I16]
	add_days = |date_time, days| { date: Date.add_days(date_time.date, days), time: date_time.time }

	add_duration : DateTime, Duration -> DateTime
	add_duration = |date_time, duration| {
		duration_nanos = duration.to_nanoseconds()
		date_nanos = Date.to_nanos_since_epoch(date_time.date)
		time_nanos = Time.to_nanos_since_midnight(date_time.time).to_i128()
		from_nanos_since_epoch(duration_nanos + date_nanos + time_nanos)
	}

	add_minutes : DateTime, m -> DateTime where [m.to_i64 : m -> I64]
	add_minutes = |date_time, minutes| add_nanoseconds(date_time, minutes.to_i64() * Const.nanos_per_minute)

	add_months : DateTime, m -> DateTime where [m.to_u64 : m -> U64]
	add_months = |date_time, months| { date: Date.add_months(date_time.date, months), time: date_time.time }

	add_hours : DateTime, h -> DateTime where [h.to_i64 : h -> I64]
	add_hours = |date_time, hours| add_nanoseconds(date_time, hours.to_i64() * Const.nanos_per_hour)

	add_nanoseconds : DateTime, n -> DateTime where [n.to_i64 : n -> I64]
	add_nanoseconds = |date_time, nanos| {
		time_nanos = Time.to_nanos_since_midnight(date_time.time) + nanos.to_i64()
		days = 
			if time_nanos >= 0 {
				time_nanos // Const.nanos_per_day.to_i64()
			} else {
				(time_nanos // Const.nanos_per_day.to_i64()) + (
					if time_nanos % Const.nanos_per_day.to_i64() < 0 {
						-1
					} else {
						0
					},
				)
			}
		{ date: Date.add_days(date_time.date, days), time: Time.from_nanos_since_midnight(time_nanos)->Time.normalize() }
	}

	add_seconds : DateTime, s -> DateTime where [s.to_i64 : s -> I64]
	add_seconds = |date_time, seconds| add_nanoseconds(date_time, seconds.to_i64() * Const.nanos_per_second)

	add_years : DateTime, y -> DateTime where [y.to_i64 : y -> I64]
	add_years = |date_time, years| { date: Date.add_years(date_time.date, years), time: date_time.time }

	after : DateTime, DateTime -> Bool
	after = |a, b| compare(a, b) == GT

	before : DateTime, DateTime -> Bool
	before = |a, b| compare(a, b) == LT

	compare : DateTime, DateTime -> [LT, EQ, GT]
	compare = |a, b| {
		Date.compare(a.date, b.date)
			->(|result| if result != EQ {
				result
			} else {
				Time.compare(a.time, b.time)
			})
	}

	equal : DateTime, DateTime -> Bool
	equal = |a, b| compare(a, b) == EQ

	format : DateTime, Str -> Str
	format = |dt, fmt| {
		time_fmt = Date.format(dt.date, fmt)
		Time.format(dt.time, time_fmt)
	}

	from_iso_str : Str -> Try(DateTime, [InvalidDateTimeFormat])
	from_iso_str = |str| str.to_utf8()->from_iso_u8()

	from_iso_u8 : List(U8) -> Try(DateTime, [InvalidDateTimeFormat])
	from_iso_u8 = |bytes| {
		match split_with_delims(bytes, |b| b == 'T') {
			[date_bytes, ['T'], time_bytes] => {
				match (Date.from_iso_u8(date_bytes), Time.from_iso_u8(time_bytes)) {
					(Ok(date), Ok(time)) => {
						{ date, time }->normalize()->Ok()
					}
					_ => Err(InvalidDateTimeFormat)
				}
			}
			[date_bytes] => {
				match Date.from_iso_u8(date_bytes) {
					Ok(date) => {
						{ date, time: Time.from_hms(0, 0, 0) }->Ok()
					}
					_ => Err(InvalidDateTimeFormat)
				}
			}
			_ => Err(InvalidDateTimeFormat)
		}
	}

	from_nanos_since_epoch : n -> DateTime where [n.to_i64 : n -> I64]
	from_nanos_since_epoch = |nanos| {
		nanos_i64 = nanos.to_i64()
		time_nanos = 
			if nanos_i64 < 0 and nanos.to_i128() % Const.nanos_per_day.to_i128() != 0 {
				nanos_i64 % Const.nanos_per_day.to_i64() + Const.nanos_per_day.to_i64()
			} else {
				nanos_i64 % Const.nanos_per_day.to_i64()
			}
		date_nanos = nanos_i64 - time_nanos
		date = Date.from_nanos_since_epoch(date_nanos)
		time = Time.from_nanos_since_midnight(time_nanos)
		{ date, time }
	}

	from_yd : y, d -> DateTime where [y.to_i64 : y -> I64, d.to_i64 : d -> I64]
	from_yd = |year, day| { date: Date.from_yd(year, day), time: Time.midnight }

	from_ymd : y, m, d -> DateTime where [y.to_i64 : y -> I64, m.to_i64 : m -> I64, d.to_i64 : d -> I64]
	from_ymd = |year, month, day| { date: Date.from_ymd(year, month, day), time: Time.midnight }

	from_yw : y, w -> DateTime where [y.to_i64 : y -> I64, w.to_i64 : w -> I64]
	from_yw = |year, week| { date: Date.from_yw(year, week), time: Time.midnight }

	from_ywd : y, w, d -> DateTime where [y.to_i64 : y -> I64, w.to_i64 : w -> I64, d.to_i64 : d -> I64]
	from_ywd = |year, week, day| { date: Date.from_ywd(year, week, day), time: Time.midnight }

	from_ymdhms : y, m, d, h, mi, s -> DateTime where [y.to_i64 : y -> I64, m.to_i64 : m -> I64, d.to_i64 : d -> I64, h.to_i64 : h -> I64, mi.to_i64 : mi -> I64, s.to_i64 : s -> I64]
	from_ymdhms = |year, month, day, hour, minute, second| {
		{ date: Date.from_ymd(year, month, day), time: Time.from_hms(hour, minute, second) }
	}

	from_ymdhmsn : y, m, d, h, mi, s, n -> DateTime where [y.to_i64 : y -> I64, m.to_i64 : m -> I64, d.to_i64 : d -> I64, h.to_i64 : h -> I64, mi.to_i64 : mi -> I64, s.to_i64 : s -> I64, n.to_i64 : n -> I64]
	from_ymdhmsn = |year, month, day, hour, minute, second, nanosecond| {
		{ date: Date.from_ymd(year, month, day), time: Time.from_hmsn(hour, minute, second, nanosecond) }
	}

	sub : DateTime, DateTime -> Duration
	sub = |a, b| {
		a_nanos = to_nanos_since_epoch(a)
		b_nanos = to_nanos_since_epoch(b)
		Duration.from_nanoseconds(a_nanos - b_nanos)
	}

	to_iso_str : DateTime -> Str
	to_iso_str = |date_time| {
		Date.to_iso_str(date_time.date).concat("T").concat(Time.to_iso_str(date_time.time))
	}

	to_iso_u8 : DateTime -> List(U8)
	to_iso_u8 = |date_time| {
		Date.to_iso_u8(date_time.date).concat(['T']).concat(Time.to_iso_u8(date_time.time))
	}

	to_nanos_since_epoch : DateTime -> I128
	to_nanos_since_epoch = |date_time| {
		date_nanos = Date.to_nanos_since_epoch(date_time.date)
		time_nanos = Time.to_nanos_since_midnight(date_time.time).to_i128()
		date_nanos + time_nanos
	}

	unix_epoch : DateTime
	unix_epoch = { date: Date.unix_epoch, time: Time.midnight }

	weekday : DateTime -> U8
	weekday = |dt| Date.weekday(dt.date)
}

normalize : DateTime -> DateTime
normalize = |date_time| {
	DateTime.add_hours(
		{
			date: date_time.date,
			time: Time.from_hmsn(0, date_time.time.minute, date_time.time.second, date_time.time.nanosecond),
		},
		date_time.time.hour.to_i64(),
	)
}

# <==== TESTS ====>
# <---- add_nanoseconds ---->
expect DateTime.from_ymdhmsn(1970, 1, 1, 0, 0, 0, 0).add_nanoseconds(1) == DateTime.from_ymdhmsn(1970, 1, 1, 0, 0, 0, 1)
expect DateTime.from_ymdhmsn(1970, 1, 1, 0, 0, 0, 0).add_nanoseconds(Const.nanos_per_second) == DateTime.from_ymdhmsn(1970, 1, 1, 0, 0, 1, 0)
expect DateTime.from_ymdhmsn(1970, 1, 1, 0, 0, 0, 0).add_nanoseconds(Const.nanos_per_day) == DateTime.from_ymdhmsn(1970, 1, 2, 0, 0, 0, 0)
expect DateTime.from_ymdhmsn(1970, 1, 1, 0, 0, 0, 0).add_nanoseconds(-1) == DateTime.from_ymdhmsn(1969, 12, 31, 23, 59, 59, (Const.nanos_per_second - 1))
expect DateTime.from_ymdhmsn(1970, 1, 1, 0, 0, 0, 0).add_nanoseconds(-Const.nanos_per_day) == DateTime.from_ymdhmsn(1969, 12, 31, 0, 0, 0, 0)
expect DateTime.from_ymdhmsn(1970, 1, 1, 0, 0, 0, 0).add_nanoseconds(((-Const.nanos_per_day) - 1)) == DateTime.from_ymdhmsn(1969, 12, 30, 23, 59, 59, (Const.nanos_per_second - 1))

# <---- add_duration ---->
expect DateTime.add_duration(DateTime.unix_epoch, Duration.from_nanoseconds(-1)) == DateTime.from_ymdhmsn(1969, 12, 31, 23, 59, 59, (Const.nanos_per_second - 1))
expect DateTime.add_duration(DateTime.unix_epoch, Duration.from_days(365)) == DateTime.from_ymdhmsn(1971, 1, 1, 0, 0, 0, 0)

# <--- after --->
expect {
	a = DateTime.from_nanos_since_epoch(0)
	b = DateTime.from_nanos_since_epoch(0)
	!(a.after(b))
}
expect {
	a = DateTime.from_nanos_since_epoch(0)
	b = DateTime.from_nanos_since_epoch(1)
	!(a.after(b))
}
expect {
	a = DateTime.from_nanos_since_epoch(1)
	b = DateTime.from_nanos_since_epoch(0)
	a.after(b)
}

# <--- before --->
expect {
	a = DateTime.from_nanos_since_epoch(0)
	b = DateTime.from_nanos_since_epoch(0)
	!(a.before(b))
}
expect {
	a = DateTime.from_nanos_since_epoch(0)
	b = DateTime.from_nanos_since_epoch(1)
	a.before(b)
}
expect {
	a = DateTime.from_nanos_since_epoch(1)
	b = DateTime.from_nanos_since_epoch(0)
	!(a.before(b))
}

# <--- equal --->
expect {
	a = DateTime.from_nanos_since_epoch(0)
	b = DateTime.from_nanos_since_epoch(0)
	a.equal(b)
}
expect {
	a = DateTime.from_nanos_since_epoch(0)
	b = DateTime.from_nanos_since_epoch(1)
	!(a.equal(b))
}
expect {
	a = DateTime.from_nanos_since_epoch(1)
	b = DateTime.from_nanos_since_epoch(0)
	!(a.equal(b))
}

# <--- format --->
expect {
	res = DateTime.format(DateTime.from_ymdhmsn(1970, 1, 1, 1, 1, 1, 123456789), "{YYYY}-{MM}-{DD}T{hh}:{mm}:{ss}.{f:3}")
	res == "1970-01-01T01:01:01.123"
}

# <--- from_nanos_since_epoch --->
expect DateTime.from_nanos_since_epoch((364 * 24 * Const.nanos_per_hour + 12 * Const.nanos_per_hour + 34 * Const.nanos_per_minute + 56 * Const.nanos_per_second + 5)) == DateTime.from_ymdhmsn(1970, 12, 31, 12, 34, 56, 5)
expect DateTime.from_nanos_since_epoch(-1) == DateTime.from_ymdhmsn(1969, 12, 31, 23, 59, 59, (Const.nanos_per_second - 1))

# <--- normalize --->
expect DateTime.normalize(DateTime.from_ymdhmsn(1970, 1, 2, -12, 1, 2, 3)) == DateTime.from_ymdhmsn(1970, 1, 1, 12, 1, 2, 3)
expect DateTime.normalize(DateTime.from_ymdhmsn(1970, 1, 1, 12, 1, 2, 3)) == DateTime.from_ymdhmsn(1970, 1, 1, 12, 1, 2, 3)
expect DateTime.normalize(DateTime.from_ymdhmsn(1970, 1, 1, 36, 1, 2, 3)) == DateTime.from_ymdhmsn(1970, 1, 2, 12, 1, 2, 3)

# <--- sub --->
expect DateTime.sub(DateTime.from_ymd(1970, 1, 1), DateTime.from_ymdhmsn(1970, 1, 1, 0, 0, 0, 1)) == Duration.from_nanoseconds(-1)
expect DateTime.sub(DateTime.from_ymdhmsn(1970, 1, 1, 1, 1, 1, 1), DateTime.from_yd(1968, 1)) == Duration.from_nanoseconds(Const.nanos_per_day * 731 + Const.nanos_per_hour + Const.nanos_per_minute + Const.nanos_per_second + 1)

# <---- to_iso_str ---->
expect DateTime.to_iso_str(DateTime.unix_epoch) == "1970-01-01T00:00:00"
expect DateTime.to_iso_str(DateTime.from_ymdhmsn(1970, 1, 1, 0, 0, 0, (Const.nanos_per_second // 2))) == "1970-01-01T00:00:00,5"

# <---- to_iso_u8 ---->
expect DateTime.to_iso_u8(DateTime.unix_epoch) == Str.to_utf8("1970-01-01T00:00:00")

# <--- to_nanos_since_epoch --->
expect DateTime.to_nanos_since_epoch(DateTime.from_ymdhmsn(1970, 12, 31, 12, 34, 56, 5)) == 364 * Const.nanos_per_day + 12 * Const.nanos_per_hour + 34 * Const.nanos_per_minute + 56 * Const.nanos_per_second + 5
