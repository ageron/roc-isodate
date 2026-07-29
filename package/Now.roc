## A small effectful module to get the current date/time as Date, Time, or DateTime objects in a single call.
## ```
## import pf.Utc
## import dt.Now 
##
## now = Now.create({
##     now!: Utc.now!,
##     now_to_nanos: Utc.to_nanos_since_epoch,
## })
## dt = now.date_time!()
## ```
import Const
import DateTime exposing [DateTime]
import Date exposing [Date]
import Time exposing [Time]

Now(a) :: { now! : {} => a, now_to_nanos : a -> U64 }.{
	create : { now! : {} => a, now_to_nanos : a -> U64 } -> Now(a)
	create = |{ now!, now_to_nanos }| { now!, now_to_nanos }

	## Convert a raw time value and converter function to a `DateTime`.
	date_time_from : a, (a -> U64) -> DateTime
	date_time_from = |val, now_to_nanos| {
		val
			->now_to_nanos()
			.to_i128()
			->DateTime.from_nanos_since_epoch()
	}

	## Convert a raw time value and converter function to a `Date`.
	date_from : a, (a -> U64) -> Date
	date_from = |val, now_to_nanos| {
		val
			->now_to_nanos()
			.to_i128()
			->Date.from_nanos_since_epoch()
	}

	## Convert a raw time value and converter function to a `Time`.
	time_from : a, (a -> U64) -> Time
	time_from = |val, now_to_nanos| {
		ns = val->now_to_nanos()
		(ns % Const.nanos_per_day.to_u64_wrap())
			.to_i64_wrap()
			->Time.from_nanos_since_midnight()
	}

	## Get the current system time as a `DateTime`.
	date_time! : Now(a) => DateTime
	date_time! = |{ now!, now_to_nanos }| date_time_from(now!({}), now_to_nanos)

	## Get the current system time as a `Date`.
	date! : Now(a) => Date
	date! = |{ now!, now_to_nanos }| date_from(now!({}), now_to_nanos)

	## Get the current system time as a `Time`.
	time! : Now(a) => Time
	time! = |{ now!, now_to_nanos }| time_from(now!({}), now_to_nanos)
}

# <===== TESTS ====>
expect {
	mock_now = Now.create({
		now!: |_| 0.U64,
		now_to_nanos: |x| x,
	})
	Now.date_time_from(0.U64, mock_now.now_to_nanos) == DateTime.unix_epoch
}

expect {
	mock_now = Now.create({
		now!: |_| 0.U64,
		now_to_nanos: |x| x,
	})
	Now.date_from(0.U64, mock_now.now_to_nanos) == Date.unix_epoch
}

expect {
	mock_now = Now.create({
		now!: |_| 0.U64,
		now_to_nanos: |x| x,
	})
	Now.time_from(0.U64, mock_now.now_to_nanos) == Time.midnight
}

expect {
	nanos = (Const.nanos_per_day + Const.nanos_per_hour + Const.nanos_per_minute + Const.nanos_per_second + 500_000_000).to_u64_wrap()
	mock_now = Now.create({
		now!: |_| { ts: nanos },
		now_to_nanos: |obj| obj.ts,
	})
	Now.date_time_from({ ts: nanos }, mock_now.now_to_nanos) == DateTime.from_ymdhmsn(1970, 1, 2, 1, 1, 1, 500_000_000)
}

expect {
	nanos = (Const.nanos_per_day + Const.nanos_per_hour + Const.nanos_per_minute + Const.nanos_per_second + 500_000_000).to_u64_wrap()
	mock_now = Now.create({
		now!: |_| { ts: nanos },
		now_to_nanos: |obj| obj.ts,
	})
	Now.date_from({ ts: nanos }, mock_now.now_to_nanos) == Date.from_ymd(1970, 1, 2)
}

expect {
	nanos = (Const.nanos_per_day + Const.nanos_per_hour + Const.nanos_per_minute + Const.nanos_per_second + 500_000_000).to_u64_wrap()
	mock_now = Now.create({
		now!: |_| { ts: nanos },
		now_to_nanos: |obj| obj.ts,
	})
	Now.time_from({ ts: nanos }, mock_now.now_to_nanos) == Time.from_hmsn(1, 1, 1, 500_000_000)
}
