import Const
import DateTime exposing [DateTime]
import Date exposing [Date]
import Time exposing [Time]

Now(a) :: { now! : {} => a, now_to_nanos : a -> U64 }.{
	create : { now! : {} => a, now_to_nanos : a -> U64 } -> Now(a)
	create = |{ now!, now_to_nanos }| { now!, now_to_nanos }

	date_time! : Now(a) => DateTime
	date_time! = |{ now!, now_to_nanos }| {
		now!({})
			->now_to_nanos()
			->DateTime.from_nanos_since_epoch()
	}

	date! : Now(a) => Date
	date! = |{ now!, now_to_nanos }| {
		now!({})
			->now_to_nanos()
			->Date.from_nanos_since_epoch()
	}

	time! : Now(a) => Time
	time! = |{ now!, now_to_nanos }| {
		ns = now!({})->now_to_nanos()
		(ns % Const.nanos_per_day)
			->Time.from_nanos_since_midnight()
	}
}
