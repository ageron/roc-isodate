app [main!] {
	pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/0.9/8GdFEvQYS3TeAZxKvTzCLVdQiomweGtXcdZkXNDEeABq.tar.zst",
	http: "https://github.com/roc-lang/http/releases/download/2.0.0/6ZUwqYhCS8PU9Mo6MF7oV82ET2o7KYb57CLKDq4cq4sS.tar.zst",
	dt: "../package/main.roc",
}

import pf.Stdout
import http.Method
import http.Request

import dt.DateTime as DT

# Temporary workaround until the platform provides pf.Send
Send :: {}.{
	send! = |_| {
		Ok({
			status: 200,
			body:
				\\{"timezone":"America/Chicago","current_utc_offset_seconds":-18000,"standard_utc_offset_seconds":-21600,"dst_utc_offset_seconds":-18000,"has_dst":true,"dst_offset_seconds":3600,"dst_active":true,"dst_from":"2026-03-08T08:00:00.000000+00:00","dst_until":"2026-11-01T07:00:00.000000+00:00","local_time":"2026-07-29T16:24:52.929432-05:00","day_of_week":"Wednesday","utc_time":"2026-07-29T21:24:52.929432+00:00","unix_timestamp":1785360292}
				.to_utf8()
		})
	}
}

main! = |_| {
	timezone = "America/Chicago"
	request = Request.from_method(GET)
		.with_uri("https://timeapi.io/api/v1/timezone/zone?timeZone=${timezone}")
		.with_timeout(TimeoutMilliseconds(3000))

	response = Send.send!(request)?
	if response.status >= 200 and response.status <= 299 {
		iso_str = get_iso_str(response.body) ? |_| Exit(1)
		dt_now = DT.from_iso_str(iso_str) ? |_| Exit(2)

		date_str = dt_now.format("{YYYY}-{MM}-{DD}")
		time_str = dt_now.format("{hh}:{mm}:{ss}")
		"The current Zulu date is: ${date_str}"->Stdout.line!()
		"The current Zulu time is: ${time_str}"->Stdout.line!()
		Ok({})
	} else {
		Stdout.line!("Failed to get server response. HTTP status: ${response.status}")
		Err(Exit(3))
	}
}

get_iso_str : List(U8) -> Try(Str, _)
get_iso_str = |bytes| {
	str = bytes->Str.from_utf8()?
	response : { localtime : Str }
	response = Json.parse(str)?
	Ok(response.localtime)
}
