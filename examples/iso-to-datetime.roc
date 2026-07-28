app [main!] {
	pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/0.9/8GdFEvQYS3TeAZxKvTzCLVdQiomweGtXcdZkXNDEeABq.tar.zst",
	http: "https://github.com/roc-lang/http/releases/download/2.0.0/6ZUwqYhCS8PU9Mo6MF7oV82ET2o7KYb57CLKDq4cq4sS.tar.zst",
	dt: "../package/main.roc",
}

import pf.Stdout
import http.Method
import http.Request

# import dt.DateTime as DT

main! = |_| {
	timezone = "America/Chicago"
	request = Request.from_method(GET)
		.with_uri("https://timeapi.io/api/v1/timezone/zone?timeZone=${timezone}")
		.with_timeout(TimeoutMilliseconds(3000))

	response = Http.send!(req)?
	if response.status >= 200 and response.status <= 299 {
		iso_str = get_iso_str(response.body)?
		dt_now = DT.from_iso_str(iso_str)?

		date_str = dt_now.format("{YYYY}-{MM}-{DD}")
		time_str = dt_now.format("{hh}:{mm}:{ss}")
		"The current Zulu date is: ${date_str}"->Stdout.line!()?
		"The current Zulu time is: ${time_str}"->Stdout.line!()
	} else {
		Err(FailedToGetServerResponse(response.status))
	}
}

format_request = |timezone| {
	method: GET,
	headers: [],
	uri: "https://timeapi.io/api/v1/timezone/zone?timeZone=${timezone}",
	body: [],
	timeout_ms: TimeoutMilliseconds(3000),
}

get_iso_str : List(U8) -> Try(Str, _)
get_iso_str = |bytes| {
	str = bytes->Str.from_utf8()
	response : { localtime : Str }
	response = Json.parse(str)?
	Ok(response.localtime)
}
