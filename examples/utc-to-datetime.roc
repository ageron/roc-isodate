app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
	dt: "../package/main.roc",
}

import pf.Stdout
import pf.Utc
import dt.DateTime

main! = |_| {
	utc_now = Utc.now!()
	now_str =
		(
			utc_now
				|> Utc.to_nanos_since_epoch
				.to_i128_try()?
				|> DateTime.from_nanos_since_epoch,
		).to_iso_str()
	_ = Stdout.line!("The current Zulu time is: ${now_str}")
	Ok({})
}
