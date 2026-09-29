app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
	dt: "../package/main.roc",
}

import pf.Stdout
import pf.Utc
import dt.DateTime
import dt.Now

main! = |_args| {
	now = Now.create({
		now!: Utc.now!,
		now_to_nanos: Utc.to_nanos_since_epoch,
	})
	_ = now.date_time!()
		.format("{MM}/{DD}/{YY} | {hh}:{mm}:{ss}")
		|> Stdout.line!
	Ok({})
}
