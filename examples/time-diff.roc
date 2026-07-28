app [main!] {
	pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/0.9/8GdFEvQYS3TeAZxKvTzCLVdQiomweGtXcdZkXNDEeABq.tar.zst",
	dt: "../package/main.roc",
}

import pf.Sleep
import pf.Stdout
import pf.Utc
import dt.Duration
import dt.Time
import dt.Now

main! = |_args| {
	now = Now.create({
		now!: Utc.now!,
		now_to_nanos: Utc.to_nanos_since_epoch,
	})
	start = now.time!()
	Sleep.millis!(1000)
	end = now.time!()
	duration = Time.sub(end, start)
	Duration.format(duration, "Slept for {s}.{f} seconds")
		->Stdout.line!()
}
