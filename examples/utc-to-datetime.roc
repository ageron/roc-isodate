app [main!] {
	pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/0.9/8GdFEvQYS3TeAZxKvTzCLVdQiomweGtXcdZkXNDEeABq.tar.zst",
	dt: "../package/main.roc",
}

import pf.Stdout
import pf.Utc
import dt.DateTime

main! = |_| {
	utc_now = Utc.now!({})
	now_str = 
		(
			utc_now
				->Utc.to_nanos_since_epoch()
				->DateTime.from_nanos_since_epoch(),
		).to_iso_str()
	Stdout.line!("The current Zulu time is: ${now_str}")
}
