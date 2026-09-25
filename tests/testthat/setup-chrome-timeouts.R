# Chrome is slow to open its debugging port on loaded CI runners, and under
# covr instrumentation. These raise the waits used by chromote (default 10 s)
# and pagedown (default 20 attempts of 0.5 s) for the test process only; a
# machine that starts Chrome promptly is not slowed down by them.
options(
  chromote.timeout = 60,
  pagedown.remote.maxattempts = 120L
)
