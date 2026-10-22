# ===== SEARCH HELPERS =====

# The search pattern as an ICU regex with a time limit: a pattern with nested
# repetition can backtrack for minutes on one long annotation and block the R
# process (the viewer on a server for everybody). With the limit the match
# stops with an error instead. ICU counts its own units, not milliseconds:
# measured 22.10.2026, 5000 units stopped after 0.5 s, so 50000 is about 5 s.
.search_regex <- function(pattern) {
	stringr::regex(pattern, time_limit = 50000L)
}
