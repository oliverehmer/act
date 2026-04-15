# Print aligned key-value pairs for show() methods.
#
# Takes a named character vector and prints each pair with the key
# left-padded to the specified width, followed by " : " and the value.
#
# @param items Named character vector of key-value pairs.
# @param width Integer. Column width for keys. All keys are padded to this width.
#
# @return Invisible NULL. Called for its side effect of printing.
#
# @keywords internal
# @noRd
.show_dl <- function(items, width) {
	nms <- names(items)
	for (i in seq_along(items)) {
		cat(sprintf("  %-*s  %s\n", width, nms[i], items[i]))
	}
}
