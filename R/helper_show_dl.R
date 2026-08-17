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

# Print the top title line of a show() method as a cat-printed rule with
# bold title text.
#
# @param text Character string. Title text.
# @return Invisible NULL. Called for its side effect of printing.
#
# @keywords internal
# @noRd
.show_title <- function(text) {
	cat(cli::rule(left = cli::style_bold(text)), "\n", sep = "")
}

# Print a bold section header for show() methods via a cat-printed ANSI
# bold string.
#
# @param text Character string. Header text.
# @return Invisible NULL. Called for its side effect of printing.
#
# @keywords internal
# @noRd
.show_head <- function(text) {
	cat(cli::style_bold(text), "\n", sep = "")
}

# Print a blank separator line between sections of a show() method.
#
# @return Invisible NULL. Called for its side effect of printing.
#
# @keywords internal
# @noRd
.show_sep <- function() {
	cat("\n")
}
