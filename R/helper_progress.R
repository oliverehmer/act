#' Helper: Set progress bar
#'
#' @param title Character string; Title of progress bar.
#' @param total Integer; Number of items to tick.
#'
#' @return NULL
#'
#' @export
#'
#' @example inst/examples/helper_tiers_merge_tables.R
#'
#'
helper_progress_set <- function(title, total) {
	if (!exists("act.environment", mode = "environment")) return(invisible(NULL))
	# the state and the callback come before the console switch: a caller (the
	# iclo viewer) shows the progress while the console bars are off
	act.environment$pb_state <- list(start = Sys.time(), current = 0, total = max(1, total), title = title)
	.progress_callback_call(title, 0, max(1, total))
	if (!getOption("act.showprogress", TRUE)) return(invisible(NULL))
	if (!requireNamespace("progress", quietly = TRUE)) {
		cli::cli_warn("Package {.arg progress} not available.")
		return(invisible(NULL))
	}

	title <- stringr::str_pad(title, width = .ACT_PROGRESS_LABEL_WIDTH, side = "right", pad = " ")
	act.environment$pb <- progress::progress_bar$new(
		format = paste0(title, "[:bar] :percent:tail"),
		total = max(1, total),
		clear = FALSE,
		show_after = 0.1,
		width = .ACT_PROGRESS_TOTAL_WIDTH
	)
	invisible(NULL)
}

#' Helper: Advance progress bar by one tick
#'
#' @param len Integer; number of items finished with this tick (default 1).
#'
#' @return NULL
#'
#' @export
#'
#' @example inst/examples/helper_tiers_merge_tables.R
#'
#'
#'
helper_progress_tick <- function(len = 1) {
	if (!exists("act.environment", mode = "environment")) return(invisible(NULL))
	state <- act.environment$pb_state
	if (is.list(state)) {
		state$current <- min(state$current + len, state$total)
		act.environment$pb_state <- state
		.progress_callback_call(state$title %||% "", state$current, state$total)
	}
	if (!getOption("act.showprogress", TRUE)) return(invisible(NULL))
	if (!exists("pb", envir = act.environment)) return(invisible(NULL))

	pb <- act.environment$pb

	# Tick only if the progress bar is not finished
	if (inherits(pb, "progress_bar") && !pb$finished) {
		tail <- if (is.list(state)) helper_progress_eta_format(state$start, state$current, state$total)
		        else sprintf(" (%7s left)", "?")
		pb$tick(len = len, tokens = list(tail = tail))
	}
	invisible(NULL)
}

#' Helper: Set a progress callback
#'
#' Registers a function that every progress bar of act (and of packages that
#' use \code{\link{helper_progress_set}} / \code{\link{helper_progress_tick}})
#' calls when it starts and at every step, also while the console bars are
#' switched off (option \code{act.showprogress}). A graphical caller (a Shiny
#' app) uses it to show the progress itself.
#'
#' @param fn Function with the arguments \code{title} (character),
#'   \code{current} (number of finished items) and \code{total} (number of
#'   all items), or \code{NULL} to remove the callback.
#'
#' @return The previous callback (or \code{NULL}), invisibly - pass it back to
#'   restore it.
#'
#' @export
#'
#' @examples
#' old <- helper_progress_callback_set(function(title, current, total)
#' 	message(title, ": ", current, " of ", total))
#' helper_progress_set("Counting", 3)
#' for (i in 1:3) helper_progress_tick()
#' helper_progress_callback_set(old)
helper_progress_callback_set <- function(fn) {
	if (!is.null(fn) && !is.function(fn)) cli::cli_abort("{.arg fn} must be a function or NULL.")
	old <- act.environment$progress_callback
	act.environment$progress_callback <- fn
	invisible(old)
}

#' Helper: Get the progress callback
#'
#' @return The function set with \code{\link{helper_progress_callback_set}},
#'   or \code{NULL}.
#'
#' @export
helper_progress_callback_get <- function() {
	act.environment$progress_callback
}

# a failing callback must not break the work it reports on
.progress_callback_call <- function(title, current, total) {
	fn <- act.environment$progress_callback
	if (is.function(fn)) tryCatch(fn(title, current, total), error = function(e) NULL)
	invisible(NULL)
}

# Progress bar geometry, kept in sync with iclo (.ICLO_PROGRESS_* constants in
# iclo/R/config.R; act must not read iclo, so the values are duplicated here):
# label column 28 = longest bar label ("Scanning sequences folders", 26) + 2,
# total width 80 = right edge of the cli section rules. The fixed-width tail
# (15 chars, sized for "(59m 20s left)") keeps the bar itself at a constant
# width.
.ACT_PROGRESS_LABEL_WIDTH <- 28L
.ACT_PROGRESS_TOTAL_WIDTH <- 80L

# Label of an export bar ("Exporting eaf, textgrid"); iclo builds its export
# bars with it as well. A list too long for the label column becomes a count,
# so the bar keeps its width.
.progress_export_label <- function(formats) {
	formats <- unique(stringr::str_to_lower(as.character(formats)))
	label <- paste("Exporting", paste(formats, collapse = ", "))
	if (nchar(label) >= .ACT_PROGRESS_LABEL_WIDTH) label <- paste("Exporting", length(formats), "formats")
	label
}

#' Helper: Remaining time of a progress bar
#'
#' Formats the remaining time of a loop as a fixed-width tail
#' \code{" (%7s left)"} (15 characters, fits "59m 20s"), estimated from the
#' elapsed time per finished item. The progress bars of act and iclo use it.
#'
#' @param start POSIXct; start time of the loop.
#' @param current Integer; number of finished items.
#' @param total Integer; number of all items.
#'
#' @return Character string.
#'
#' @export
helper_progress_eta_format <- function(start, current, total) {
	if (current <= 0 || current >= total) {
		eta_txt <- "0s"
	} else {
		elapsed   <- as.numeric(difftime(Sys.time(), start, units = "secs"))
		remaining <- max(0, elapsed / current * (total - current))
		eta_txt <- if (remaining >= 100 * 3600) {
			">99h"
		} else if (remaining >= 3600) {
			sprintf("%dh %02dm", floor(remaining / 3600), floor((remaining %% 3600) / 60))
		} else if (remaining >= 60) {
			sprintf("%dm %02ds", floor(remaining / 60), floor(remaining %% 60))
		} else {
			sprintf("%ds", ceiling(remaining))
		}
	}
	sprintf(" (%7s left)", eta_txt)
}
