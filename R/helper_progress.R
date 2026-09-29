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
	if (!getOption("act.showprogress", TRUE)) return(invisible(NULL))
	if (!exists("act.environment", mode = "environment")) return(invisible(NULL))
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
	act.environment$pb_state <- list(start = Sys.time(), current = 0, total = max(1, total))
	invisible(NULL)
}

#' Helper: Advance progress bar by one tick
#'
#'
#' @return NULL
#'
#' @export
#'
#' @example inst/examples/helper_tiers_merge_tables.R
#'
#'
#'
helper_progress_tick <- function() {
	# Only proceed if progress is enabled
	if (!getOption("act.showprogress", TRUE)) return(invisible(NULL))

	# Ensure act.environment and pb exist
	if (!exists("act.environment", mode = "environment")) return(invisible(NULL))
	if (!exists("pb", envir = act.environment)) return(invisible(NULL))

	pb <- act.environment$pb

	# Tick only if the progress bar is not finished
	if (inherits(pb, "progress_bar") && !pb$finished) {
		state <- act.environment$pb_state
		if (is.list(state)) {
			state$current <- state$current + 1
			act.environment$pb_state <- state
			tail <- .act_progress_eta_tail(state$start, state$current, state$total)
		} else {
			tail <- sprintf(" (%7s left)", "?")
		}
		pb$tick(tokens = list(tail = tail))
	}
}


# Progress bar geometry, kept in sync with iclo (.ICLO_PROGRESS_* constants in
# iclo/R/config.R; act must not read iclo, so the values are duplicated here):
# label column 28 = longest bar label ("Scanning sequences folders", 26) + 2,
# total width 80 = right edge of the cli section rules. The fixed-width tail
# (15 chars, sized for "(59m 20s left)") keeps the bar itself at a constant
# width.
.ACT_PROGRESS_LABEL_WIDTH <- 28L
.ACT_PROGRESS_TOTAL_WIDTH <- 80L

# Fixed-width eta tail " (%7s left)" (15 chars total, fits "59m 20s"),
# computed from elapsed time per finished item.
.act_progress_eta_tail <- function(start, current, total) {
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
