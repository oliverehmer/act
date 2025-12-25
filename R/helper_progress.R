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
		warning("Package 'progress' not available.")
		return(invisible(NULL))
	}
	
	title <- stringr::str_pad(title, width = 24, side = "right", pad = " ")
	act.environment$pb <- progress::progress_bar$new(
		format = paste("  ", title, "[:bar] :percent (:eta left)", sep = ""),
		total = max(1, total),
		clear = FALSE,
		show_after = 0,
		width = 70
	)
}
#helper_progress_set <- function(title, total) {
#	#set progress bar	
#	if(getOption("act.showprogress", TRUE)) {
#		if (exists("act.environment", mode="environment")) {
#			if(exists("pb", envir=act.environment)) {
#				title <- stringr::str_pad(title, width=24, side="right", pad=" ")
#				act.environment$pb <- progress::progress_bar$new(
#					format = paste("  ", title, "[:bar] :percent (:eta left)", sep=""),
#					total = max(1,total), 
#					clear = FALSE, 
#					show_after = 0,
#					width= 70)
#			}
#		}			
#	}
#}

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
		pb$tick()
	}
}
#helper_progress_tick <- function() {
#	#update progress
#	if (getOption("act.showprogress", TRUE)) {
#		if (exists("act.environment", mode="environment")) {
#			if(exists("pb", envir=act.environment)) {
#				if (!act.environment$pb$finished) {			
#					act.environment$pb$tick()
#				}
#			}
#		}
#	}
#}