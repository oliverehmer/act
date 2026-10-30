#' Helper: Find Praat and ELAN at their usual places
#'
#' Fills the options \code{act.path.praat} and \code{act.path.elan} with the
#' programs found at the usual installation places: on macOS
#' \code{/Applications} and \code{~/Applications}, on Windows the program
#' folders, elsewhere \code{/opt}, the home folder and the search path. When
#' several ELAN versions are installed side by side (\code{ELAN_6.7},
#' \code{ELAN_7.1}), the newest one is taken.
#'
#' An option that already points to an existing program is kept. An option
#' that is empty or points nowhere (for example to an ELAN version removed
#' after an update) is replaced by the program found. act calls the helper
#' when it is loaded.
#'
#' @return Invisibly, a data frame with one row per option: \code{option},
#'   \code{before} (value before the call), \code{after} (value after the
#'   call) and \code{changed} (logical).
#'
#' @export
#'
#' @examples
#' act::helper_program_paths_detect()
#'
helper_program_paths_detect <- function() {
	targets <- list(
		act.path.praat = .act_program_candidates_praat,
		act.path.elan  = .act_program_candidates_elan
	)
	before <- character(0)
	after  <- character(0)
	for (option_name in names(targets)) {
		current <- getOption(option_name, "")
		current_text <- if (is.character(current) && length(current) >= 1 && !is.na(current[1])) current[1] else ""
		before[option_name] <- current_text
		after[option_name]  <- current_text
		if (nzchar(current_text) && file.exists(current_text)) next
		candidates <- tryCatch(targets[[option_name]](), error = function(e) character(0))
		found <- candidates[nzchar(candidates) & file.exists(candidates)]
		if (length(found) > 0) {
			value <- list(found[1])
			names(value) <- option_name
			options(value)
			after[option_name] <- found[1]
		}
	}
	invisible(data.frame(option = names(targets), before = unname(before[names(targets)]),
						 after = unname(after[names(targets)]),
						 changed = unname(before[names(targets)] != after[names(targets)]),
						 stringsAsFactors = FALSE))
}

# ===== PROGRAM PATHS (Praat, ELAN) =====

# Called at package load when the options are empty or point nowhere, so
# "open in Praat/ELAN" works without configuration. A path set later (e.g. by
# a config file) wins, unless it points nowhere: iclo::config() then calls
# helper_program_paths_detect() again.
.act_detect_programs <- function() {
	helper_program_paths_detect()
	invisible(NULL)
}

.act_program_candidates_praat <- function() {
	sysname <- Sys.info()[["sysname"]]
	if (identical(sysname, "Darwin")) {
		return(c("/Applications/Praat.app", file.path(path.expand("~"), "Applications", "Praat.app")))
	}
	if (.Platform$OS.type == "windows") {
		roots <- .act_program_roots_windows()
		return(c(file.path(roots, "Praat", "Praat.exe"), file.path(roots, "Praat.exe"),
				 file.path(Sys.getenv("USERPROFILE"), "Desktop", "Praat.exe")))
	}
	unname(Sys.which("praat"))
}

# Several ELAN versions can be installed side by side (ELAN_6.7, ELAN_7.1);
# the newest one is taken.
.act_program_candidates_elan <- function() {
	sysname <- Sys.info()[["sysname"]]
	paths <- if (identical(sysname, "Darwin")) {
		c(Sys.glob("/Applications/ELAN_*.app"), Sys.glob(file.path(path.expand("~"), "Applications", "ELAN_*.app")))
	} else if (.Platform$OS.type == "windows") {
		Sys.glob(file.path(.act_program_roots_windows(), "ELAN_*", "ELAN.exe"))
	} else {
		c(Sys.glob("/opt/ELAN_*/bin/ELAN"), Sys.glob(file.path(path.expand("~"), "ELAN_*", "bin", "ELAN")))
	}
	if (length(paths) < 2) return(paths)
	versions <- stringr::str_match(paths, "ELAN_([0-9]+(?:\\.[0-9]+)*)")[, 2]
	parsed   <- suppressWarnings(numeric_version(ifelse(is.na(versions), "0", versions), strict = FALSE))
	paths[order(parsed, decreasing = TRUE)]
}

.act_program_roots_windows <- function() {
	roots <- c(Sys.getenv("ProgramFiles"), Sys.getenv("ProgramFiles(x86)"), Sys.getenv("LOCALAPPDATA"),
			   file.path(Sys.getenv("LOCALAPPDATA"), "Programs"))
	roots[nzchar(roots)]
}
