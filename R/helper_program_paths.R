# ===== PROGRAM PATHS (Praat, sendpraat, ELAN) =====

# Fills act.path.praat, act.path.sendpraat and act.path.elan at package load
# when they are empty or point nowhere, so "open in Praat/ELAN" works without
# configuration. A path set later (e.g. by a config file) always wins.
.act_detect_programs <- function() {
	targets <- list(
		act.path.praat     = .act_program_candidates_praat,
		act.path.sendpraat = .act_program_candidates_sendpraat,
		act.path.elan      = .act_program_candidates_elan
	)
	for (option_name in names(targets)) {
		current <- getOption(option_name, "")
		if (is.character(current) && length(current) == 1 && nzchar(current) && file.exists(current)) next
		candidates <- tryCatch(targets[[option_name]](), error = function(e) character(0))
		found <- candidates[nzchar(candidates) & file.exists(candidates)]
		if (length(found) > 0) {
			value <- list(found[1])
			names(value) <- option_name
			options(value)
		}
	}
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

.act_program_candidates_sendpraat <- function() {
	sysname <- Sys.info()[["sysname"]]
	if (identical(sysname, "Darwin")) {
		return(c("/Applications/sendpraat", "/usr/local/bin/sendpraat", "/opt/homebrew/bin/sendpraat"))
	}
	if (.Platform$OS.type == "windows") {
		roots <- .act_program_roots_windows()
		return(c(file.path(roots, "Praat", "sendpraat.exe"), file.path(roots, "sendpraat.exe")))
	}
	unname(Sys.which("sendpraat"))
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
