#=== progress bar in new environment
act.environment    <- new.env()

.onLoad <- function(libname, pkgname) {
	#progress bar is created at load time, not at install time (a build-time
	#R6 object would be serialized into the package database)
	act.environment$pb <- progress::progress_bar$new(
		format = paste0(stringr::str_pad("Default", .ACT_PROGRESS_LABEL_WIDTH, "right"), "[:bar] :percent:tail"),
		total = NA,
		clear = FALSE,
		show_after = 0,
		width = .ACT_PROGRESS_TOTAL_WIDTH)
	#transfer the missing options to Rs options
	toset <- !(names(act.options.default) %in% names(options()))
	if (any(toset)) {
		options(act.options.default[toset])
	}

}
.onAttach <- function(libname, pkgname) {

}

.onUnload <- function(libpath) {

}



