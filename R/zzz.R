#=== progress bar in new environment
act.environment    <- new.env()
act.environment$pb <- progress::progress_bar$new(
	format = "  Default  [:bar] :percent (:eta left)",
	total = NA, 
	clear = FALSE, 
	show_after = 0,
	width= 60)

.onLoad <- function(libname, pkgname) {
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



