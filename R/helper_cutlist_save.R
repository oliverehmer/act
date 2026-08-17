#' Helper: Save cutlist
#'
#' Saves FFMPEG cut list fpr mac or windows
#' For windows: simly saves to a .cmd file
#' For mac: saves as a shell script and makes it executable (also adds "#!/bin/sh")
#' 
#' @param cutlistMac Character string; Content if file, if \code{NULL} no file will be saved.
#' @param cutlistWin Character string; Content if file, if \code{NULL} no file will be saved.
#' @param outFolder Character string; Destination folder.
#' @param outFilename Character string; Destination filename
#'
#' @return NULL
#' 
#' @export
#'
#' @example inst/examples/helper_tiers_merge_tables.R
#' 
#' 
helper_cutlist_save <-  function(cutlistMac    = NULL, 
								 cutlistWin    = NULL,
								 outFolder, 
								 outFilename) {
	
	if (missing(outFolder)) {cli::cli_abort("Parameter {.arg outFolder} is missing")}
	if (missing(outFilename)) {cli::cli_abort("Parameter {.arg outFilename} is missing")}
	#-- make the destination folder, if it does not exist
	if (!is.null(cutlistMac) | !is.null(cutlistWin)) {
		if (dir.exists(outFolder)==FALSE) 	{
			dir.create(outFolder, recursive=TRUE)
		}
	}
	
	#-- win
	if (!is.null(cutlistWin)) {
		out_path 	<- file.path(outFolder, paste(outFilename, "_win.cmd", sep=""))
		.cutlist_write_utf8(cutlistWin, out_path, eol="\r\n")
	}

	#-- mac
	if (!is.null(cutlistMac)) {
		#add that it is an executable
		cutlistMac <- c("#!/bin/sh", cutlistMac)
		#save
		out_path 	<- file.path(outFolder, paste(outFilename, "_mac", sep=""))
		.cutlist_write_utf8(cutlistMac, out_path, eol="\n")
		#make executable on a mac or a linux machine
		if (file.exists(out_path)) {
			Sys.chmod(out_path, mode="0755")
		}
	}
}


# Write a cut list as UTF-8 with explicit line endings for the TARGET OS
# (CRLF for the Windows .cmd, LF for the mac/linux shell script), regardless
# of the OS that generates the list. A text-mode connection would use the
# host's native encoding and line endings: a _mac script generated on Windows
# would get CRLF after "#!/bin/sh" ("bad interpreter"), a _win.cmd generated
# on macOS would get LF-only line endings, and non-ASCII file names would be
# written in the host locale encoding.
.cutlist_write_utf8 <- function(lines, path, eol = "\n") {
	txt <- paste0(paste(lines, collapse = "\n"), "\n")
	txt <- stringi::stri_replace_all_fixed(txt, "\r\n", "\n")
	if (!identical(eol, "\n")) {
		txt <- stringi::stri_replace_all_fixed(txt, "\n", eol)
	}
	con <- file(path, open = "wb")
	on.exit(close(con))
	writeBin(charToRaw(enc2utf8(txt)), con)
	invisible(NULL)
}

