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
	
	if (missing(outFolder)) {stop("Parameter 'outFolder' is missing")}
	if (missing(outFilename)) {stop("Parameter 'outFilename' is missing")}
	#-- make the destination folder, if it does not exist
	if (!is.null(cutlistMac) | !is.null(cutlistWin)) {
		if (dir.exists(outFolder)==FALSE) 	{
			dir.create(outFolder, recursive=TRUE)
		}
	}
	
	#-- win
	if (!is.null(cutlistWin)) {
		out_path 	<- file.path(outFolder, paste(outFilename, "_win.cmd", sep=""))
		fileConn 	<- file(out_path)
		writeLines(cutlistWin, fileConn)
		close(fileConn)
	}
	
	#-- mac
	if (!is.null(cutlistMac)) {
		#add that it is an executable
		cutlistMac <- c("#!/bin/sh", cutlistMac)
		#save
		out_path 	<- file.path(outFolder, paste(outFilename, "_mac", sep=""))
		fileConn 	<- file(out_path)
		writeLines(cutlistMac, fileConn)
		close(fileConn)
		#make executable on a mac or a linux machine
		if (file.exists(out_path)) {
			if (Sys.info()["sysname"]=="Darwin") {
				system(paste("chmod 755 '", out_path, "'", sep=""))
			}				
		}
	}
}

