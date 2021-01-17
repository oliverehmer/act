#' Open a search result in 'Quicktime' (and play it)
#'
#' The function remote controls 'Quicktime' by using an Apple Script. 
#' It opens a search result in 'Quicktime' and plays it.
#' 
#' You need to be on a Mac to use this function.
#' 
#' #' \emph{Span} \cr
#' If you want to extend the cut before or after each search result, you can modify \code{@cuts.span.beforesec} and \code{@cuts.span.aftersec} in your search object.
#' 
#' @param x Corpus object.
#' @param s Search object.
#' @param resultNr Integer; Number of the search result (row in the data frame \code{s@results}) to be played. 
#' @param play Logical; If \code{TRUE} selection will be played.
#' @param closeAfterPlaying Logical; if \code{TRUE} the Quicktime player will be closed after playing the cuts. 
#' @param bringQuicktimeToFront Logical; if \code{TRUE} the Quicktime player will be activated and placed before all other windows. 
#' @param filterFile Vector of character strings; Each element of the vector is a regular expression. Expressions will be checked consecutively. The first match with an existing media file will be used for playing. The default checking order is video > uncompressed audio > compressed audio.
#'
#' @return Logical; \code{TRUE} if media file has been played, or \code{FALSE} if not.
#' @export
#'
#' @example inst/examples/search_openresult_inquicktime.R 
#' 
search_openresult_inquicktime  <- function(x, 
										   s, 
										   resultNr,
										   play=TRUE, 
										   closeAfterPlaying=FALSE, 
										   bringQuicktimeToFront=TRUE, 
										   filterFile=c('.*\\.(mp4|mov)', '.*\\.(aiff|aif|wav)', '.*\\.mp3') ) {
	
	# result <- mysearch@results[1,]
	# x <-examplecorpus
	# search_openresult_inquicktime(x, searchresults[1,])
	# closeAfterPlaying <-TRUE

	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		} else { if (class(s)[[1]]!="search")		{stop("Parameter 's' needs to be a search object.") 	} }
	if (missing(resultNr))                   {stop("Number of the search result 'resultNr' is missing.") 	}
	if (Sys.info()["sysname"]!="Darwin")     {stop("You need to be on a Mac to use this function") 	}
	
	if (resultNr>nrow(s@results)) {stop("Number of the search result exceeds rows in 's@results'.") 	}
	result <- s@results[resultNr,]
	
	#--- get  corresponding transcript
	t <- x@transcripts[[result$transcript.name]]
	if (is.null(t))	{ 	stop("Transcript not found in corpus object x.") 	}
	
	#---get path of media file
	pathMediaFile <- act::media_getPathToExistingFile(t) 
	if (is.null(pathMediaFile))	{
		warning("No media file(s) found.")
		return (FALSE)
	} 
	
	#--- get path to apple script
	appleScriptPath	<- file.path(system.file("extdata", "applescript", package="act"), "PlayCutInQuicktime.scpt")
	#appleScriptPath <- "/Users/oliverehmer/Desktop/PlayCutInQuicktime.scpt"
	appleScriptPath <- normalizePath(appleScriptPath)
	file.exists(appleScriptPath)
	
	startSec <-result$startSec - s@cuts.span.beforesec
	endSec <-result$endSec + s@cuts.span.aftersec
	durationSec <- endSec-startSec+0.3
	
	#execute script
	cmd <- sprintf('osascript "%s" "%s" %s %s %s %s %s', appleScriptPath, pathMediaFile,  as.character(startSec), as.character(durationSec), if(play) {1} else {0}, if(closeAfterPlaying) {1} else {0}, if (bringQuicktimeToFront){1} else {0})
	rslt=system(cmd, intern=TRUE, ignore.stderr = TRUE, ignore.stdout=TRUE)
	
	return(TRUE)
}
