#' Open a search result in 'Praat'
#'
#' The function remote controls 'Praat' by using 'sendpraat' and a 'Praat' script. 
#' It opens a search result in the 'Praat' TextGrid Editor.
#' 
#' To make this function work you need to do two things first:
#' - Install 'sendpraat' on your computer. To do so  follow the  instructions in the vignette 'installation-sendpraat'. Show the vignette with \code{vignette("installation-sendpraat")}.
#' - Set the path to the 'sendpraat' executable correctly by using 'options(act.path.sendpraat = ...)'.
#' 
#' @param x Corpus object.
#' @param s Search object. 
#' @param resultid Integer; Number of the search result (row in the data frame \code{s@results}) to be played. 
#' @param play Logical; If \code{TRUE} selection will be played.
#' @param close Logical; If \code{TRUE} TextGrid editor will be closed after playing (Currently non functional!)
#' @param filterMediaFile Vector of character strings; Each element of the vector is a regular expression. Expressions will be checked consecutively. The first match with an existing media file will be used for playing. The default checking order is uncompressed audio > compressed audio.
#' @param delay Double; Time in seconds before the section will be opened in Praat. This is useful if Praat opens but the section does not. In that case increase the delay. 
#' @seealso \code{vignette("install_sendpraat", package = "act")}
#'
#' @export
#'
#' @examples
#' library(act)
#'
#' mysearch <- act::search_new(x=examplecorpus, pattern = "pero")
#' 
#' # You can only use this functions if you have installed and 
#' # located the 'sendpraat' executable properly in the package options.
#' \dontrun{
#' act::search_openresult_inpraat(x=examplecorpus, s=mysearch, resultid=1, TRUE, TRUE)
#' }
search_openresult_inpraat  <- function(x, 
									   s, 
									   resultid, 
									   play           =TRUE, 
									   close          =FALSE, 
									   filterMediaFile=c('(?i).*\\.(aiff|aif|wav)', '(?i).*\\.mp3'),
									   delay          =0.5) {
	
	# result <- mysearch@results[1,]
	# x <- examplecorpus
	# search_openresult_inpraat(x, searchresults[1,])
	
	.assert_corpus(x, missing = missing(x))
	.assert_search(s, missing = missing(s))
	
	
	if (missing(resultid)) {cli::cli_abort("Number of the search result {.arg resultid} is missing.") 	}
	
	
	#--- get  corresponding transcript
	t <- x@transcripts[[s@results[resultid, ]$transcriptName]]
	if (is.null(t))	{
		cli::cli_abort("Transcript not found in corpus object'.")
	}
	
	#--- TextGrid: the original, or a temporary copy written by the helper
	path_textgrid <- .get_textgrid_for_transcript(t)
	is_copy <- is.na(t@file.path) || !identical(normalizePath(path_textgrid, mustWork = FALSE),
	                                            normalizePath(t@file.path, mustWork = FALSE))

	#--- sound: the first existing media file that matches filterMediaFile
	path_longsound <- media_path_to_existing_file(t, filterMediaFile = filterMediaFile)
	if (is.null(path_longsound)) {
		path_longsound <- ""
		cli::cli_warn("No media file(s) found.")
	}

	invisible(.praat_open_selection(pathTextGrid  = path_textgrid,
	                      pathLongSound = path_longsound,
	                      startSec      = s@results[resultid, ]$startsec,
	                      endSec        = s@results[resultid, ]$endsec,
	                      play          = play,
	                      close         = close,
	                      reload        = is_copy,
	                      delay         = delay))
}
