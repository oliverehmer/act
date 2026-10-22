#' Open a search result in 'Praat'
#'
#' The function remote controls 'Praat' with a 'Praat' script. 
#' It opens a search result in the 'Praat' TextGrid Editor.
#' 
#' 'Praat' receives the script through its own command line option \code{--send}
#' (from 'Praat' 6.1 on): set the path to 'Praat' with 'options(act.path.praat = ...)'.
#' If 'Praat' is not running, it is started (see \code{vignette("praat_remote_control", package = "act")}).
#' 
#' @param x Corpus object.
#' @param s Search object. 
#' @param resultid Integer; Number of the search result (row in the data frame \code{s@results}) to be played. 
#' @param play Logical; If \code{TRUE} selection will be played.
#' @param close Logical; If \code{TRUE} TextGrid editor will be closed after playing.
#' @param filterMediaFile Vector of character strings; Each element of the vector is a regular expression. Expressions will be checked consecutively. The first match with an existing media file will be used for playing. The default checking order is uncompressed audio > compressed audio.
#' @param delay Double; Time in seconds before the section will be opened in Praat. This is useful if Praat opens but the section does not. In that case increase the delay. 
#' @seealso \code{vignette("praat_remote_control", package = "act")}
#'
#' @export
#'
#' @examples
#' library(act)
#'
#' mysearch <- act::search_new(x=examplecorpus, pattern = "pero")
#' 
#' # You can only use this functions if you have installed 'Praat'
#' # and located it properly in the package options.
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
	
	#--- the original TextGrid when there is one, else a copy in tempdir()
	invisible(act::transcripts_openin_praat(t               = t,
	                                        openOriginal    = TRUE,
	                                        filePathOut     = NULL,
	                                        startSec        = s@results[resultid, ]$startsec,
	                                        endSec          = s@results[resultid, ]$endsec,
	                                        play            = play,
	                                        filterMediaFile = filterMediaFile,
	                                        delay           = delay,
	                                        zoomStartSec    = NULL,
	                                        zoomEndSec      = NULL,
	                                        close           = close))
}
