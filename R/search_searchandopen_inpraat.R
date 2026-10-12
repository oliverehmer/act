#' Search corpus and open first result in Praat
#'
#' The function remote controls 'Praat' with a 'Praat' script. 
#' It first searches your corpus object and uses the first search hit. 
#' The corresponding TextGrid will be opened in the 'Praat' TextGrid Editor and the search hit will be displayed.
#' 
#' 'Praat' receives the script through its own command line option \code{--send}
#' (from 'Praat' 6.1 on): set the path to 'Praat' with 'options(act.path.praat = ...)'.
#' If 'Praat' is not running, it is started. Only if the path to 'Praat' is not set,
#' 'sendpraat' is used ('options(act.path.sendpraat = ...)', see \code{vignette("install_sendpraat", package = "act")}).
#' 
#' 
#' @param x Corpus object.
#' @param pattern Character string; search pattern as regular expression.
#' 
#' @seealso \code{vignette("install_sendpraat", package = "act")}
#'
#' @export
#'
#' @examples
#' library(act)
#'
#' # You can only use this functions if you have installed 'Praat'
#' # and located it properly in the package options.
#' \dontrun{
#' act::search_searchandopen_inpraat(x=examplecorpus, "pero")
#' }
#' 
#' 
#' 
search_searchandopen_inpraat <- function(x,
										 pattern) {
	
	.assert_corpus(x, missing = missing(x))
	
	if (missing(pattern)) 	{cli::cli_abort("Pattern is missing.") }	
	
	s <- act::search_new(x=x, pattern=pattern, concordanceMake=FALSE)
	if (is.null(s@results)) {
		#No results
		#return(NULL)
	} else if (nrow(s@results)==0) {
		#No results.
		#return(NULL)
	} else {
		act::search_openresult_inpraat(x, s, 1)
	}
}
