#' Search corpus and open first result in Praat
#'
#' The function remote controls 'Praat' by using 'sendpraat' and a 'Praat' script. 
#' It first searches your corpus object and uses the first search hit. 
#' The corresponding TextGrid will be opened in the 'Praat' TextGrid Editor and the search hit will be displayed.
#' 
#' To make this function work you need to set the path to the 'sendpraat' executable using 'options(act.path.sendpraat = ...)'.
#' 
#' @param x Corpus object.
#' @param pattern Character string; search pattern as regular expression.
#' 
#' @export
#'
#' @examples
#' library(act)
#' 
#' # You can only use this functions if you have located the 'sendpraat' executable 
#' # properly in the package options.
#' \dontrun{
#' act::search_searchandopen_inpraat(x=examplecorpus, "pero")
#' }
#' 
#' 
#' 
search_searchandopen_inpraat <- function(x,
										 pattern) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	
	if (missing(pattern)) 	{stop("Pattern is missing.") }	
	
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
