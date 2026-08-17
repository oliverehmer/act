#' Helper: Get names of all transcripts in a corpus
#' 
#' Gets the names of all transcript objects in a corpus object based from the \code{@name} attribute of each transcript.
#' 
#' @param x Corpus object
#'
#' @return List
#' @export
#'
#' @example inst/examples/helper_transcript_names_get.R
#' 
helper_transcript_names_get <- function(x) {
	
	.assert_corpus(x, missing = missing(x))
	
	#--- get names from objects in list
	names.original.ids <- c(unlist(lapply(x@transcripts, "slot", name = "name")),use.names=FALSE)
	
	return(names.original.ids)
}
