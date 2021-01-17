#' Helper: Get names of all transcripts in a corpus
#' 
#' Gets the names of all transcript objects in a corpus object based from the \code{@name} attribute of each transcript.
#' 
#' @param x Corpus object
#'
#' @return List
#' @export
#'
#' @example inst/examples/helper_transcriptNames_get.R
#' 
helper_transcriptNames_get <- function(x) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	
	#--- get names from objects in list
	names.original.ids <- c(unlist(lapply(x@transcripts, "slot", name = "name")),use.names=FALSE)
	
	return(names.original.ids)
}