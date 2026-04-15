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
	
	if (missing(x)) 	{cli::cli_abort("Corpus object in parameter {.arg x} is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{cli::cli_abort("Parameter {.arg x} needs to be a {.cls corpus} object.") } }
	
	#--- get names from objects in list
	names.original.ids <- c(unlist(lapply(x@transcripts, "slot", name = "name")),use.names=FALSE)
	
	return(names.original.ids)
}