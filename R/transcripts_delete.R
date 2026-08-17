#' Delete transcripts from a corpus
#' 
#' Delete transcript objects from a corpus object.
#' You need to name the transcripts to delete directly in the parameter 'transcriptNames'.
#' If you want to delete transcripts based on a search pattern (regular expression) use \code{act::search_sub} first.
#' 
#' @param x Corpus object
#' @param transcriptNames Vector of character strings; names of the transcript object to be deleted. 
#'
#' @return Corpus object
#' @export 
#'
#' @example inst/examples/transcripts_delete.R
transcripts_delete <- function(x, transcriptNames) {
	
	.assert_corpus(x, missing = missing(x))
	if (missing(transcriptNames)) 	{cli::cli_abort("Parameter {.arg transcriptNames} is missing.") 	}
	
	transcripts.deleted.ids <- which(names(x@transcripts) %in% transcriptNames)
	transcripts.deleted.names <- names(x@transcripts)[which(names(x@transcripts) %in% transcriptNames)]
	
	x@transcripts <-	x@transcripts[names(x@transcripts) %in% transcriptNames == FALSE]  
	
	
	#HISTORY corpus: update history
	x@history[[length(x@history)+1]] <- list(
		modification               = "transcripts_delete",
		systime                    = Sys.time(),
		transcripts.deleted.names  = transcripts.deleted.names,
		transcripts.deleted.count  = length(transcripts.deleted.ids)
	)
	return(x)
}
