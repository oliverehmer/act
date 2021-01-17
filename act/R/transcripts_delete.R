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
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	if (missing(transcriptNames)) 	{stop("Parameter 'transcriptNames' is missing.") 	}
	
	transcripts.deleted.ids <- which(names(x@transcripts) %in% transcriptNames)
	transcripts.deleted.names <- names(x@transcripts)[which(names(x@transcripts) %in% transcriptNames)]
	
	x@transcripts <-	x@transcripts[names(x@transcripts) %in% transcriptNames == FALSE]  
	
	
	#--- do the report
	x@history[[length(x@history)+1]] <- list(
		modification               = "transcripts_delete",
		systime                    = Sys.time(),
		transcripts.deleted.names    = transcripts.deleted.names,
		transcripts.deleted.count  = length(transcripts.deleted.ids)
	)
	return(x)
}