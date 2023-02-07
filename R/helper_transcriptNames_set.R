#' Helper: Set names of all transcripts in a corpus
#' 
#' Sets the names of all transcript objects in a corpus object both in the names of the list \code{x@transcripts} and in the slot \code{@name} of each transcript.
#' 
#' @param x Corpus object
#' @param transcriptNames Vector of character strings; new names.
#'
#' @return List
#' @export
#'
#' @example inst/examples/helper_transcriptNames_set.R
#' 
helper_transcriptNames_set <- function(x, transcriptNames) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{stop("Parameter 'x' needs to be a corpus object.") } }
	
	#--- get names from objects in list
	names.original.ids <- c(unlist(lapply(x@transcripts, "slot", name = "name")), use.names=FALSE)

	transcriptNames <- stringr::str_trim(transcriptNames)
	
	#--- check equal length
	if (length(x@transcripts)!=length(transcriptNames)) {
		stop("Length of 'transcriptNames' does not match the number of transcripts in 'x'.")
	}
	
	#--- check empty names
	if (any(transcriptNames=="")) {
		stop("Parameter 'transcriptNames' contains empty names.")
	}
	
	#--- check duplicates
	if (any(duplicated(transcriptNames))) {
		stop("Parameter 'transcriptNames' contains duplicates.")
	}
	
	#--- set names
	if (length(x@transcripts)>0 ){
		for (i in 1:length(x@transcripts)) {
			x@transcripts[[i]]@name <- transcriptNames[i]
		}
		names(x@transcripts) <- transcriptNames
	}
	return(x)
}