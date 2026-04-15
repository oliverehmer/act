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
#' @example inst/examples/helper_transcript_names_set.R
#' 
helper_transcript_names_set <- function(x, transcriptNames) {
	
	if (missing(x)) 	{cli::cli_abort("Corpus object in parameter {.arg x} is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{cli::cli_abort("Parameter {.arg x} needs to be a {.cls corpus} object.") } }
	
	#--- get names from objects in list
	names.original.ids <- c(unlist(lapply(x@transcripts, "slot", name = "name")), use.names=FALSE)
	
	transcriptNames[is.na(transcriptNames)] <- ""
	transcriptNames <- stringr::str_trim(transcriptNames)
	
	#--- check equal length
	if (length(x@transcripts)!=length(transcriptNames)) {
		cli::cli_abort("Length of {.arg transcriptNames} does not match the number of transcripts in {.arg x}.")
	}
	
	#--- check empty names
	if (any(transcriptNames=="")) {
		cli::cli_abort("Parameter {.arg transcriptNames} contains empty names. Possibly you ill-defined your regular expressions for {.arg namesExtractPatterns}, {.arg namesSearchPatterns} and/or {.arg namesSearchReplacements}.")
	}
	
	#--- check duplicates
	if (any(duplicated(transcriptNames))) {
		cli::cli_abort("Parameter {.arg transcriptNames} contains duplicates.")
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