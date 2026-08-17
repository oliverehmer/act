#' Rename transcripts in a corpus
#' 
#' Rename transcript objects in a corpus object.
#' This function changes both the names of the transcripts in the list \code{x@transcripts} and in the \code{@name} slot of the transcript.
#' The function ensures that each transcript object preserves a unique name. 
#'
#' @param x Corpus object
#' @param newTranscriptNames Vector of character strings; new names for the transcripts. If left open, the current names in the corpus object will be taken as basis. 
#' @param searchPatterns Character string; Search pattern as regular expression applied to the names of the transcripts.
#' @param searchReplacements Character string; String to replace the hits of the search.
#' @param toUpper Logical; Convert transcript names all to upper case.
#' @param toLower Logical; Convert transcript names all to lower case.
#' @param trim Logical; Remove leading and trailing spaces in names.
#' @param stopNonUnique Logical; If \code{TRUE} the function will stop if replacement would lead to non-unique names; If \code{FALSE} names will be automatically changed to be unique. 
#'
#' @return Corpus object
#' @export 
#'
#' @example inst/examples/transcripts_rename.R
#' 
transcripts_rename <- function(x, 
							   newTranscriptNames  = NULL, 
							   searchPatterns      = NULL, 
							   searchReplacements  = NULL, 
							   toUpper         = FALSE,
							   toLower         = FALSE,
							   trim                = FALSE,
							   stopNonUnique     = TRUE ) {
	
	.assert_corpus(x, missing = missing(x))
	
	#--- check
	if (is.null(searchPatterns)) {
		searchPatterns <- character()
	}
	if (is.null(searchReplacements)) {
		searchReplacements <- character()
	}
	
	#---get names
	oldtranscriptNames <- act::helper_transcript_names_get(x)
	
	if (is.null(newTranscriptNames)) 	{
		newTranscriptNames <- oldtranscriptNames
	} else {
		if (length(newTranscriptNames)!=length(x@transcripts)) {
			cli::cli_abort("Parameter {.arg newTranscriptNames} is not of the same length as list of transcripts in the corpus: {length(newTranscriptNames)} vs {length(x@transcripts)}.")
		}
	}
	
	#--- perform the rename operations
	transcriptNames.info <- helper_transcript_names_make(transcriptNames      = newTranscriptNames,
														searchPatterns       = searchPatterns,
														searchReplacements   = searchReplacements,
														toUpper          = toUpper,
														toLower          = toLower,
														trim                 = trim,
														defaultEmpty = "no_name")
	
	#--- check if there were duplicated names
	if (stopNonUnique) {
		if(length(transcriptNames.info$duplicated.ids)>0) {
			cli::cli_abort("Transcript names would not be unique after renaming. Nothing renamed.")
		}
	}
	
	#--- rename
	x <- act::helper_transcript_names_set(x, transcriptNames.info$names.ok.ids)
	
	#HISTORY corpus: update history
	x@history[[length(x@history)+1]] <- list(
		modification               = "transcripts_rename",
		systime                    = Sys.time(),
		transcripts.renamed.ids    = setdiff(oldtranscriptNames, names(x@transcripts)),
		transcripts.renamed.count  = length(setdiff(oldtranscriptNames, names(x@transcripts)))
	)
	
	return(x)
}
