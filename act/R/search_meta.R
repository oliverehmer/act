#' Find names of transcripts and tiers 
#'
#' Search a corpus object and return the names of all transcripts and tiers that match the given parameters.
#' You can define parameters to include and/or exclude transcripts and tiers based on their names.
#' All parameters that you pass to the function will be combined.
#' 
#' This functions is useful if you want to use functions of the package such as \code{transcripts_update_normalization}, \code{transcripts_update_fulltexts}, \code{corpus_export} and limit them to only some of the transcripts.
#' 
#' @param x Corpus object.
#' @param filterTranscriptNames Vector of character strings; Names of the transcripts that you want to include; to include all transcripts in the corpus object leave parameter empty or set to  \code{character()} or \code{""}.
#' @param filterTranscriptIncludeRegEx Character string; as regular expression, include transcripts matching the expression.
#' @param filterTranscriptExcludeRegEx Character string; as regular expression, exclude transcripts matching the expression.
#' @param filterTierNames Vector of character strings; Names of the tiers that you want to include; to include all tiers in the corpus object leave parameter empty or set to  \code{character()} or \code{""}.
#' @param filterTierIncludeRegEx Character string; as regular expression, include tiers matching the expression.
#' @param filterTierExcludeRegEx Character string; as regular expression, exclude tiers matching the expression.
#'
#' @return List of character vectors. \code{$filterTranscriptNames} contains all transcript names in the corpus matching the expressions, \code{$filterTierNames} contains all tier names in the corpus matching the expressions.
#' 
#' @seealso \link{search_new}, \link{search_run}, \link{search_sub}
#' 
#' @export
#'
#' @example inst/examples/search_meta.R
#' 
search_meta <- function( x,
						 filterTranscriptNames=NULL,
						 filterTranscriptIncludeRegEx=NULL, 
						 filterTranscriptExcludeRegEx=NULL,
						 filterTierNames=NULL,
						 filterTierIncludeRegEx=NULL,
						 filterTierExcludeRegEx=NULL) {
	
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }

	#=== get the transcript names
	#if none are given, take all names
	if (is.null(filterTranscriptNames)) {		
		filterTranscriptNames <- NULL
	} else if (length(filterTranscriptNames)==0) {
		filterTranscriptNames <- NULL
	} else if (length(filterTranscriptNames)==1) {
		if (filterTranscriptNames[1]=="") { filterTranscriptNames <- NULL }
	}
	if (is.null(filterTranscriptNames)) {	filterTranscriptNames <- names(x@transcripts)	}
	
	
	#filter the names by regular expressions
	if (!is.null(filterTranscriptIncludeRegEx)) {
		if (filterTranscriptIncludeRegEx!="") {
			filterTranscriptNames <- grep(pattern=filterTranscriptIncludeRegEx, filterTranscriptNames, value=TRUE)
		}
	}
	if (!is.null(filterTranscriptExcludeRegEx)) {
		if (filterTranscriptExcludeRegEx!="") {
			filterTranscriptNames <- filterTranscriptNames[-grep(pattern=filterTranscriptExcludeRegEx, filterTranscriptNames)]
		}
	}
	
	#if filterTierNames are not specified, get all tiers (in selected transcripts)
	if (is.null(filterTranscriptNames)) {		
		filterTierNames <- NULL
	} else if (length(filterTierNames)==0) {
		filterTierNames <- NULL
	} else if (length(filterTierNames)==1) {
		if (filterTierNames[1]=="") {
			filterTierNames <- NULL
		}
	}
	
	if (is.null(filterTierNames)) {
		tiers           <- lapply(x@transcripts, "slot", name = "tiers")
		temp            <- do.call("rbind", tiers)
		filterTierNames <- unique(temp$name)
	}
	
	#filter the filterTierNames by regular expressions
	if (!is.null(filterTierIncludeRegEx)) {
		if (filterTierIncludeRegEx!="") {
			filterTierNames <- grep(pattern=filterTierIncludeRegEx, filterTierNames, value=TRUE)
		}
	}
	if (!is.null(filterTierExcludeRegEx)) {
		if (filterTierExcludeRegEx!="") {
			filterTierNames <- filterTierNames[-grep(pattern=filterTierExcludeRegEx, filterTierNames)]
		}
	}
	
	#now filter the transcripts again 
	#check for each transcript if it contains one of the included tiers
	filterTranscriptNames.new <- c()
	for (transcriptName in filterTranscriptNames) {
		intersectingTiers <- intersect(x@transcripts[[transcriptName]]@tiers$name, filterTierNames)
		#if current transcript contains tiers in the included filterTierNames
		if (length(intersectingTiers)>0) {
			filterTranscriptNames.new <- c(filterTranscriptNames.new, transcriptName)
		}
	}	
	filterTranscriptNames <- filterTranscriptNames.new
	
	#store names in a named list
	meta <- list(transcripts.names=filterTranscriptNames, tiers.names=filterTierNames)
	#return this
	return(meta)
}
