#' Makes a filter for transcript and tier names 
#'
#' Search a corpus object and return the names of all transcripts and tiers that match the given parameters.
#' You can define parameters to include and/or exclude transcripts and tiers based on their names.
#' All parameters passed to the function will be combined.
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
#' @example inst/examples/search_makefilter.R
#' 
search_makefilter <- function( x,
						 filterTranscriptNames        =NULL,
						 filterTranscriptIncludeRegEx =NULL, 
						 filterTranscriptExcludeRegEx =NULL,
						 filterTierNames              =NULL,
						 filterTierIncludeRegEx       =NULL,
						 filterTierExcludeRegEx       =NULL) {
	
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }

	# x<-corpus
	# filterTranscriptNames<-NULL
	# filterTranscriptIncludeRegEx<-NULL
	# filterTranscriptExcludeRegEx<-NULL
	# filterTierNames<-NULL
	# filterTierIncludeRegEx<-NULL
	# filterTierExcludeRegEx<-NULL
	# 
	# 
	# filterTranscriptIncludeRegEx <- "ARG"
	# filterTierIncludeRegEx       <- "A"
	# 
	# filterTranscriptIncludeRegEx <- "(i)A"
	
	
	#=== TRANSCRIPT
	#do some checks
	if (!is.null(filterTranscriptNames)) {		
		if (length(filterTranscriptNames)==0) {
			filterTranscriptNames <- NULL
		} else if (length(filterTranscriptNames)==1) {
			if (filterTranscriptNames[1]=="") { filterTranscriptNames <- NULL }
		}
	}
	
	#if no filter is given, take all names
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
	
	#=== TIER
	#do some checks
	if (!is.null(filterTierNames)) {		
		if (length(filterTierNames)==0) {
			filterTierNames <- NULL
		} else if (length(filterTierNames)==1) {
			if (filterTierNames[1]=="") {
				filterTierNames <- NULL
			}
		}
	}
	
	#get all tier names from selected transcripts
	tiers.all        <- lapply(x@transcripts[filterTranscriptNames], "slot", name = "tiers")
	tiers.all        <- do.call("rbind", tiers.all)
	tiernames.all    <- unique(tiers.all$name)
	if (is.null(tiernames.all)) {
		tiernames.all <- as.character()
	}
	
	if (is.null(filterTierNames)) {
		filterTierNames <- tiernames.all
	} else {
		# if tiernames have been passed to the function
		# intersect the names : only names that are in both vectors
		filterTierNames <- intersect(tiernames.all, filterTierNames)
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
	
	#=== TRANSCRIPT
	#now filter the transcripts again 
	#check for each transcript if it contains one of the included tiers
	filterTranscriptNames.new <- as.character()
	for (transcriptName in filterTranscriptNames) {
		myTrans <- x@transcripts[[transcriptName]]
		if (!is.null(myTrans)) {
			intersectingTiers <- intersect(x@transcripts[[transcriptName]]@tiers$name, filterTierNames)
			#if current transcript contains tiers in the included filterTierNames
			if (length(intersectingTiers)>0) {
				filterTranscriptNames.new <- c(filterTranscriptNames.new, transcriptName)
			}		
		}
	}	
	filterTranscriptNames <- filterTranscriptNames.new
	
	#store names in a named list
	meta <- list(transcripts.names=filterTranscriptNames, tiers.names=filterTierNames)
	#return this
	return(meta)
}
