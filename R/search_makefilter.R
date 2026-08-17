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
#' @param filterTranscriptIncludeRegex Character string; as regular expression, include transcripts matching the expression.
#' @param filterTranscriptExcludeRegex Character string; as regular expression, exclude transcripts matching the expression.
#' @param filterTierNames Vector of character strings; Names of the tiers that you want to include; to include all tiers in the corpus object leave parameter empty or set to  \code{character()} or \code{""}.
#' @param filterTierIncludeRegex Character string; as regular expression, include tiers matching the expression.
#' @param filterTierExcludeRegex Character string; as regular expression, exclude tiers matching the expression.
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
							 filterTranscriptIncludeRegex =NULL,
							 filterTranscriptExcludeRegex =NULL,
							 filterTierNames              =NULL,
							 filterTierIncludeRegex       =NULL,
							 filterTierExcludeRegex       =NULL) {


	.assert_corpus(x, missing = missing(x))

	#=== TRANSCRIPT
	if (!is.null(filterTranscriptNames)) {
		if (length(filterTranscriptNames)==0) {
			filterTranscriptNames <- NULL
		} else if (length(filterTranscriptNames)==1) {
			if (filterTranscriptNames[1]=="") { filterTranscriptNames <- NULL }
		}
	}

	if (is.null(filterTranscriptNames)) {	filterTranscriptNames <- names(x@transcripts)	}

	if (!is.null(filterTranscriptIncludeRegex)) {
		if (filterTranscriptIncludeRegex!="") {
			filterTranscriptNames <- grep(pattern=filterTranscriptIncludeRegex, filterTranscriptNames, value=TRUE)
		}
	}
	if (!is.null(filterTranscriptExcludeRegex)) {
		if (filterTranscriptExcludeRegex!="") {
			pos <- grep(pattern=filterTranscriptExcludeRegex, filterTranscriptNames)
			if (length(pos)>0) {
				filterTranscriptNames <- filterTranscriptNames[-pos]
			}
		}
	}

	#=== TIER
	if (!is.null(filterTierNames)) {
		if (length(filterTierNames)==0) {
			filterTierNames <- NULL
		} else if (length(filterTierNames)==1) {
			if (filterTierNames[1]=="") {
				filterTierNames <- NULL
			}
		}
	}

	tiers.all        <- lapply(x@transcripts[filterTranscriptNames], "slot", name = "tiers")
	tiers.all        <- do.call("rbind", tiers.all)
	tierNames.all    <- unique(tiers.all$name)
	if (is.null(tierNames.all)) {
		tierNames.all <- as.character()
	}

	if (is.null(filterTierNames)) {
		filterTierNames <- tierNames.all
	} else {
		filterTierNames <- intersect(tierNames.all, filterTierNames)
	}

	if (!is.null(filterTierIncludeRegex)) {
		if (filterTierIncludeRegex!="") {
			filterTierNames <- grep(pattern=filterTierIncludeRegex, filterTierNames, value=TRUE)
		}
	}
	if (!is.null(filterTierExcludeRegex)) {
		if (filterTierExcludeRegex!="") {
			pos <- grep(pattern=filterTierExcludeRegex, filterTierNames)
			if (length(pos)>0) {
				filterTierNames <- filterTierNames[-pos]
			}
		}
	}

	#=== TRANSCRIPT: keep only transcripts that contain at least one of the kept tiers
	# Vectorized via vapply (single S4 slot access per transcript, no growing c() vector).
	if (length(filterTranscriptNames) > 0) {
		has_tier <- vapply(filterTranscriptNames, function(tn) {
			tr <- x@transcripts[[tn]]
			if (is.null(tr)) return(FALSE)
			any(tr@tiers$name %in% filterTierNames)
		}, logical(1))
		filterTranscriptNames <- filterTranscriptNames[has_tier]
	}

	meta <- list(transcriptNames=filterTranscriptNames, tierNames=filterTierNames)
	return(meta)
}
