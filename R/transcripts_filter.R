#' Filter all transcripts in a corpus
#' 
#' Filter all transcript objects in a corpus and return the filtered corpus object.
#' It is possible to filter out temporal sections and tiers.
#' In case that you want to select tiers by using regular expressions use the function \code{act::search_makefilter} first.
#' 
#' @param x Corpus object;
#' @param filterTranscriptNames Vector of character strings; names of transcripts to remain in the transcripts. If left unspecified, all transcripts will  remain in the transcripts.
#' @param filterOnlyTheseTranscripts Vector of character strings; names of transcripts to which filters will be applied. If left unspecified, all transcripts will be filtered. 
#' @param filterTierNames Vector of character strings; names of tiers to remain in the transcripts. If left unspecified, all tiers will remain in the transcripts.
#' @param filterSectionStartsec Double, start of selection in seconds.
#' @param filterSectionEndsec Double, end of selection in seconds.
#' @param preserveTimes Logical; Parameter is used if \code{filterSectionStartsec} it set. If \code{TRUE} start times will be preserved, if \code{FALSE} the selection will start from 0. 
#' @param sort Logical; Annotations will be sorted: 'none' (=no sorting), 'tier>startSec' (=sort first by tier, then by startSec), 'startSec>tier' (=sort first by startSec, then by tier) 
#'
#' @return Corpus object; 
#' 
#' @export
#'
#' @example inst/examples/transcripts_filter.R
#' 
transcripts_filter <- function (x, 
						   filterTranscriptNames=NULL,
						   filterOnlyTheseTranscripts=NULL,
						   filterTierNames=NULL, 
						   filterSectionStartsec = NULL, 
						   filterSectionEndsec = NULL, 
						   preserveTimes=TRUE, 
						   sort=c("none", "tier>startSec", "startSec>tier")) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	
	tiers.deleted.count        <- 0
	tiers.deleted.ids          <- c()
	annotations.deleted.count  <- 0
	transcripts.modified.count <- 0
	transcripts.modified.ids   <- c()
	transcripts.deleted.count  <- 0
	transcripts.deleted.ids	   <- c()
	if (!is.null(filterTranscriptNames)) {
		x@transcripts <- x@transcripts[[filterTranscriptNames]]
		transcripts.deleted.count <- length(filterTranscriptNames)
		transcripts.deleted.ids<- filterTranscriptNames
	}
	
	if (is.null(filterOnlyTheseTranscripts)) {
		filterOnlyTheseTranscripts <- names(x@transcripts)
	}
	
	for (i in filterOnlyTheseTranscripts) {
		x@transcripts[[i]] <- act::transcripts_filter_single(t                     = x@transcripts[[i]],
													 filterTierNames       = filterTierNames, 
													 filterSectionStartsec = filterSectionStartsec, 
													 filterSectionEndsec   = filterSectionEndsec, 
													 preserveTimes         = preserveTimes, 
													 sort                  = sort)
		h <- x@transcripts[[i]]@history[[length(x@transcripts[[i]]@history)]]

		
		if (h$tiers.deleted.count>0 | h$annotations.deleted.count>0) {
			transcripts.modified.count  <- transcripts.modified.count+1
			transcripts.modified.ids    <- c(transcripts.modified.ids, i)
			tiers.deleted.count         <- tiers.deleted.count        + h$tiers.deleted.count  
			annotations.deleted.count   <- annotations.deleted.count  + h$annotations.deleted.count
		}
		
	} #next transcript
	
	x@history[[length(x@history)+1]] <- list(
		modification               = "transcripts_filter",
		systime                    = Sys.time(),
		transcripts.deleted.count  = transcripts.deleted.count,
		transcripts.deleted.ids    = transcripts.deleted.ids,
		transcripts.modified.count = transcripts.modified.count,
		transcripts.modified.ids   = transcripts.modified.ids,
		tiers.deleted.count        = tiers.deleted.count,
		annotations.deleted.count  = annotations.deleted.count
	)
	return (x)
}
