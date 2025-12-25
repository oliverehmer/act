#' Filter a single transcript: Extract
#' 
#' Filter all transcript objects in a corpus and return the filtered corpus object.
#' It is possible to filter out temporal sections and tiers.
#' In case that you want to select tiers by using regular expressions use the function \code{act::search_makefilter} first.
#' 
#' @param x Corpus object;
#' @param filterTranscriptNames Vector of character strings; names of transcripts to remain. If left unspecified, all transcripts will  remain in the transcripts.
#' @param filterTranscriptsRestict Vector of character strings; names of transcripts to which filters will be applied. If left unspecified, all transcripts will be filtered. 
#' @param filterTierNames Vector of character strings; names of tiers to remain in the transcripts. If left unspecified, all tiers will remain in the transcripts.
#' @param filterSectionStartsec Double, start of selection in seconds.
#' @param filterSectionEndsec Double, end of selection in seconds.
#' @param timesPreserve Logical; Parameter is used if \code{filterSectionStartsec} it set. If \code{TRUE} start times will be preserved, if \code{FALSE} the selection will start from 0. 
#' @param sort Logical; Annotations will be sorted: 'none' (=no sorting), 'tier>startsec' (=sort first by tier, then by startsec), 'startsec>tier' (=sort first by startsec, then by tier) 
#'
#' @return Corpus object; 
#' 
#' @export
#'
#' @example inst/examples/transcripts_filter.R
#' 
transcripts_filter <- function (x, 
						   filterTranscriptNames=NULL,
						   filterTranscriptsRestict=NULL,
						   filterTierNames=NULL, 
						   filterSectionStartsec = NULL, 
						   filterSectionEndsec = NULL, 
						   timesPreserve=TRUE, 
						   sort=c("none", "tier>startsec", "startsec>tier")) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{stop("Parameter 'x' needs to be a corpus object.") } }
	
	
	#--- check parameter 'filterTierNames'
	if (!is.null(filterTierNames)) {
		if (length(filterTierNames)>0) {
			if (!is.vector(filterTierNames)) {
				{stop("Parameter 'filterTierNames' needs to be a vector containing names of tiers.") 	}
			}
			if (!is.atomic(filterTierNames)) {
				{stop("Parameter 'filterTierNames' needs to be a vector containing names of tiers.") 	}
			}
			if (!is.character(filterTierNames)) {
				{stop("Parameter 'filterTierNames' needs to be a vector containing names of tiers.") 	}
			}
		}
	}
	#--- check parameter 'filterTranscriptNames'
	if (!is.null(filterTranscriptNames)) {
		if (length(filterTranscriptNames)>0) {
			if (!is.vector(filterTranscriptNames)) {
				{stop("Parameter 'filterTranscriptNames' needs to be a vector containing names of tiers.") 	}
			}
			if (!is.atomic(filterTranscriptNames)) {
				{stop("Parameter 'filterTranscriptNames' needs to be a vector containing names of tiers.") 	}
			}
			if (!is.character(filterTranscriptNames)) {
				{stop("Parameter 'filterTranscriptNames' needs to be a vector containing names of tiers.") 	}
			}
		}
	}	
	
	
	
	tiersDeleted.count        <- 0
	tiersDeleted.ids          <- c()
	annotations.deleted.count  <- 0
	transcripts.modified.ids   <- c()
	transcripts.deleted.count  <- 0
	transcripts.deleted.ids	   <- c()
	if (!is.null(filterTranscriptNames)) {
		x@transcripts <- x@transcripts[filterTranscriptNames]
		transcripts.deleted.count <- length(filterTranscriptNames)
		transcripts.deleted.ids<- filterTranscriptNames
	}
	
	if (is.null(filterTranscriptsRestict)) {
		filterTranscriptsRestict <- names(x@transcripts)
	}
	
	for (i in filterTranscriptsRestict) {
		x@transcripts[[i]] <- act::transcripts_filter_single(t             = x@transcripts[[i]],
													 filterTierNames       = filterTierNames, 
													 filterSectionStartsec = filterSectionStartsec, 
													 filterSectionEndsec   = filterSectionEndsec, 
													 timesPreserve         = timesPreserve, 
													 sort                  = sort)
		
		#HISTORY transcript:
		#realized in transcripts_filter_single
		
		h <- x@transcripts[[i]]@history[[length(x@transcripts[[i]]@history)]]
		if (h$tiersDeleted.count>0 | h$annotations.deleted.count>0) {
			transcripts.modified.ids    <- c(transcripts.modified.ids, i)
			tiersDeleted.count         <- tiersDeleted.count        + h$tiersDeleted.count  
			annotations.deleted.count   <- annotations.deleted.count  + h$annotations.deleted.count
		}
	} #next transcript
	
	#HISTORY corpus: update history
	x@history[[length(x@history)+1]] <- list(
		modification               = "transcripts_filter",
		systime                    = Sys.time(),
		transcripts.deleted.count  = transcripts.deleted.count,
		transcripts.deleted.ids    = transcripts.deleted.ids,
		transcripts.modified.count = length(transcripts.deleted.ids),
		transcripts.modified.ids   = transcripts.modified.ids,
		tiersDeleted.count        = tiersDeleted.count,
		annotations.deleted.count  = annotations.deleted.count
	)
	return (x)
}
