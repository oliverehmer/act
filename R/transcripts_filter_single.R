#' Filter a single transcript
#' 
#' Filter a transcript object and return the filtered transcript object.
#' It is possible to filter out temporal sections and tiers.
#' In case that you want to select tiers by using regular expressions use the function \code{act::search_makefilter} first.
#' 
#' @param t Transcript object.
#' @param filterTierNames Vector of character strings; names of tiers to be remain in the transcripts. If left unspecified, all tiers will remain in the transcript exported.
#' @param filterSectionStartsec Double, start of selection in seconds.
#' @param filterSectionEndsec Double, end of selection in seconds.
#' @param preserveTimes Logical; Parameter is used if \code{filterSectionStartsec} it set. If \code{TRUE} start times will be preserved, if \code{FALSE} the selection will start from 0. 
#' @param sort Logical; Annotations will be sorted: 'none' (=no sorting), 'tier>startSec' (=sort first by tier, then by startSec), 'startSec>tier' (=sort first by startSec, then by tier) 
#'
#' @return Transcript object; 
#' 
#' @export
#'
#' @example inst/examples/transcripts_filter_single.R
#' 
transcripts_filter_single <- function (t, 
							   filterTierNames=NULL, 
							   filterSectionStartsec = NULL, 
							   filterSectionEndsec = NULL, 
							   preserveTimes=TRUE, 
							   sort=c("none", "tier>startSec", "startSec>tier")) {
	#=== settings
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	}	else { if (!methods::is(t, "transcript")) 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	
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
	
	#--- tiers
	tiers.deleted.ids <- c()
	annotations.deleted.count <- 0
	

	if (!is.null(filterTierNames)) {
			if (length(filterTierNames)>0) {
				tiers.deleted.ids <- setdiff( t@tiers$name, filterTierNames)
				#tier names
				ids <- which(t@tiers$name %in% filterTierNames)
				t@tiers <- t@tiers[ids, ]
				
				#annotations
				annotations.deleted.count <- annotations.deleted.count + nrow( t@annotations) 
				t@annotations <- t@annotations[t@annotations$tier.name %in% filterTierNames, ]
				annotations.deleted.count <- annotations.deleted.count - nrow( t@annotations)
			}
	}
	
	#--- time
	if (nrow( t@annotations)>0) {
		if (!is.null(filterSectionEndsec)) {
			if (length(filterSectionEndsec)>0) {
				filterSectionEndsec <- as.double(filterSectionEndsec)
				
				#filter
				annotations.deleted.count <- annotations.deleted.count + nrow( t@annotations) 
				t@annotations <-  t@annotations[t@annotations$startSec<filterSectionEndsec, ]
				annotations.deleted.count <- annotations.deleted.count - nrow( t@annotations)
				
				#set times correctly
				if (nrow(t@annotations)>0) {
					t@annotations$endSec[t@annotations$endSec > filterSectionEndsec] <- filterSectionEndsec
				}
			}
		}
	}
	if (nrow( t@annotations)>0) {
		if (!is.null(filterSectionStartsec) ) {
			if (length(filterSectionStartsec)>0) {
				filterSectionStartsec <- as.double(filterSectionStartsec)
				
				#filter
				annotations.deleted.count <- annotations.deleted.count + nrow( t@annotations) 
				t@annotations <-  t@annotations[ t@annotations$endSec>filterSectionStartsec , ]
				annotations.deleted.count <- annotations.deleted.count - nrow( t@annotations)
				
				if (nrow( t@annotations)>0) { 
					#set times correctly
					t@annotations$startSec[t@annotations$startSec < filterSectionStartsec] <- filterSectionStartsec
					#preserve times?
					if (!preserveTimes) {
						t@annotations$startSec <- t@annotations$startSec - filterSectionStartsec
						t@annotations$endSec   <- t@annotations$endSec   - filterSectionStartsec
					}
				}
			}
		}
	}
	
	if (is.factor(t@annotations$tier.name)) {
		t@annotations$tier.name <- droplevels(t@annotations$tier.name)
	}	
	
	#------ sort
	if (nrow( t@annotations)>0) {
		sort <- sort[1]
		if (sort=='tier>startSec') {
			#sort annotations by tier names and start time
			t@annotations <- t@annotations[order(ordered(t@annotations$tier.name, levels = t@tiers$name), t@annotations$startSec),]
		} else if (sort=='startSec>tier') {
			#sort annotations by tier names and start time
			t@annotations <- t@annotations[order(t@annotations$startSec, ordered(t@annotations$tier.name, levels = 	t@tiers$name)),]
		} else {
		}
	}
	
	#HISTORY transcript
	t@modification.systime <- Sys.time()
	t@history[[length(t@history)+1]] <-	list(
		modification              = "transcripts_filter_single",
		systime                   = Sys.time(),
		tiers.deleted.count       = length(tiers.deleted.ids),
		tiers.deleted.ids         = tiers.deleted.ids,
		annotations.deleted.count = annotations.deleted.count
	)
	
	return(t)
}