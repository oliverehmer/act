#' Filter a single transcript: Extract
#' 
#' Filter a transcript object and return the filtered transcript object.
#' It is possible to EXTRACT temporal sections and tiers.
#' In case that you want to select tiers by using regular expressions use the function \code{act::search_makefilter} first.
#' 
#' @param t Transcript object.
#' @param filterTierNames Vector of character strings; names of tiers to be remain in the transcripts. If left unspecified, all tiers will remain in the transcript exported.
#' @param filterSectionStartsec Double, start of selection in seconds.
#' @param filterSectionEndsec Double, end of selection in seconds.
#' @param timesPreserve Logical; Parameter is used if \code{filterSectionStartsec} it set. If \code{TRUE} start times will be preserved, if \code{FALSE} the selection will start from 0. 
#' @param sort Logical; Annotations will be sorted: 'none' (=no sorting), 'tier>startsec' (=sort first by tier, then by startsec), 'startsec>tier' (=sort first by startsec, then by tier) 
#'
#' @return Transcript object; 
#' 
#' @export
#'
#' @example inst/examples/transcripts_filter_single.R
#' 
transcripts_filter_single <- function (t, 
							   filterTierNames       = NULL, 
							   filterSectionStartsec = NULL, 
							   filterSectionEndsec   = NULL, 
							   timesPreserve         = TRUE, 
							   sort=c("none", "tier>startsec", "startsec>tier")) {
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
	tiersDeleted.ids <- c()
	annotations.deleted.count <- 0
	

	if (!is.null(filterTierNames)) {
			if (length(filterTierNames)>0) {
				tiersDeleted.ids <- setdiff( t@tiers$name, filterTierNames)
				#tier names
				ids <- which(t@tiers$name %in% filterTierNames)
				t@tiers <- t@tiers[ids, ]
				
				#annotations
				annotations.deleted.count <- annotations.deleted.count + nrow( t@annotations) 
				t@annotations <- t@annotations[t@annotations$tierName %in% filterTierNames, ]
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
				t@annotations <-  t@annotations[t@annotations$startsec<filterSectionEndsec, ]
				annotations.deleted.count <- annotations.deleted.count - nrow( t@annotations)
				
				#set times correctly
				if (nrow(t@annotations)>0) {
					t@annotations$endsec[t@annotations$endsec > filterSectionEndsec] <- filterSectionEndsec
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
				t@annotations <-  t@annotations[ t@annotations$endsec>filterSectionStartsec , ]
				annotations.deleted.count <- annotations.deleted.count - nrow( t@annotations)
				
				if (nrow( t@annotations)>0) { 
					#set times correctly
					t@annotations$startsec[t@annotations$startsec < filterSectionStartsec] <- filterSectionStartsec
					#preserve times?
					if (!timesPreserve) {
						t@annotations$startsec <- t@annotations$startsec - filterSectionStartsec
						t@annotations$endsec   <- t@annotations$endsec   - filterSectionStartsec
					}
				}
			}
		}
	}
	
	if (is.factor(t@annotations$tierName)) {
		t@annotations$tierName <- droplevels(t@annotations$tierName)
	}	
	
	#------ sort
	if (nrow( t@annotations)>0) {
		sort <- sort[1]
		if (sort=='tier>startsec') {
			#sort annotations by tier names and start time
			t@annotations <- t@annotations[order(ordered(t@annotations$tierName, levels = t@tiers$name), t@annotations$startsec),]
		} else if (sort=='startsec>tier') {
			#sort annotations by tier names and start time
			t@annotations <- t@annotations[order(t@annotations$startsec, ordered(t@annotations$tierName, levels = 	t@tiers$name)),]
		} else {
		}
	}
	
	#HISTORY transcript
	t@modification.systime <- Sys.time()
	t@history[[length(t@history)+1]] <-	list(
		modification              = "transcripts_filter_single",
		systime                   = Sys.time(),
		tiersDeleted.count       = length(tiersDeleted.ids),
		tiersDeleted.ids         = tiersDeleted.ids,
		annotations.deleted.count = annotations.deleted.count
	)
	
	return(t)
}