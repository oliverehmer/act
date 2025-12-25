#' Filter a single transcript: Remove
#' 
#' Filter a transcript object and return the filtered transcript object.
#' It is possible to REMOVE temporal sections and tiers.
#' In case that you want to select tiers by using regular expressions use the function \code{act::search_makefilter} first.
#' 
#' @param t Transcript object.
#' @param filterTierNames Vector of character strings; names of tiers to be remain in the transcripts. If left unspecified, all tiers will remain in the transcript exported.
#' @param filterSectionStartsec Double, start of selection in seconds.
#' @param filterSectionEndsec Double, end of selection in seconds.
#' @param sort Logical; Annotations will be sorted: 'none' (=no sorting), 'tier>startsec' (=sort first by tier, then by startsec), 'startsec>tier' (=sort first by startsec, then by tier) 
#'
#' @return Transcript object; 
#' 
#' @export
#'
#' @example inst/examples/transcripts_filter_single.R
#' 
transcripts_filter_remove_single <- function (t, 
									   filterTierNames=NULL, 
									   filterSectionStartsec = NULL, 
									   filterSectionEndsec = NULL, 
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
	
	
	#--- check parameter 'filterSection____sec'
	if (!is.null(filterSectionStartsec)) {
		filterSectionStartsec<- as.double(filterSectionStartsec)
		if (filterSectionStartsec<0) {
			{stop("Parameter 'filterSectionStartsec' needs to be at least 0") 	}
		}
		if (is.null(filterSectionEndsec)) {
			stop("If you set 'filterSectionStartsec' you also need to set 'filterSectionEndsec'") 
			filterSectionEndsec<- as.double(filterSectionEndsec)
		}	
	} else {
		if (!is.null(filterSectionEndsec)) {
			stop("If you set 'filterSectionEndsec' you also need to set 'filterSectionStartsec'") 
		}	
	}
	
	
	

	
	#--- tiers
	tiersDeleted.ids <- c()
	annotations.deleted.count <- 0
	
	if (!is.null(filterTierNames)) {
		if (length(filterTierNames)>0) {
			#delete
			tiersDeleted.ids <- which(t@tiers$name %in% filterTierNames)
				
			#keep
			ids <- setdiff(t@tiers$name, filterTierNames)
			t@tiers <- t@tiers[ids, ]
			
			#annotations
			annotations.deleted.count <- annotations.deleted.count + nrow( t@annotations) 
			t@annotations <- t@annotations[t@annotations$tierName %in% setdiff(t@tiers$name, filterTierNames), ]
			annotations.deleted.count <- annotations.deleted.count - nrow( t@annotations)
		}
	}
	
	#--- time
	IDs<-0
	if (nrow( t@annotations)>0) {
		if (!is.null(filterSectionStartsec) ) {

				#filter
				#remove those
				IDs <- which(t@annotations$startsec >= filterSectionStartsec & t@annotations$endsec <= filterSectionEndsec)
				t@annotations <-  t@annotations[-IDs, ]
				
				if (nrow( t@annotations)>0) { 
					#set times correctly
					
					#start before end of cut and end after cut
					t@annotations$startsec[t@annotations$startsec < filterSectionEndsec & t@annotations$endsec > filterSectionEndsec] <- filterSectionEndsec
					
					#start before start of cut and end after start of cut
					t@annotations$endsec[t@annotations$startsec < filterSectionStartsec & t@annotations$endsec > filterSectionStartsec] <- filterSectionStartsec
				}
			}
	}
	annotations.deleted.count <- annotations.deleted.count +length(IDs)
	
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