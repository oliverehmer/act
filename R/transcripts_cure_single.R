#' Cure a single transcript
#' 
#' Transcript object may contain errors, e.g. because of defect annotation input files or user modifications.
#' This function may cure some of these errors.
#' - Annotations with reversed times: annotations with \code{endsec} lower than \code{startsec} will be deleted.
#' - Overlapping annotations: earlier annotations will end where the next annotation starts.
#' - Annotations below 0 sec: Annotations that are starting and ending before 0 sec will be deleted; Annotations starting before but ending after 0 sec will be truncated.
#' - Missing tiers: Tiers that are present in the annotations but missing in the list of tiers in \code{@tiers} of the transcript object will be added.
#' 
#' @param t Transcript object.
#' @param annotationsTimesReversed Logical; If \code{TRUE} annotations with reversed times will be deleted 
#' @param annotationsTimesBelowZero Logical; If \code{TRUE} annotations before 0 sec will be corrected. 
#' @param annotationsOverlap Logical; If \code{TRUE} overlapping annotations will be corrected. 
#' @param tiersMissing Logical; If \code{TRUE} tiers missing in \code{@tiers} slot of the transcript object will be added. 
#' @param warning Logical; If \code{TRUE} a warning notice will be shown upon correction.
#'
#' @return Transcript object; 
#' 
#' @seealso \link{transcripts_cure}
#' 
#' @export
#'
#' @example inst/examples/transcripts_cure_single.R
#' 
transcripts_cure_single <- function (t, 
									 annotationsTimesReversed=TRUE, 
									 annotationsOverlap=TRUE, 
									 annotationsTimesBelowZero=TRUE,
									 tiersMissing=TRUE, 
									 warning=FALSE) {
	
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	}	else { if (!methods::is(t, "transcript")) 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	
	#--- annotationsTimesReversed
	annotationsTimesReversed.deleted.count <- 0
	if (annotationsTimesReversed) {
		if (nrow( t@annotations)>0) {
			#completely before 0
			ids <- which ( t@annotations$endsec<t@annotations$startsec )
			if (length(ids)>0) {
				t@annotations <- t@annotations[-ids,]
				annotationsTimesReversed.deleted.count <- length(ids)
			}
		}
	}
	#nrow( t@annotations)
	
	#--- annotationsOverlap
	annotationsOverlap.corrected.count <- 0
	if (annotationsOverlap) {
		if (nrow( t@annotations)>1) {
			t@annotations <- t@annotations[order(t@annotations$tierName, t@annotations$startsec), ]
			#for each tier in transcript
			tiers <- unique(t@annotations$tierName)
			for (tier in tiers) {
				ids <- which(t@annotations$tierName==tier)
				if (length(ids)>1) {
					for (i in 1:(length(ids)-1)) {
						if 	(t@annotations$endsec[ids[i]]>t@annotations$startsec[ids[i]+1]) {
							#t@annotations$endsec[ids[i]<- t@annotations$startsec[ids[i]+1]
							#print(tier)
							#print(i)
							annotationsOverlap.corrected.count <- annotationsOverlap.corrected.count +1
						}
					}
				}
			}
		}
	}
	#nrow(t@annotations)
	
	#--- below 0
	annotationsTimesBelowZero.deleted.count <- 0
	annotationsTimesBelowZero.corrected.count <- 0
	if (annotationsTimesBelowZero) {
		#completely before 0
		if (nrow( t@annotations)>0) {
			ids <- which ( t@annotations$endsec<0 & t@annotations$startsec<0 )
			if (length(ids)>0) {
				t@annotations <- t@annotations[-ids,]
				annotationsTimesBelowZero.deleted.count <- length(ids)
			}
		}
		
		#starting before 0
		if (nrow( t@annotations)>0) {
			ids <- which ( t@annotations$startsec<0 )
			if (length(ids)>0) {
				t@annotations$startsec[ids] <- 0
				annotationsTimesBelowZero.corrected.count <- length(ids)
			}
		}
	}
	#nrow(t@annotations)

	#--- @tiers in transcript object
	tiersMissing.added.count <- 0 
	if (tiersMissing) {
		if (nrow( t@annotations)>0) {
			#---get tier names
			#from the annotations
			tierNames <- unique(t@annotations$tierName)

			#from transcript
			if (is.null(t@tiers)) {
				t@tiers <- .emptyTiers
			}
			if (length(t@tiers)==0) {
				t@tiers <- .emptyTiers
			}
			tierNamesInList <- 	t@tiers$name
			
			if (length(setdiff(tierNames, tierNamesInList))>0) {
				tiersMissing.added.count <- length(setdiff(tierNames, tierNamesInList))
				#join all names
				if (!is.null(tierNamesInList)) {
					tierNames 		<- union(tierNamesInList, tierNames)
				}
				
				#---get tier classes
				tierTypesInList <- t@tiers$type
				names(tierTypesInList) <- NULL
				#add types if missing, assume they are interval tiers
				if (is.null(tierTypesInList))	{
					#if no classes are found at all
					tierTypes <- rep("IntervalTier", length(tierNames))
				} else	{
					#if some are found, add only classes for the ones that are missing
					missing <- length(tierNames)-length(tierTypesInList)
					if (missing>0) {
						tierTypesInList <- c(tierTypesInList,rep("IntervalTier", missing))
					}
					tierTypes <- tierTypesInList
				}
				
				#--- tiers to object
				t@tiers <- act::helper_tiers_new_table(tierNames=tierNames, tierTypes=tierTypes)
			}
		}
	}
	#tiersMissing.added.count
	
	#HISTORY transcript
	t@modification.systime <- Sys.time()
	t@history[[length(t@history)+1]] <-	list(
		modification                                  = "transcripts_cure_single",
		systime                                       = Sys.time(),
		annotationsTimesReversed.deleted.count    = annotationsTimesReversed.deleted.count,      
		annotationsTimesBelowZero.deleted.count   = annotationsTimesBelowZero.deleted.count,
		annotationsTimesBelowZero.corrected.count = annotationsTimesBelowZero.corrected.count,
		annotationsOverlap.corrected.count		  = annotationsOverlap.corrected.count,
		tiersMissing.added.count				      = tiersMissing.added.count
	)
	

	if (warning) {
		if (annotationsTimesReversed.deleted.count>0 | annotationsTimesBelowZero.deleted.count>0 | annotationsTimesBelowZero.corrected.count>0 | annotationsOverlap.corrected.count>0 | tiersMissing.added.count>0) {
			message <- paste(
				sprintf("Transcript '%s' cured:", t@name),
				if (annotationsTimesReversed.deleted.count>0)     {sprintf("\n    %s annotation(s) with reversed times deleted", annotationsTimesReversed.deleted.count)} else {""},
				if (annotationsTimesBelowZero.deleted.count>0)    {sprintf("\n    %s annotation(s) below 0 sec deleted", annotationsTimesBelowZero.deleted.count)} else {""},
				if (annotationsTimesBelowZero.corrected.count>0)  {sprintf("\n    %s annotation(s) starting before but ending after 0 sec truncated", annotationsTimesBelowZero.corrected.count)} else {""},
				if (annotationsOverlap.corrected.count>0)         {sprintf("\n    %s overlapping annotation(s) corrected", annotationsOverlap.corrected.count)} else {""},
				if (tiersMissing.added.count>0)                       {sprintf("\n    %s missing tier(s) added", tiersMissing.added.count)} else {""},
				sep="", collapse=""
			)
			warning(unique(message))	
		}
	}
	return(t)
}
