#' Cure a single transcript
#' 
#' Transcript object may contain errors, e.g. because of defect annotation input files or user modifications.
#' This function may cure some of these errors.
#' - Annotations with reversed times: annotations with \code{endSec} lower than \code{startSec} will be deleted.
#' - Overlapping annotations: earlier annotations will end where the next annotation starts.
#' - Annotations below 0 sec: Annotations that are starting and ending before 0 sec will be deleted; Annotations starting before but ending after 0 sec will be truncated.
#' - Missing tiers: Tiers that are present in the annotations but missing in the list of tiers in \code{@tiers} of the transcript object will be added.
#' 
#' @param t Transcript object.
#' @param annotationsWithReversedTimes Logical; If \code{TRUE} annotations with reversed times will be deleted 
#' @param annotationsWithTimesBelowZero Logical; If \code{TRUE} annotations before 0 sec will be corrected. 
#' @param overlappingAnnotations Logical; If \code{TRUE} overlapping annotations will be corrected. 
#' @param missingTiers Logical; If \code{TRUE} tiers missing in \code{@tiers} slot of the transcript object will be added. 
#' @param showWarning Logical; If \code{TRUE} a warning notice will be shown upon correction.
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
									 annotationsWithReversedTimes=TRUE, 
									 overlappingAnnotations=TRUE, 
									 annotationsWithTimesBelowZero=TRUE,
									 missingTiers=TRUE, 
									 showWarning=FALSE) {
	
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	} else { if (class(t)[[1]]!="transcript") 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	
	#--- annotationsWithReversedTimes
	annotationsWithReversedTimes.deleted.count <- 0
	if (annotationsWithReversedTimes) {
		if (nrow( t@annotations)>0) {
			#completely before 0
			ids <- which ( t@annotations$endSec<t@annotations$startSec )
			if (length(ids)>0) {
				t@annotations <- t@annotations[-ids,]
				annotationsWithReversedTimes.deleted.count <- length(ids)
			}
		}
	}
	#nrow( t@annotations)
	
	#--- overlappingAnnotations
	overlappingAnnotations.corrected.count <- 0
	if (overlappingAnnotations) {
		if (nrow( t@annotations)>1) {
			t@annotations <- t@annotations[order(t@annotations$tier.name, t@annotations$startSec), ]
			#for each tier in transcript
			tiers <- unique(t@annotations$tier.name)
			for (tier in tiers) {
				ids <- which(t@annotations$tier.name==tier)
				if (length(ids)>1) {
					for (i in 1:(length(ids)-1)) {
						if 	(t@annotations$endSec[ids[i]]>t@annotations$startSec[ids[i]+1]) {
							#t@annotations$endSec[ids[i]<-t@annotations$startSec[ids[i]+1]
							print(tier)
							print(i)
							overlappingAnnotations.corrected.count <- overlappingAnnotations.corrected.count +1
						}
					}
				}
			}
		}
	}
	#nrow(t@annotations)
	
	#--- below 0
	annotationsWithTimesBelowZero.deleted.count <- 0
	annotationsWithTimesBelowZero.corrected.count <- 0
	if (annotationsWithTimesBelowZero) {
		#completely before 0
		if (nrow( t@annotations)>0) {
			ids <- which ( t@annotations$endSec<0 & t@annotations$startSec<0 )
			if (length(ids)>0) {
				t@annotations <- t@annotations[-ids,]
				annotationsWithTimesBelowZero.deleted.count <- length(ids)
			}
		}
		
		#starting before 0
		if (nrow( t@annotations)>0) {
			ids <- which ( t@annotations$startSec<0 )
			if (length(ids)>0) {
				t@annotations$startSec[ids] <- 0
				annotationsWithTimesBelowZero.corrected.count <- length(ids)
			}
		}
	}
	#nrow(t@annotations)

	#--- @tiers in transcript object
	missingTiers.added.count <- 0 
	if (missingTiers) {
		if (nrow( t@annotations)>0) {
			#---get tier names
			#from the annotations
			tierNames <- unique(t@annotations$tier.name)

			#from transcript
			if (is.null(t@tiers)) {
				t@tiers <- .emptyTiers
			}
			if (length(t@tiers)==0) {
				t@tiers <- .emptyTiers
			}
			tierNamesInList <- 	t@tiers$name
			
			if (length(setdiff(tierNames, tierNamesInList))>0) {
				missingTiers.added.count <- length(setdiff(tierNames, tierNamesInList))
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
	#missingTiers.added.count
	
	#update history
	t@modification.systime <- Sys.time()
	t@history[[length(t@history)+1]] <-	list(
		modification                                  = "transcripts_cure_single",
		systime                                       = Sys.time(),
		annotationsWithReversedTimes.deleted.count    = annotationsWithReversedTimes.deleted.count,      
		annotationsWithTimesBelowZero.deleted.count   = annotationsWithTimesBelowZero.deleted.count,
		annotationsWithTimesBelowZero.corrected.count = annotationsWithTimesBelowZero.corrected.count,
		overlappingAnnotations.corrected.count		   = overlappingAnnotations.corrected.count,
		missingTiers.added.count				       = missingTiers.added.count
	)
	

	if (showWarning) {
		if (annotationsWithReversedTimes.deleted.count>0 | annotationsWithTimesBelowZero.deleted.count>0 | annotationsWithTimesBelowZero.corrected.count>0 | overlappingAnnotations.corrected.count>0 | missingTiers.added.count>0) {
			message <- paste(
				sprintf("Transcript '%s' cured:", t@name),
				if (annotationsWithReversedTimes.deleted.count>0)     {sprintf("\n    %s annotation(s) with reversed times deleted", annotationsWithReversedTimes.deleted.count)} else {""},
				if (annotationsWithTimesBelowZero.deleted.count>0)    {sprintf("\n    %s annotation(s) below 0 sec deleted", annotationsWithTimesBelowZero.deleted.count)} else {""},
				if (annotationsWithTimesBelowZero.corrected.count>0)  {sprintf("\n    %s annotation(s) starting before but ending after 0 sec truncated", annotationsWithTimesBelowZero.corrected.count)} else {""},
				if (overlappingAnnotations.corrected.count>0)         {sprintf("\n    %s overlapping annotation(s) corrected", overlappingAnnotations.corrected.count)} else {""},
				if (missingTiers.added.count>0)                       {sprintf("\n    %s missing tier(s) added", missingTiers.added.count)} else {""},
				sep="", collapse=""
			)
			warning(message)		
		}
	}
	return(t)
}
