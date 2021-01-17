#' Cure a corpus
#' 
#' Transcript object may contain errors, e.g. because of defect annotation input files or user modifications.
#' This function may cure some of these errors in all transcript objects of a corpus.
#' - Annotations with reversed times: annotations with \code{endSec} lower than \code{startSec} will be deleted.
#' - Overlapping annotations: earlier annotations will end where the next annotation starts.
#' - Annotations below 0 sec: Annotations that are starting and ending before 0 sec will be deleted; Annotations starting before but ending after 0 sec will be truncated.
#' - Missing tiers: Tiers that are present in the annotations but missing in the list of tiers in \code{@tiers} of the transcript object will be added.
#' 
#' @param x Corpus object.
#' @param filterTranscriptNames Vector of character strings; names of the transcripts to be included. 
#' @param annotationsWithReversedTimes Logical; If \code{TRUE} annotations with reversed times will be deleted 
#' @param annotationsWithTimesBelowZero Logical; If \code{TRUE} annotations before 0 sec will be corrected. 
#' @param overlappingAnnotations Logical; If \code{TRUE} overlapping annotations will be corrected. 
#' @param missingTiers Logical; If \code{TRUE} tiers missing in \code{@tiers} slot of the transcript object will be added. 
#' @param showWarning Logical; If \code{TRUE} a warning notice will be shown upon correction. 
#'
#' @return Corpus object; 
#' 
#' @seealso \link{transcripts_cure_single}
#' 
#' @export
#'
#' @example inst/examples/transcripts_cure.R
#' 
#' 
transcripts_cure <- function (x, 
						 filterTranscriptNames=NULL, 
						 annotationsWithReversedTimes=TRUE, 
						 overlappingAnnotations=TRUE, 
						 annotationsWithTimesBelowZero=TRUE, 
						 missingTiers=TRUE, 
						 showWarning=FALSE) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	
	annotationsWithReversedTimes.deleted.count    <-0     
	annotationsWithTimesBelowZero.deleted.count   <-0
	annotationsWithTimesBelowZero.corrected.count <-0
	overlappingAnnotations.corrected.count		  <-0
	missingTiers.added.count				      <-0
	transcripts.cured.count                       <-0
	transcripts.cured.ids                         <-c()
	
	for (i in filterTranscriptNames) {
		x@transcripts[[i]] <- act::transcripts_cure_single(x@transcripts[[i]], 
												   annotationsWithReversedTimes=annotationsWithReversedTimes, 
												   overlappingAnnotations=overlappingAnnotations, 
												   annotationsWithTimesBelowZero=annotationsWithTimesBelowZero, 
												   missingTiers=missingTiers, showWarning=showWarning)
		
		h <- x@transcripts[[i]]@history[[length(x@transcripts[[i]]@history)]]
		if (h$annotationsWithReversedTimes.deleted.count>0 | h$annotationsWithTimesBelowZero.deleted.count>0 | h$annotationsWithTimesBelowZero.corrected.count>0 | h$overlappingAnnotations.corrected.count>0 | h$missingTiers.added.count>0) {
			transcripts.cured.count                       <- transcripts.cured.count+1
			transcripts.cured.ids                         <- c(transcripts.cured.ids, i)
			annotationsWithReversedTimes.deleted.count    <- annotationsWithReversedTimes.deleted.count   + h$annotationsWithReversedTimes.deleted.count   
			annotationsWithTimesBelowZero.deleted.count   <- annotationsWithTimesBelowZero.deleted.count  + h$annotationsWithTimesBelowZero.deleted.count
			annotationsWithTimesBelowZero.corrected.count <- annotationsWithTimesBelowZero.corrected.count+ h$annotationsWithTimesBelowZero.corrected.count
			overlappingAnnotations.corrected.count		  <- overlappingAnnotations.corrected.count       + h$overlappingAnnotations.corrected.count
			missingTiers.added.count				      <- missingTiers.added.count                     + h$missingTiers.added.count
		}
	} #next transcript
	
	x@history[[length(x@history)+1]] <- list(
		modification                                  = "transcripts_cure",
		systime                                       = Sys.time(),
		transcripts.cured.count                       = transcripts.cured.count,
		transcripts.cured.ids                         = transcripts.cured.ids,
		annotationsWithReversedTimes.deleted.count    = annotationsWithReversedTimes.deleted.count,      
		annotationsWithTimesBelowZero.deleted.count   = annotationsWithTimesBelowZero.deleted.count,
		annotationsWithTimesBelowZero.corrected.count = annotationsWithTimesBelowZero.corrected.count,
		overlappingAnnotations.corrected.count		  = overlappingAnnotations.corrected.count,
		missingTiers.added.count				      = missingTiers.added.count
	)
	return (x)
}
