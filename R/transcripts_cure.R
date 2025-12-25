#' Cure all transcript objects in a corpus
#' 
#' Transcript object may contain errors, e.g. because of defect annotation input files or user modifications.
#' This function may cure some of these errors in all transcript objects of a corpus.
#' - Annotations with reversed times: annotations with \code{endsec} lower than \code{startsec} will be deleted.
#' - Overlapping annotations: earlier annotations will end where the next annotation starts.
#' - Annotations below 0 sec: Annotations that are starting and ending before 0 sec will be deleted; Annotations starting before but ending after 0 sec will be truncated.
#' - Missing tiers: Tiers that are present in the annotations but missing in the list of tiers in \code{@tiers} of the transcript object will be added.
#' 
#' @param x Corpus object.
#' @param filterTranscriptNames Vector of character strings; names of the transcripts to be included. 
#' @param annotationsTimesReversed Logical; If \code{TRUE} annotations with reversed times will be deleted 
#' @param annotationsTimesBelowZero Logical; If \code{TRUE} annotations before 0 sec will be corrected. 
#' @param annotationsOverlap Logical; If \code{TRUE} overlapping annotations will be corrected. 
#' @param tiersMissing Logical; If \code{TRUE} tiers missing in \code{@tiers} slot of the transcript object will be added. 
#' @param warning Logical; If \code{TRUE} a warning notice will be shown upon correction. 
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
						 annotationsTimesReversed=TRUE, 
						 annotationsOverlap=TRUE, 
						 annotationsTimesBelowZero=TRUE, 
						 tiersMissing=TRUE, 
						 warning=FALSE) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{stop("Parameter 'x' needs to be a corpus object.") } }
	
	annotationsTimesReversed.deleted.count    <- 0     
	annotationsTimesBelowZero.deleted.count   <- 0
	annotationsTimesBelowZero.corrected.count <- 0
	annotationsOverlap.corrected.count		  <- 0
	tiersMissing.added.count				      <- 0
	transcripts.cured.ids                         <- c()
	
	for (i in filterTranscriptNames) {
		x@transcripts[[i]] <- act::transcripts_cure_single(x@transcripts[[i]], 
												   annotationsTimesReversed=annotationsTimesReversed, 
												   annotationsOverlap=annotationsOverlap, 
												   annotationsTimesBelowZero=annotationsTimesBelowZero, 
												   tiersMissing=tiersMissing, warning=warning)
		
		#HISTORY transcript:
		# realized in transcript_cure_single
		
		h <- x@transcripts[[i]]@history[[length(x@transcripts[[i]]@history)]]
		if (h$annotationsTimesReversed.deleted.count>0 | h$annotationsTimesBelowZero.deleted.count>0 | h$annotationsTimesBelowZero.corrected.count>0 | h$annotationsOverlap.corrected.count>0 | h$tiersMissing.added.count>0) {
			transcripts.cured.ids                         <- c(transcripts.cured.ids, i)
			annotationsTimesReversed.deleted.count    <- annotationsTimesReversed.deleted.count   + h$annotationsTimesReversed.deleted.count   
			annotationsTimesBelowZero.deleted.count   <- annotationsTimesBelowZero.deleted.count  + h$annotationsTimesBelowZero.deleted.count
			annotationsTimesBelowZero.corrected.count <- annotationsTimesBelowZero.corrected.count+ h$annotationsTimesBelowZero.corrected.count
			annotationsOverlap.corrected.count		  <- annotationsOverlap.corrected.count       + h$annotationsOverlap.corrected.count
			tiersMissing.added.count				      <- tiersMissing.added.count                     + h$tiersMissing.added.count
		}
	} #next transcript
	
	#HISTORY corpus
	x@history[[length(x@history)+1]] <- list(
		modification                                  = "transcripts_cure",
		systime                                       = Sys.time(),
		transcripts.cured.count                       = length(transcripts.cured.ids),
		transcripts.cured.ids                         = transcripts.cured.ids,
		annotationsTimesReversed.deleted.count    = annotationsTimesReversed.deleted.count,      
		annotationsTimesBelowZero.deleted.count   = annotationsTimesBelowZero.deleted.count,
		annotationsTimesBelowZero.corrected.count = annotationsTimesBelowZero.corrected.count,
		annotationsOverlap.corrected.count		  = annotationsOverlap.corrected.count,
		tiersMissing.added.count				      = tiersMissing.added.count
	)
	return (x)
}
