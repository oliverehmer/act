#' Convert tiers 
#' 
#' Converts tier types between 'interval' and 'point' tier.  
#' Applies to all tiers in all transcript objects of a corpus.
#' If only certain transcripts or tiers should be affected set the parameter \code{filterTranscriptNames}. 
#' In case that you want to select transcripts by using regular expressions use the function \code{act::search_meta} first.
#' 
#' Note: When converting from interval > point tier, the original end times of the annotations will be lost definitely.
#' 
#' @param x Corpus object.
#' @param intervalToPoint Logical; if \code{TRUE} interval tiers will be converted to point/text tiers.
#' @param pointToInterval Logical; if \code{TRUE} point/text tiers will be converted to interval tiers.
#' @param filterTierNames Vector of character strings; names of the tiers to be included. 
#' @param filterTranscriptNames Vector of character strings; names of the transcripts to be checked. If left open, all transcripts will be checked 
#'
#' @return Corpus object.
#' 
#' @seealso \link{tiers_add}, \link{tiers_delete}, \link{tiers_rename}, \link{tiers_sort}, \link{helper_tiers_new_table}, \link{helper_tiers_sort_table}
#' 
#' @export
#'
#' @example inst/examples/tiers_convert.R
tiers_convert <- function(x, 
						  intervalToPoint=FALSE, 
						  pointToInterval=FALSE, 
						  filterTierNames=NULL,
						  filterTranscriptNames=NULL
						  ) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	
	#=== get the transcript names
	#if none are given, take all names
	if (is.null(filterTranscriptNames)) {		
		filterTranscriptNames <- NULL
	} else if (length(filterTranscriptNames)==0) {
		filterTranscriptNames <- NULL
	} else if (length(filterTranscriptNames)==1) {
		if (filterTranscriptNames[1]=="") { filterTranscriptNames <- NULL }
	}
	if (is.null(filterTranscriptNames)) {	filterTranscriptNames <- names(x@transcripts)	}
	
	tiers_converted_all <- c()
	transcripts_modified_ids <- c()
	for (i in filterTranscriptNames) {
		#reset transcript log
		x@transcripts[[i]]@modification.systime <- character()
		
		#get the tier names that should be affected
		filterTierNamesCurrent <- x@transcripts[[i]]@tiers$name
		if (!is.null(filterTierNames)){
			filterTierNamesCurrent <- intersect(filterTierNamesCurrent, filterTierNames)
		}
		tiers_converted_transcript <- c()
		if (length(filterTierNamesCurrent)>0) {
			for (j in 1:length(filterTierNamesCurrent)) {
				TierAlreadyConverted <- FALSE
				if (intervalToPoint & x@transcripts[[i]]@tiers$type[j]=='IntervalTier') {
					#--change type in list
					x@transcripts[[i]]@tiers$type[j]<-'TextTier'
					
					#--modify times
					ids <- which(x@transcripts[[i]]@annotations$tier.name==j)
					if (length(ids)>0) {
						x@transcripts[[i]]@annotations$endSec[ids]<- x@transcripts[[i]]@annotations$startSec[ids]
					}
					tiers_converted_transcript <- c(tiers_converted_transcript,j)
					transcripts_modified_ids <- c(transcripts_modified_ids, i)
					TierAlreadyConverted <- TRUE
				}
				
				if (pointToInterval & !TierAlreadyConverted & x@transcripts[[i]]@tiers$type[j]=='TextTier' ) {
					#--change type in list
					x@transcripts[[i]]@tiers$type[j]<-'IntervalTier'
					
					#--modify times
					#get all end times
					ids <- which(x@transcripts[[i]]@annotations$tier.name==j)
					if (length(ids)>0) {
						newTimes <- c(x@transcripts[[i]]@annotations$endSec[ids], x@transcripts[[i]]@length)
						newTimes <- newTimes[2:length(newTimes)]
						x@transcripts[[i]]@annotations$endSec[ids]<- newTimes
					}
					tiers_converted_transcript <- c(tiers_converted_transcript,j)
					transcripts_modified_ids <- c(transcripts_modified_ids, i)
				}
			}
		}
		#report
		x@transcripts[[i]]@history[[length(x@transcripts[[i]]@history)+1]] <-	list( 
			modification         ="tiers_convert",
			systime               = Sys.time(),
			result                =paste("OK:", length(tiers_converted_transcript), "tier(s) converted"),
			tiers.converted.ids   =tiers_converted_transcript,
			tiers.converted.count =length(tiers_converted_transcript)
		)

		x@transcripts[[i]]@modification.systime <- Sys.time()
		tiers_converted_all <- c(tiers_converted_all, tiers_converted_transcript)
	}
	
	#report
	x@history[[length(x@history)+1]] <- list(  
		modification               = "tiers_convert",
		systime                    = Sys.time(),
		tiers.converted.count      = length(tiers_converted_all),
		transcripts.modified.count = length(transcripts_modified_ids),
		transcripts.modified.ids   = transcripts_modified_ids)
	
	
	return (x)
}