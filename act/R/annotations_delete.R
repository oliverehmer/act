#' Delete annotations
#' 
#' Delete annotations in a corpus object.
#' If only certain transcripts or tiers should be affected set the parameter \code{filterTranscriptNames} and \code{filterTierNames}.
#' In case that you want to select transcripts and/or tiers by using regular expressions use the function \code{act::search_meta} first.
#'
#' @param x Corpus object.
#' @param filterContent Character string; regular expression; all annotations that match this expression will be deleted.
#' @param filterTranscriptNames Vector of character strings; names of the transcripts to be included. 
#' @param filterTierNames Character string; names of the tiers to be included.
#'
#' @return Corpus object.
#' @export
#'
#' @example inst/examples/annotations_delete.R

annotations_delete <- function (x, 
								filterContent="", 
								filterTranscriptNames=NULL, 
								filterTierNames=NULL) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	
	transcripts_modified_nr <- 0
	transcripts_modified_ids <- c()
	annotations_deleted_total_nr <- 0
	
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
	
	for (i in filterTranscriptNames) {
		annotations_deleted_ids <- c()
		#each  annotation
		if (nrow(x@transcripts[[i]]@annotations)>0) {
			
			for (j in 1:nrow(x@transcripts[[i]]@annotations)) {
				
				#check if this tier is to be included
				processThisRecordset <- TRUE
				if (!is.null(filterTierNames)) {
					processThisRecordset <- x@transcripts[[i]]@annotations$tier.name[[j]] %in% filterTierNames
				}
				
				if (processThisRecordset) {
					if(filterContent=="") {
						annotations_deleted_ids <- c(annotations_deleted_ids,j)
					} else {
						if (stringr::str_detect(string=x@transcripts[[i]]@annotations$content[[j]], pattern=filterContent)) {
							annotations_deleted_ids <- c(annotations_deleted_ids,j)
						}
					}
				}
			} #end each annotation
		}
		
		if (length(annotations_deleted_ids)>0) {
			x@transcripts[[i]]@annotations <- x@transcripts[[i]]@annotations[-annotations_deleted_ids, ]
			
			#update modification info
			x@transcripts[[i]]@modification.systime <- Sys.time()
			x@transcripts[[i]]@history[[length(x@transcripts[[i]]@history)+1]] <-	list( 
				modification             = "annotations_delete",
				systime                   = Sys.time(),
				filterContent             = filterContent,
				annotations.deleted.count = length(annotations_deleted_ids)
			)
			
			#increase counter for corpus object
			transcripts_modified_nr <- transcripts_modified_nr +1
			transcripts_modified_ids=c(transcripts_modified_ids, i)
			annotations_deleted_total_nr = annotations_deleted_total_nr + length(annotations_deleted_ids)
		}
	} #next transcript
	
	#update history
	x@history[[length(x@history)+1]] <-  list(
		modification                    ="annotations_delete",
		systime                         = Sys.time(),
		filterContent                   =filterContent,
		transcripts.modified.count      =transcripts_modified_nr,
		transcripts.modified.ids        =transcripts_modified_ids,
		annotations.deleted.total.count =annotations_deleted_total_nr
	)
	
	return (x)
}