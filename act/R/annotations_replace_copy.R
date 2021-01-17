#' Search, replace and copy the contents of annotations
#' 
#' The function searches within the contents of annotations and replaces the search hits. 
#' In addition the search hit may be copied to another tier. 
#' In case that there is no overlapping annotation in the destination tier a new annotation will be created (based on the time values of the original annotation). In case that there is an overlapping annotation in the destination tier, the search result will be added at the end. 
#'
#' If only certain transcripts or tiers should be affected set the parameter \code{filterTranscriptNames} and \code{filterTierNames}.
#' In case that you want to select transcripts and/or tiers by using regular expressions use the function \code{act::search_meta} first.
#' 
#' @param x Corpus object.
#' @param pattern Character string; search pattern as regular expression.
#' @param replacement Character string; replacement.
#' @param destTier Character string; name of the tier to which the hit should be copied (if no copying is intended set to "").
#' @param filterTranscriptNames Vector of character strings; names of the transcripts to be included. 
#' @param filterTierNames Character string; names of the tiers to be included.
#' @param collapseString Character string; will be used to collapse multiple search results into one string.
#'
#' @return Corpus object.
#' @export
#'
#' @example inst/examples/annotations_replace_copy.R
#' 
annotations_replace_copy <- function (x, 
									  pattern, 
									  replacement=NA, 
									  destTier="", 
									  filterTranscriptNames=NULL, 
									  filterTierNames=NULL, 
									  collapseString=" | ") {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	
	transcripts_modified_nr <- 0
	transcripts_modified_ids <- c()
	
	annotations_copied_nr <- 0
	annotations_replaced_nr <- 0
	annotations_copied_total_nr <- 0
	annotations_replaced_total_nr <- 0
	recodsets_copiederror_tiermissingintranscript_nr <- 0
	recodsets_copiederror_tiermissingintranscript_ids <- c()
	#i <- 1
	#x <- corpus
	
	
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
	
	#all transcripts
	for (i in filterTranscriptNames) {
		#check if destination tier exists (in case that destTier is set)
		destTierIsPresent <- FALSE
		if (destTier!="") {
			#if destination tier is not in the tier list
			destTierIsPresent <- any(x@transcripts[[i]]@tiers$name==destTier)==TRUE
		}
		anyChanges <- FALSE
		
		#each annotation
		if (nrow(x@transcripts[[i]]@annotations)>0) {
			
			for (j in 1:nrow(x@transcripts[[i]]@annotations)) {
				
				#check if this tier is to be included
				processThisRecordset <- TRUE
				if (!is.null(filterTierNames)) {
					processThisRecordset <- x@transcripts[[i]]@annotations$tier.name[[j]] %in% filterTierNames
				}
				
				if (processThisRecordset) {
					#search and copy
					if ((is.na(destTier)!=TRUE) & (destTierIsPresent==TRUE)) {
						hits 		<- stringr::str_extract_all(x@transcripts[[i]]@annotations$content[[j]], pattern)
						hits_merged <- c()
						
						#if (length(hits) ==1) {
						#	hits_merged <- unlist(hits)[1]
						#} else if (length(hits)>1) { 
						#	hits_merged <- stringr::str_flatten(unlist(hits), collapse=collapseString)
						#}
						if (length(unlist(hits))>0) {
							hits_merged <- stringr::str_flatten(unlist(hits), collapse=collapseString)
						}
						
						#if there is a hit
						if (length(hits_merged) > 0) {
							#get record set in destination tier that possibly overlaps
							temp <- (	x@transcripts[[i]]@annotations$tier.name==destTier) & (x@transcripts[[i]]@annotations$startSec < x@transcripts[[i]]@annotations$endSec[[j]]) & (x@transcripts[[i]]@annotations$endSec > x@transcripts[[i]]@annotations$startSec[[j]])
							
							#if there is no overlapping record set on destination tier
							if (length(which(temp, TRUE))==0) {
								#create new record set
								myrow <- x@transcripts[[i]]@annotations[j,]
								myrow$tier.name     <- destTier
								myrow$content      <- hits_merged
								myrow$content.norm <- ""
								myrow$annotationID       <- max(x@transcripts[[i]]@annotations$annotationID)+1
								
								#add to table
								x@transcripts[[i]]@annotations <- rbind(x@transcripts[[i]]@annotations, myrow)
								
							} else {
								#if there is one or several overlapping record sets on the destination tiers, take the first one
								index <- which(temp, TRUE) [[1]]
								#add text in the end
								x@transcripts[[i]]@annotations$content[[index]] <- paste (x@transcripts[[i]]@annotations$content[[index]], hits_merged, collapse= collapseString)
							}
							
							annotations_copied_nr <- annotations_copied_nr + 1

							anyChanges <- TRUE
							
						} #end there are hits
					} #end copy
					
					#search and replace
					if (is.na(replacement)!=TRUE) {
						newvalue <- stringr::str_replace_all(x@transcripts[[i]]@annotations$content[[j]], pattern, replacement)
						if (newvalue!=x@transcripts[[i]]@annotations$content[[j]]) {
							#set new value
							x@transcripts[[i]]@annotations$content[[j]] <- newvalue
							
							#increase counter
							annotations_replaced_nr <- annotations_replaced_nr + 1
							

							anyChanges <- TRUE
						}
					}
				}
			} #end each annotation
		}
		
		#update info for transcript
		if (anyChanges) {
			#update history
			#if copy option is set but tier is not in the tier list
			if ((destTier!="") & (destTierIsPresent==FALSE)) {
				modificationTrans <- list(modification           ="annotations_search_replace_copy",
										  annotations.copy.error ="ERROR: destination tier for copying is not in the tier list of this transcript.")
				#remember values for corpus object
				recodsets.copiederror.tiermissingintranscript.count = recodsets_copiederror_tiermissingintranscript_nr + 1
				recodsets.copiederror.tiermissingintranscript.ids = c(recodsets_copiederror_tiermissingintranscript_ids, i)
			}
			
			x@transcripts[[i]]@modification.systime <- Sys.time()
			x@transcripts[[i]]@history[[length(x@transcripts[[i]]@history)+1]] <-	list( 
				modification               ="annotations_search_replace_copy",
				systime                    = Sys.time(),
				annotations.replaced.count =annotations_replaced_nr,
				annotations.copied.count   =annotations_copied_nr
			)
			
			#increase counter for corpus object
			transcripts_modified_nr <- transcripts_modified_nr +1
			transcripts_modified_ids=c(transcripts_modified_ids, i)
			annotations_replaced_total_nr = annotations_replaced_total_nr + annotations_replaced_nr
			annotations_copied_total_nr = annotations_copied_total_nr + annotations_copied_nr
		}
	} #next transcript
	
	#update modification in corpus file
	x@history[[length(x@history)+1]] <- list( modification                     ="annotations_search_replace_copy",
						 	   systime                          = Sys.time(),
							   pattern                          =pattern,
							   replacement                      =replacement,
							   destTier                         =destTier,
							   transcripts.modified.count       =transcripts_modified_nr,
							   transcripts.modified.ids         =transcripts_modified_ids,
						 	   annotations.replaced.total.count =annotations_replaced_total_nr,
						 	   annotations.copied.total.count   =annotations_copied_total_nr)
	
	if (recodsets_copiederror_tiermissingintranscript_nr>0) {
		x@history[[length(x@history)+1]] <-  list( 
			modification                                        ="annotations_search_replace_copy",
			systime                                             = Sys.time(),
			recodsets.copiederror                               = "ERROR: the destination tier for copying was missing in some transcripts. No data copied.",
			recodsets.copiederror.tiermissingintranscript.count =recodsets_copiederror_tiermissingintranscript_nr,
			recodsets.copiederror.tiermissingintranscript.ids   =recodsets_copiederror_tiermissingintranscript_ids
		)
		
	}
	
	return (x)
}