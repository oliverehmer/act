#' Reorder tiers in all transcripts of a corpus
#'
#' Reorder the positions of tiers in all transcripts of a corpus object. 
#' The ordering of the tiers will be done according to a vector of regular expressions defined in 'sortVector'.
#' If only certain transcripts or tiers should be affected set the parameter \code{filterTranscriptNames}.
#' In case that you want to select transcripts by using regular expressions use the function \code{act::search_meta} first.

#' @param x Corpus object.
#' @param sortVector Vector of character strings; regular expressions to match the tier names. The order within the vector presents the new order of the tiers. Use "\\*" (=two backslashes and a star) to indicate where tiers that are not present in the sort vector but in the transcript should be inserted.
#' @param filterTranscriptNames Vector of character strings; names of the transcripts to be included. 
#' @param addMissingTiers Logical; if \code{TRUE} all tiers that are given in 'the 'sortVector' but are missing in the transcripts will be added.
#' @param deleteTiersThatAreNotInTheSortVector Logical; if \code{TRUE} tiers that are not matched by the regular expressions in 'sortVector' will be deleted. Otherwise the will be inserted at the end of the table or at the position defined by '"\\*' in  'sortVector.
#'
#' @return Corpus object.
#' 
#' @seealso \link{tiers_convert}, \link{tiers_rename}, \link{helper_tiers_new_table}, \link{helper_tiers_sort_table}, 
#' 
#' @export
#'
#' @example inst/examples/tiers_sort.R
#'                            
tiers_sort <- function(x, 
					   sortVector, 
					   filterTranscriptNames=NULL,
					   addMissingTiers=FALSE, 
					   deleteTiersThatAreNotInTheSortVector=FALSE) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }

	transcripts_modified_nr <- 0
	transcripts_modified_ids <- c()
	
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
	
	#for (t in corpus@transcripts) {
	for (i in filterTranscriptNames) {
		tiers_before 	<- x@transcripts[[i]]@tiers
		tiers_after 	<- act::helper_tiers_sort_table(x@transcripts[[i]]@tiers, sortVector, addMissingTiers, deleteTiersThatAreNotInTheSortVector)
		tiers_deleted 	<- setdiff(tiers_before$name, tiers_after$name)
		tiers_added 	<- setdiff(tiers_after$name, tiers_before$name)
		
		tiers_same_orderbefore <- intersect(tiers_before$name, tiers_after$name)
		tiers_same_orderafter <- intersect(tiers_after$name, tiers_before$name)
		tiers_orderofcopiedtiershaschanged <- !(all.equal(tiers_same_orderbefore, tiers_same_orderafter)==TRUE)
		
		#check if there are  changes
		anyChanges <- FALSE
		if (nrow(tiers_before)!=nrow(tiers_after)) {
			anyChanges <- TRUE
		} else {
			if(all.equal(tiers_before$name, tiers_after$name)!=TRUE) {
				anyChanges <- TRUE
			}
		}
		
		#if there are changes
		if(anyChanges) {
			#write changes to transcipt log
			x@transcripts[[i]]@history[[length(x@transcripts[[i]]@history)+1]] <-	list( 
				modification       = "tiers_reorder",
				systime             = Sys.time(),
				tiers.orderchanged  = tiers_orderofcopiedtiershaschanged,
				tiers.deleted.count = length(tiers_deleted),
				tiers.deleted       = tiers_deleted,
				tiers.added.count   = length(tiers_added),
				tiers.added         = tiers_added,
				tiers.before        = tiers_before$name,
				tiers.after         = tiers_after$name
			)
			x@transcripts[[i]]@modification.systime <- Sys.time()
			
			#remember values for corpus object
			transcripts_modified_nr <- transcripts_modified_nr + 1
			transcripts_modified_ids <- c(transcripts_modified_ids, i)
			
			#apply changes
			#create new tier list
			x@transcripts[[i]]@tiers <- tiers_after
			
			#copy only what is contained in new list
			x@transcripts[[i]]@annotations <- x@transcripts[[i]]@annotations[ (x@transcripts[[i]]@annotations$tier.name %in% x@transcripts[[i]]@tiers$name) , ]
		}
		
	}
	x@history[[length(x@history)+1]] <- list(  
		modification               = "tiers_sort",
		systime                    = Sys.time(),
		transcripts.modified.count = transcripts_modified_nr,
		transcripts.modified.ids   = transcripts_modified_ids
		)
	
	return (x)
}

