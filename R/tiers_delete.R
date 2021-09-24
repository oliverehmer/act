#' Delete tiers 
#' 
#' Deletes tiers in all transcript objects of a corpus.
#' If only tiers in certain transcripts should be affected set the parameter \code{filterTranscriptNames}. 
#' In case that you want to select tiers and/or transcripts by using regular expressions use the function \code{act::search_makefilter} first.
#' Results  will be reported in \code{@history} of the transcript objects.
#'
#' @param x Corpus object.
#' @param tierNames Character string; names of the tiers to be deleted.
#' @param filterTranscriptNames Vector of character strings; names of the transcripts to be modified. If left open, all transcripts will be checked. 
#'
#' @return Corpus object.
#' 
#' @seealso \link{tiers_add}, \link{tiers_rename}, \link{tiers_convert}, \link{tiers_sort}, \link{helper_tiers_new_table}, \link{helper_tiers_sort_table}
#' 
#' @export
#'
#' @example inst/examples/tiers_delete.R
#' 
tiers_delete <- function(x, 
						 tierNames, 
						 filterTranscriptNames=NULL) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	
	tiers_deleted_count_all <- 0
	tiers_deleted_names_all <- c()
	transcripts_modified_names <- c()
	annotations_deleted_count_all <- 0
	
	 # x <- corpus2
	 # tierNames<- "XXX"
	 # filterTranscriptNames=NULL

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
	
	i<- filterTranscriptNames[2]
	for (i in filterTranscriptNames) {

		#check if transcript contains some of the tiers
		tiers.ids<- which(x@transcripts[[i]]@tiers$name %in% tierNames)
		if(length(tiers.ids>0)) {
			#remember name of the transcripts
			transcripts_modified_names <- c(transcripts_modified_names, i)
			
			#delete tiers from the tier list
			tiers_deleted_names <-  x@transcripts[[i]]@tiers$name[tiers.ids]
			tiers_deleted_names_all <- unique(c(tiers_deleted_names_all, tiers_deleted_names))
			tiers_deleted_count_all <- tiers_deleted_count_all+length(tiers.ids)
			x@transcripts[[i]]@tiers <- x@transcripts[[i]]@tiers[-tiers.ids, ]
				
			#delete annotations
			annotations.ids<- which(x@transcripts[[i]]@annotations$tier.name %in% tierNames)
			x@transcripts[[i]]@annotations <- x@transcripts[[i]]@annotations[-annotations.ids, ]
			annotations_deleted_count <- length(annotations.ids)
			annotations_deleted_count_all <- annotations_deleted_count_all + annotations_deleted_count
			
			#reset transcript log
			x@transcripts[[i]]@modification.systime <- Sys.time()
			
			#history
			x@transcripts[[i]]@history[[length(x@transcripts[[i]]@history)+1]] <-	list( 
				modification        = "tiers_delete",
				systime             = Sys.time(),
				tiers.deleted.count = length(tiers_deleted_names),
				tiers.deleted.names = tiers_deleted_names,
				annotations.deleted = annotations_deleted_count
			)
		}
	}

	if (is.null(tiers_deleted_names_all)) {
		tiers_deleted_names_all <- as.character()
	}
	if (is.null(annotations_deleted_count_all)) {
		annotations_deleted_count_all <- as.character()
	}
	if (is.null(transcripts_modified_names)) {
		transcripts_modified_names <- as.character()
	}		
	
	x@history[[length(x@history)+1]] <- list(  
		modification                 ="tiers_delete",
		systime                      = Sys.time(),
		tiers.deleted.count          = length(tiers_deleted_names_all),
		tiers.deleted.names          = tiers_deleted_names_all,
		annotations.deleted          = annotations_deleted_count_all,
		transcripts.modified.count   = length(transcripts_modified_names),
		transcripts.modified.names   = transcripts_modified_names
	)
	
	return (x)
}