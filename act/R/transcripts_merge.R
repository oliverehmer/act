#' Merge several transcripts
#' 
#' Merges several transcript objects in a corpus object.
#' One transcript is the destination transcript (the transcript that will be updated and receives the new data). 
#' The other transcripts are the update transcripts (they contain the data that will replace data in teh destination transcript). 
#' The update transcripts need to contain a tier in which the update sections are marked with a specific character string.
#' 
#' You may chose between the following two options:
#' - The update sections in the destination transcript will first be erased completely and then the updates will be filled in.
#' - The update sections in the destination transcript will NOT be erased completely. Rater only the contents of tiers will be erased that are also present in the update tiers. e.g. if your destination transcript contains more tiers than the update transcripts, the contents of those tiers will be preserved in the destination tier during the update.
#' 
#' @param x Corpus object;
#' @param destinationTranscriptName Character strings; name of transcript hat willl be updated. 
#' @param updateTranscriptNames Vector of character strings; names of transcripts that contain the updates.
#' @param identifierTier Character string;  regular expression that identifies the tier in which the sections are marked, that will be inserted into transDestination.
#' @param identifierPattern Character string; regular expression that identifies the sections that will be inserted into transDestination.
#' @param eraseUpdateSectionsCompletely Logical; if \code{TRUE} update sections in destination transcript will be erased completely, if \code{FALSE} update sections in the destination tier will not be erased completely but only the tiers that are present in the transUpdates be erased.
#' 
#' @return Transcript object
#'  
#' @seealso \link{transcripts_merge2}
#' 
#' @export
#'
#' @example inst/examples/transcripts_merge.R
#' 
transcripts_merge <- function (x,
							   destinationTranscriptName, 
							   updateTranscriptNames, 
							   identifierTier="update",
							   identifierPattern=".+",
							   eraseUpdateSectionsCompletely=TRUE) {
	
	#x=examplecorpus
	#act::info_summarized(x)
	#destinationTranscriptName <- 'update_destination'
	#updateTranscriptNames <- c('update_update1', 'update_update2')
	#updateTranscriptNames <- c('update_update1', 'update_update2', 'SDAF', "xxx")
	
	if (missing(x))             					{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	if (missing(destinationTranscriptName))			{stop("Name of destination transcript is missing (parameter: destinationTranscriptName")	}
	if (missing(updateTranscriptNames))			    {stop("Name(s) of update transcript(s) are missing (parameter: updateTranscriptNames")	}
	
	#=== destination
	# make sure it is (only) one destination transcript
	if (length(destinationTranscriptName)==0) {
		stop("Parameter 'destinationTranscriptName' needs to be the name of a transcript object in 'x'.")
	} else if  (length(destinationTranscriptName)>1) {
		stop("Parameter 'destinationTranscriptName' needs to contain the name of only ONE transcript object in 'x'.")
	}
	
	#try to get destination transcript
	#raise error if there is no such transcript
	destinationTranscript <- x@transcripts[[destinationTranscriptName]]
	if(length(destinationTranscript)<1) {
		stop("Transcript object specified in 'destinationTranscriptName' not found in corpus object 'x'.")
	}
	
	#===updates
	if (length(updateTranscriptNames)==0) {
		stop("Parameter 'updateTranscriptNames' needs to contain the name(s) of at leat one transcript object in 'x'.")
	}
	ids <- which(names(x@transcripts) %in% updateTranscriptNames)
	if (length(ids)==0) {
		stop("The transcript object(s) specified in 'updateTranscriptNames' were not found in transcript object in 'x'.")
	}
	if (length(ids)!=length(updateTranscriptNames)) {
		missingTranscriptNames <- setdiff(updateTranscriptNames, names(x@transcripts)[ids] )
		stop(paste("Not all transcript object(s) specified in 'updateTranscriptNames' were not found in transcript object in 'x'. Missing transcript names:", paste(missingTranscriptNames, sep=", ", collapse=", "), sep= " "))
	}
	updateTranscripts <- x@transcripts[ids]
	
	#=== get the merged trasncript
	mergedT <- act::transcripts_merge2 (destinationTranscript=destinationTranscript, 
										updateTranscripts=updateTranscripts, 
										identifierTier=identifierTier,
										identifierPattern=identifierPattern,
										eraseUpdateSectionsCompletely=eraseUpdateSectionsCompletely) 
	

	#=== delete  transcripts
	x <- act::transcripts_delete(x, c(destinationTranscriptName, updateTranscriptNames))
	

	
	#=== add destination transcript
	mergedT@name <- destinationTranscriptName
	#update modification info
	mergedT@modification.systime <- Sys.time()
	mergedT@history[[length(mergedT@history)+1]] <-	list( 
		modification               = "transcripts_merge",
		systime                    = Sys.time(),
		destinationTranscriptName  = destinationTranscriptName,
		updateTranscriptNames      = updateTranscriptNames
	)
	x <- act::transcripts_add(x, mergedT)
	
	#=== update history
	x@history[[length(x@history)+1]] <- list(  
		modification               = "transcripts_merge",
		systime                    = Sys.time(),
		destinationTranscriptName  = destinationTranscriptName,
		updateTranscriptNames      = updateTranscriptNames
	)
	#=== return x
	return(x)
	
	
	
}

