#' Merge several transcripts (works with transcript objects directly)
#' 
#' Merges several transcripts. One transcript is the destination transcript (the transcript that will be updated). 
#' The other transcripts are the update transcripts and contain the updates. 
#' The update transcripts need to contain a tier in which the update sections are marked with a specific character string.
#' 
#' You may chose between the following two options:
#' - The update sections in the destination transcript will first be erased completely and then the updates will be filled in.
#' - The update sections in the destination transcript will NOT be erased completely. Rater only the contents of tiers will be erased that are also present in the update tiers. e.g. if your destination transcript contains more tiers than the update transcripts, the contents of those tiers will be preserved in the destination tier during the update.
#' 
#' @param destinationTranscript Transcript object; transcript that serves as destination (and will receive the updates).
#' @param updateTranscripts List of transcript objects; transcript objects that will be inserted into the destination transcripts (entirely or in part).
#' @param identifierTier Character string;  regular expression that identifies the tier in which the sections are marked, that will be inserted into destinationTranscript.
#' @param identifierPattern Character string; regular expression that identifies the sections that will be inserted into destinationTranscript.
#' @param eraseUpdateSectionsCompletely Logical; if \code{TRUE} update sections in destination transcript will be erased completely, if \code{FALSE} update sections in the destination tier will not be erased completely but only the tiers that are present in the updateTranscripts be erased.
#' 
#' @return Transcript object
#' 
#' @seealso \link{transcripts_merge}
#' 
#' @export
#'
#' @example inst/examples/transcripts_merge2.R
#' 
transcripts_merge2 <- function (destinationTranscript, 
							   updateTranscripts, 
							   identifierTier="update",
							   identifierPattern=".+",
							   eraseUpdateSectionsCompletely=TRUE) {
	
	# x <- examplecorpus
	# x<-corpus
	# destinationTranscript 	<- x@transcripts[['update_destination']]
	# destinationTranscript 	<- x@transcripts['update_destination']
	# updateTranscripts 		<- x@transcripts[c('update_update1', 'update_update2')]
	# updateTranscripts 		<- list(x@transcripts[['update_update1']], x@transcripts[['update_update2']])
	# updateTranscripts		<- x@transcripts[['update_update1']]
	# updateTranscripts		<- x@transcripts['update_update1']
	# identifierTier <- "update"
	# identifierPattern <- ".+"
	# eraseUpdateSectionsCompletely <- TRUE

	if (missing(destinationTranscript)) 	{stop("Transcript object in parameter 'destinationTranscript' is missing.") 	}
	if (missing(updateTranscripts)) 	{stop("Transcript object(s) in parameter 'updateTranscripts' is/are missing.") 	}
	
	#check: destinationTranscript
	if(typeof(destinationTranscript)=="list") {
		if (length(destinationTranscript)>1) {
			stop("Parameter 'destinationTranscript' may only be a single transcript object.") 
		}
		destinationTranscript <- destinationTranscript[[1]]
	}
	if (class(destinationTranscript)!="transcript") 	{
		stop("Parameter 'destinationTranscript' needs to be a transcript object.") 
	}
	#--> result should be a single Object as accessed by [[]]
	
	#check: updateTranscripts
	if(typeof(updateTranscripts)!="list") {
		updateTranscripts <- list(updateTranscripts)
	}
	allAreTransripts <- TRUE
	for (transUpdate in updateTranscripts) {
		if(class(transUpdate)!="transcript") {
			allAreTransripts <- FALSE
		}
	}
	if (!allAreTransripts) 	{
		stop("Parameter 'updateTranscripts' may only contain transcript objects.") 
	}
	#--> result should be a list
	
	#--- helper_tiers_merge_tables
	tier.table <- act::helper_tiers_merge_tables(destinationTranscript,updateTranscripts )
	
	#--- sort table with identifier tier in position 1	
	tier.table <- act::helper_tiers_sort_table(tierTable=tier.table, sortVector=unique(c(identifierTier, tier.table$name)))

	#--- tiers to object 
	destinationTranscript@tiers <- tier.table
	
	#--- get annotations of destinaton transcript
	myAnnotations <- destinationTranscript@annotations
	
	#for all transcripts in updates
	for (transUpdate in updateTranscripts) {

		#skip transcript if update is empty
		#get all annotations
		myDestIntervals <- transUpdate@annotations
		
		#if tier identifier is specified, get first tiers that fits condition
		if (!is.null(identifierTier)) {
			#get all intervals in this tier
			myDestIntervals <- myDestIntervals[stringr::str_detect(myDestIntervals$tier.name, identifierTier), ]
			
			#if a specifier in this tier is set,  select those that fit condition
			if (!is.null(identifierPattern)) {
				myDestIntervals <- myDestIntervals[stringr::str_detect(myDestIntervals$content, identifierPattern), ]
			}
		} else {
			#auto create an update interval, starting with first and ending with last interval
			startSec <- min(myDestIntervals$startSec)
			endSec <- max(myDestIntervals$endSec)
			myDestIntervals <- myDestIntervals[1:1,]
			
			myDestIntervals$startSec[1] 	<- startSec
			myDestIntervals$endSec[1] 	<- endSec
		}
		
		if(nrow(myDestIntervals)>0) {
			#run through all update intervalls
			for (i in 1:nrow(myDestIntervals)) {
				myDestInterval <- myDestIntervals[i,]
				
				#get annotations form the update transcript
				myAnnotationsUpdate <- transUpdate@annotations
				myAnnotationsUpdate <- myAnnotationsUpdate[((myAnnotationsUpdate$startSec>=myDestInterval$startSec & myAnnotationsUpdate$startSec<=myDestInterval$endSec) | (myAnnotationsUpdate$endSec>myDestInterval$startSec & myAnnotationsUpdate$endSec<=myDestInterval$endSec) | (myAnnotationsUpdate$startSec<myDestInterval$startSec & myAnnotationsUpdate$endSec>myDestInterval$endSec)), ]
				
				#---truncate intervals that do no fall entirely into the intervall
				#intervals that start before
				for (j in 1:nrow(myAnnotationsUpdate)) {
					myAnnotationsUpdate$startSec[j] <- max(myAnnotationsUpdate$startSec[j], myDestInterval$startSec)
				}
				
				#intervals that end after
				for (j in 1:nrow(myAnnotationsUpdate)) {
					myAnnotationsUpdate$endSec[j] <- min(myAnnotationsUpdate$endSec[j], myDestInterval$endSec)
				}
				
				#destination
				if (eraseUpdateSectionsCompletely==TRUE) {
					#for for all tiers
					
					#destination: get annotations outside of update region
					myAnnotations <- myAnnotations[myAnnotations$startSec<myDestInterval$startSec | myAnnotations$endSec>myDestInterval$endSec,]
					
					#truncate intervals that reach into the update area
					myAnnotations$endSec[myAnnotations$startSec<myDestInterval$startSec & myAnnotations$endSec>myDestInterval$startSec] <- myDestInterval$startSec
					
					#truncate intervals that reach out of the update area
					myAnnotations$startSec[myAnnotations$startSec<myDestInterval$endSec & myAnnotations$endSec>myDestInterval$endSec] <- myDestInterval$endSec
					
				} else {
					#for tiers that are in update transcript
					
					#get IDs of annotations in tiers that are also in the update file
					myIDs_tiers <- which(myAnnotations$tier.name %in% transUpdate@tiers$name)
					myIDS_times <- which(myAnnotations$startSec>myDestInterval$startSec & myAnnotations$endSec<myDestInterval$endSec)
					myIDs_tiers_timesection <- intersect(myIDs_tiers, myIDS_times)
					
					#revove intervals that fall entirely into the update region
					if (length(myIDs_tiers_timesection)>0) {
						#remove intervals 
						myAnnotations <- myAnnotations[-myIDs_tiers_timesection,]
					}
					#truncate intervals that reach into the update area
					myIDs_tiers <- which(myAnnotations$tier.name %in% transUpdate@tiers$name)
					myIDS_times <- which(myAnnotations$startSec<myDestInterval$startSec & myAnnotations$endSec>myDestInterval$startSec)
					myIDs_tiers_timesection <- intersect(myIDs_tiers, myIDS_times)
					myAnnotations$endSec[myIDs_tiers_timesection] <- myDestInterval$startSec
					
					#truncate intervals that reach out of the update area
					myIDs_tiers <- which(myAnnotations$tier.name %in% transUpdate@tiers$name)
					myIDS_times <- which(myAnnotations$startSec<myDestInterval$endSec & myAnnotations$endSec>myDestInterval$endSec)
					myIDs_tiers_timesection <- intersect(myIDs_tiers, myIDS_times)
					myAnnotations$startSec[myIDs_tiers_timesection] <- myDestInterval$endSec
				}
				
				#merge update and destination transcript
				myAnnotations <- rbind(myAnnotations, myAnnotationsUpdate)
				
			}
		}
	}
	#assign new annotations to transcript
	destinationTranscript@annotations <- myAnnotations
	
	#change name & path
	destinationTranscript@name <- paste(destinationTranscript@name, "_UPDATED",sep="")
	destinationTranscript@file.path <-""
	
	return(destinationTranscript)
}

