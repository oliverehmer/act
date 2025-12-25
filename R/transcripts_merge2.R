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
#' @param eraseCompletely Logical; if \code{TRUE} update sections in destination transcript will be erased completely, if \code{FALSE} update sections in the destination tier will not be erased completely but only the tiers that are present in the updateTranscripts be erased.
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
								identifierTier        = "update",
								identifierPattern     = ".+",
								eraseCompletely       = TRUE) {


		#destinationTranscript 	<- x@transcripts[['update_destination']]
		#destinationTranscript 	<- x@transcripts['update_destination']
		#updateTranscripts 		<- x@transcripts[c('update_update1', 'update_update2')]
		#updateTranscripts 		<- list(x@transcripts[['update_update1']], x@transcripts[['update_update2']])
		#updateTranscripts		<- x@transcripts[['update_update1']]
		#updateTranscripts		<- x@transcripts['update_update1']
		#identifierTier <- "update"
		#identifierPattern <- ".+"
		#eraseCompletely <- TRUE
		#pathOverview <- "/Users/oliverehmer/Library/CloudStorage/OneDrive-Persönlich/Corpus/corpus_work/update/out_destination_updated/tier_comparison_update_destination.xlsx"

	
	#x<-corpus
	#destinationTranscriptName <- 'destination'
	#destinationTranscript <- x@transcripts[[destinationTranscriptName]]
	
	#updateTranscriptNames <- c('A')
	#updateTranscripts <- x@transcripts[ids]
	
	#identifierTier='status-progress'
	#identifierPattern=".+"
	#eraseCompletely<-TRUE
	
	
	if (missing(destinationTranscript)) 	{stop("Transcript object in parameter 'destinationTranscript' is missing.") 	}
	if (missing(updateTranscripts)) 	{stop("Transcript object(s) in parameter 'updateTranscripts' is/are missing.") 	}
	
	#check: destinationTranscript
	if(typeof(destinationTranscript)=="list") {
		if (length(destinationTranscript)>1) {
			stop("Parameter 'destinationTranscript' may only be a single transcript object.") 
		}
		destinationTranscript <- destinationTranscript[[1]]
	}
	if (!methods::is(destinationTranscript,"transcript")) 	{
		stop("Parameter 'destinationTranscript' needs to be a transcript object.") 
	}
	#--> result should be a single Object as accessed by [[]]
	
	#check: updateTranscripts
	if(typeof(updateTranscripts)!="list") {
		updateTranscripts <- list(updateTranscripts)
	}
	allAreTransripts <- TRUE
	for (transUpdate in updateTranscripts) {
		if(!methods::is(transUpdate,"transcript")) {
			allAreTransripts <- FALSE
		}
	}
	if (!allAreTransripts) 	{
		stop("Parameter 'updateTranscripts' may only contain transcript objects.") 
	}
	#--> result should be a list
	
	#--- helper_tiers_merge_tables
	tierTable <- act::helper_tiers_merge_tables(destinationTranscript,updateTranscripts )
	
	#--- sort table with identifier tier in position 1	
	sortVector <- unique(c(identifierTier, tierTable$name))
	tierTable  <- act::helper_tiers_sort_table( tierTable = tierTable, 
												sortVector = sortVector 
	)
	
	#--- tiers to object 
	destinationTranscript@tiers <- tierTable
	
	#--- get annotations of destinaton transcript
	ann <- destinationTranscript@annotations
	
	#for all transcripts in updates
	for (transUpdate in updateTranscripts) {
		
		#skip transcript if update is empty
		#get all annotations
		myDestIntervals <- transUpdate@annotations
		
		#if tier identifier is specified, get first tiers that fits condition
		if (!is.null(identifierTier)) {
			#get all intervals in this tier
			myDestIntervals <- myDestIntervals[stringr::str_detect(myDestIntervals$tierName, identifierTier), ]
			
			#if a specifier in this tier is set,  select those that fit condition
			if (!is.null(identifierPattern)) {
				myDestIntervals <- myDestIntervals[stringr::str_detect(myDestIntervals$content, identifierPattern), ]
			}
		} else {
			#auto create an update interval, starting with first and ending with last interval
			startsec <- min(myDestIntervals$startsec)
			endsec <- max(myDestIntervals$endsec)
			myDestIntervals <- myDestIntervals[1:1,]
			
			myDestIntervals$startsec[1] 	<- startsec
			myDestIntervals$endsec[1] 	<- endsec
		}
		
		if(nrow(myDestIntervals)>0) {
			#run through all update intervalls
			for (i in 1:nrow(myDestIntervals)) {
				myDestInterval <- myDestIntervals[i,]
				
				#get annotations form the update transcript
				annUpdate <- transUpdate@annotations
				annUpdate <- annUpdate[((annUpdate$startsec>=myDestInterval$startsec & annUpdate$startsec<=myDestInterval$endsec) | (annUpdate$endsec>myDestInterval$startsec & annUpdate$endsec<=myDestInterval$endsec) | (annUpdate$startsec<myDestInterval$startsec & annUpdate$endsec>myDestInterval$endsec)), ]
				
				#---truncate intervals that do no fall entirely into the intervall
				#intervals that start before
				for (j in 1:nrow(annUpdate)) {
					annUpdate$startsec[j] <- max(annUpdate$startsec[j], myDestInterval$startsec)
				}
				
				#intervals that end after
				for (j in 1:nrow(annUpdate)) {
					annUpdate$endsec[j] <- min(annUpdate$endsec[j], myDestInterval$endsec)
				}
				
				#destination
				if (eraseCompletely==TRUE) {
					#for for all tiers
					
					#destination: get annotations outside of update region
					ann <- ann[ann$startsec<myDestInterval$startsec | ann$endsec>myDestInterval$endsec,]
					
					#truncate intervals that reach into the update area
					ann$endsec[ann$startsec<myDestInterval$startsec & ann$endsec>myDestInterval$startsec] <- myDestInterval$startsec
					
					#truncate intervals that reach out of the update area
					ann$startsec[ann$startsec<myDestInterval$endsec & ann$endsec>myDestInterval$endsec] <- myDestInterval$endsec
					
				} else {
					#for tiers that are in update transcript
					
					#get IDs of annotations in tiers that are also in the update file
					myIDs_tiers <- which(ann$tierName %in% transUpdate@tiers$name)
					myIDS_times <- which(ann$startsec>myDestInterval$startsec & ann$endsec<myDestInterval$endsec)
					myIDs_tiers_timesection <- intersect(myIDs_tiers, myIDS_times)
					
					#revove intervals that fall entirely into the update region
					if (length(myIDs_tiers_timesection)>0) {
						#remove intervals 
						ann <- ann[-myIDs_tiers_timesection,]
					}
					#truncate intervals that reach into the update area
					myIDs_tiers <- which(ann$tierName %in% transUpdate@tiers$name)
					myIDS_times <- which(ann$startsec<myDestInterval$startsec & ann$endsec>myDestInterval$startsec)
					myIDs_tiers_timesection <- intersect(myIDs_tiers, myIDS_times)
					ann$endsec[myIDs_tiers_timesection] <- myDestInterval$startsec
					
					#truncate intervals that reach out of the update area
					myIDs_tiers <- which(ann$tierName %in% transUpdate@tiers$name)
					myIDS_times <- which(ann$startsec<myDestInterval$endsec & ann$endsec>myDestInterval$endsec)
					myIDs_tiers_timesection <- intersect(myIDs_tiers, myIDS_times)
					ann$startsec[myIDs_tiers_timesection] <- myDestInterval$endsec
				}
				
				#merge update and destination transcript
				ann <- rbind(ann, annUpdate)
				
			}
		}
	}
	#assign new annotations to transcript
	destinationTranscript@annotations <- ann
	
	#change name & path
	destinationTranscript@name <- paste(destinationTranscript@name, "_UPDATED",sep="")
	destinationTranscript@file.path <-""
	
	#HISTORY transcript
	destinationTranscript@modification.systime <- Sys.time()
	destinationTranscript@history[[length(destinationTranscript@history)+1]] <-	list( 
		modification               = "transcripts_merge",
		systime                    = Sys.time(),
		destinationTranscript      = destinationTranscript@name,
		sourceTranscripts          = c(destinationTranscript, updateTranscripts)
	)
	

	return(destinationTranscript)
}



