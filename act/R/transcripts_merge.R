#' Merge several transcripts
#' 
#' Merges several transcripts. One transcript is the destination transcript (the transcript that will be updated). 
#' The other transcripts are the update transcripts and contain the updates. 
#' The update transcripts need to contain a tier in which the update sections are marked.
#' 
#' You may chose between the following two options:
#' - The update sections in the destination transcript will first be erased completely and then the updates will be filled in.
#' - The update sections in the destination transcript will NOT be erased completely. Rater only the contents of tiers will be erased that are also present in the update tiers. e.g. if your destination transcript contains more tiers than the update transcripts, the contents of those tiers will be preserved in the destination tier during the update.
#' @param trans_destination Transcript object; transcript that serves as destination (and will receive the updates).
#' @param trans_updates List of transcript objects; transcript objects that will be inserted into the destination transcripts (entirely or in part).
#' @param identifier_tier Character string;  regular expression that identifies tier (in all trans_updates) in which the sections that will be inserted into trans_destination are marked.
#' @param identifier_intervall Character string; regular expression that identifies  (in trans_updates) the sections that will be inserted into trans_destination.
#' @param erase_update_sections_completely Logical; if \code{TRUE} update sections in destination transcript will be erased completely, if \code{FALSE} update sections in the destination tier will not be erased completely but only the tiers that are present in the trans_updates be erased.
#' 
#' @return Transcript object
#' @export
#'
#' @example inst/examples/transcripts_merge.R
#' 
transcripts_merge <- function (trans_destination, 
							   trans_updates, 
							   identifier_tier="update",
							   identifier_intervall=".+",
							   erase_update_sections_completely=TRUE) {
	
	#x <-examplecorpus
	#trans_destination 	<- x@transcripts[['update_destination']]
	#trans_updates 		<- x@transcripts[c('update_update1', 'update_update2')]
	#identifier_tier <- "update"
	#identifier_intervall <- ".+"
	#erase_update_sections_completely <- TRUE
	
	#--- helper_tiers_merge_tables
	tier.table <- act::helper_tiers_merge_tables(trans_destination,trans_updates )
	
	#--- sort table with identifier tier in position 1	
	tier.table <- act::helper_tiers_sort_table(tierTable=tier.table, sortVector=unique(c(identifier_tier, tier.table$name)))

	#--- tiers to object 
	trans_destination@tiers <- tier.table
		
	#--- get annotations of destinaton transcript
	myAnnotations <- trans_destination@annotations
	
	#for all transcripts in updates
	for (trans_update in trans_updates) {

		#skip transcript if update is empty
		#get all annotations
		myDestIntervals <- trans_update@annotations
		
		#if tier identifier is specified, get first tiers that fits condition
		if (!is.null(identifier_tier)) {
			#get all intervals in this tier
			myDestIntervals <- myDestIntervals[stringr::str_detect(myDestIntervals$tier.name, identifier_tier), ]
			
			#if a specifier in this tier is set,  select those that fit condition
			if (!is.null(identifier_intervall)) {
				myDestIntervals <- myDestIntervals[stringr::str_detect(myDestIntervals$content, identifier_intervall), ]
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
				myAnnotationsUpdate <- trans_update@annotations
				myAnnotationsUpdate <- myAnnotationsUpdate[((myAnnotationsUpdate$startSec>=myDestInterval$startSec & myAnnotationsUpdate$startSec<=myDestInterval$endSec) | (myAnnotationsUpdate$endSec>myDestInterval$startSec & myAnnotationsUpdate$endSec<=myDestInterval$endSec) | (myAnnotationsUpdate$startSec<myDestInterval$startSec & myAnnotationsUpdate$endSec>myDestInterval$endSec)), ]
				
				#---truncate intervals that do no fall entirely into the intervall
				#intervals that start before
				for (j in 1:nrow(myAnnotationsUpdate)) {
					myAnnotationsUpdate$startSec[j] <- max(myAnnotationsUpdate$startSec[j], myDestInterval$startSec)
				}
				
				#intervals that end after
				for (j in 1:nrow(myAnnotationsUpdate)) {
					myAnnotationsUpdate$endSec[j] <-min(myAnnotationsUpdate$endSec[j], myDestInterval$endSec)
				}
				
				#destination
				if (erase_update_sections_completely==TRUE) {
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
					myIDs_tiers <- which(myAnnotations$tier.name %in% trans_update@tiers$name)
					myIDS_times <- which(myAnnotations$startSec>myDestInterval$startSec & myAnnotations$endSec<myDestInterval$endSec)
					myIDs_tiers_timesection <- intersect(myIDs_tiers, myIDS_times)
					
					#revove intervals that fall entirely into the update region
					if (length(myIDs_tiers_timesection)>0) {
						#remove intervals 
						myAnnotations <- myAnnotations[-myIDs_tiers_timesection,]
					}
					#truncate intervals that reach into the update area
					myIDs_tiers <- which(myAnnotations$tier.name %in% trans_update@tiers$name)
					myIDS_times <- which(myAnnotations$startSec<myDestInterval$startSec & myAnnotations$endSec>myDestInterval$startSec)
					myIDs_tiers_timesection <- intersect(myIDs_tiers, myIDS_times)
					myAnnotations$endSec[myIDs_tiers_timesection] <- myDestInterval$startSec
					
					#truncate intervals that reach out of the update area
					myIDs_tiers <- which(myAnnotations$tier.name %in% trans_update@tiers$name)
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
	trans_destination@annotations <- myAnnotations
	
	#change name & path
	trans_destination@name <- paste(trans_destination@name, "_UPDATED",sep="")
	trans_destination@file.path <-""
	
	return(trans_destination)
}

