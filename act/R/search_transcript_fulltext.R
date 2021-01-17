#' Search in full text of a single transcript
#'
#' @param t Transcript object; transcript to search in.
#' @param s Search object.
#' 
#' @return Data.frame; data frame with search results.
#' 
#'   
#' # @example inst/examples/search_transcript_fulltext.R
#' 
search_transcript_fulltext <- function(t, s) {
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	} else { if (class(t)[[1]]!="transcript") 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		} else { if (class(s)[[1]]!="search")		{stop("Parameter 's' needs to be a search object.") 	} }
	
	#progress
	helper_progress_tick()
	
	mySearchResults 	 	<- NULL
	mySearchResults.byTime	<- NULL
	mySearchResults.byTier	<- NULL
	
	#== helper function: get the numbers of the record set where the hit starts
	getRecordsetForHit <- function(hit.length, start, end)	{
		#compare the position of the match with the cummulated positions 	= where the recordset ENDS in the big text
		# start <- myAnnotations$char.norm.bytime.start
		# end <- myAnnotations$char.norm.bytime.end
		# XXX XXX min(which(hit.length<end))
		
		#this gives an arror for hits that are length==1
		#which(hit.length>=start & hit.length<end)[1]
		which(hit.length>=start & hit.length<=end)[1]
	}
	
	#== helper function: detect the extension of a hit, based on separators
	detectHitSpan <- function (myhit) {
		if (is.na(myhit)) 	{ return ("error")}
		if (myhit=="") 		{ return ("error")}
		
		#=== across tiers
		#check if hit contains separator
		p <- options()$act.separator_between_tiers
		if (stringr::str_detect(myhit, stringr::fixed(p))) {
			#check if separator ist not at the beginning of the hit
			results <- as.data.frame(stringr::str_locate_all(myhit, stringr::fixed(p)))
			
			#get rid of separators at the beginning
			results <- results[!(results$start==1),]
			
			#get rid of separators at the end
			results <- results[!(results$end==nchar(myhit)),]
			
			#if there is a hit left
			if (nrow(results)>0) {
				return("across tiers")
			}
		}
		
		#=== across intervals
		p <- options()$act.separator_between_intervals
		if (stringr::str_detect(myhit, stringr::fixed(p))) {
			#check if separator ist not at the beginning of the hit
			results <- as.data.frame(stringr::str_locate_all(myhit, stringr::fixed(p)))
			
			#get rid of separators at the beginning
			results <- results[!(results$start==1),]
			
			#get rid of sepearators at the end
			results <- results[!(results$end==nchar(myhit)),]
			
			#if there is a hit left
			if (nrow(results)>0) {
				return("across intervals")
			}
		}
		return("within interval")
	}
	
	#================================ By TIME
	if (s@search.mode=="fulltext" | s@search.mode=="fulltext.byTime") {
		#=== get full text
		if (s@search.normalized==TRUE)  {
			myFulltext <- t@fulltext.bytime.norm
		} else {
			myFulltext <- t@fulltext.bytime.orig
		}
		
		#=== check if fulltext is given
		continue <- TRUE
		if (length(myFulltext) == 0)  	{
			continue <- FALSE
		} else {
			if (is.na(myFulltext) == TRUE)  {
				continue <- FALSE
			}
		}
		
		if (continue) {
			#check if there are results
			if (stringr::str_detect(myFulltext, s@pattern))	{
				#=== get the text and info of the matches
				hit 		  		<- 	unlist(stringr::str_extract_all(myFulltext, s@pattern))
				hit.length			<- 	stringr::str_length(hit)
				hit.nr		  		<-	c(1:length(hit))
				hit.pos.fulltext	<- 	data.frame(stringr::str_locate_all(myFulltext, s@pattern))$start
				
				#=== get original annotations
				myAnnotations <- t@annotations
				
				if (s@search.normalized==TRUE) 	{
					#calculate the interval that contains the hit
					matches.recordsetNrs <- sapply(hit.pos.fulltext, getRecordsetForHit, start = myAnnotations$char.norm.bytime.start, end = myAnnotations$char.norm.bytime.end)
					
					#select the recordsets that contain the match
					mySearchResults.byTime 	<-	myAnnotations[matches.recordsetNrs,]
					rm(myAnnotations)
					
					# calculate position start of hit in content
					hit.pos.content <- hit.pos.fulltext - mySearchResults.byTime$char.norm.bytime.start + 1
					
					#subtract length of separator
					hit.pos.content <- hit.pos.content - (mySearchResults.byTime$char.norm.bytime.end - mySearchResults.byTime$char.norm.bytime.start - nchar(mySearchResults.byTime$content.norm) + 1)
				} else {
					#calculate the interval that contains the hit
					matches.recordsetNrs <- sapply(hit.pos.fulltext, getRecordsetForHit, start = myAnnotations$char.orig.bytime.start, end = myAnnotations$char.orig.bytime.end)
					
					#select the recordsets that contain the match
					mySearchResults.byTime 	<-	myAnnotations[matches.recordsetNrs,]
					rm(myAnnotations)
					
					#calculate position start of hit in content
					hit.pos.content <- hit.pos.fulltext - mySearchResults.byTime$char.orig.bytime.start + 1
					
					#subtract length of separator
					hit.pos.content <- hit.pos.content - (mySearchResults.byTime$char.orig.bytime.end - mySearchResults.byTime$char.orig.bytime.start - nchar(mySearchResults.byTime$content) + 1)
				}
				
				
				#=== calculate if hit is across tiers
				hit.span 			<- unlist(lapply(hit, detectHitSpan))
				
				#=== add further columns
				mySearchResults.byTime 	<-	cbind(mySearchResults.byTime, hit, hit.nr, hit.length, hit.pos.fulltext, hit.pos.content, search.mode="byTime", hit.span=hit.span)
				rowNumbers				<-	row.names(mySearchResults.byTime)
			}
		}
	}
	
	#================================ By TIER
	if (s@search.mode=="fulltext" | s@search.mode=="fulltext.byTier") {
		#=== get full text
		if (s@search.normalized==TRUE)  {
			myFulltext <- t@fulltext.bytier.norm
		} else {
			myFulltext <- t@fulltext.bytier.orig
		}
		
		#=== check if fulltext is given
		continue <- TRUE
		if (length(myFulltext) == 0)  	{
			continue <- FALSE
		} else {
			if (is.na(myFulltext) == TRUE)  {
				continue <- FALSE
			}
		}
		
		if (continue) {
			#check if there are results
			if (stringr::str_detect(myFulltext, s@pattern))	{
				#=== get the text and info of the matches
				hit 		  		<- 	unlist(stringr::str_extract_all(myFulltext, s@pattern))
				hit.length			<- 	stringr::str_length(hit)
				hit.nr		  		<-	c(1:length(hit))
				hit.pos.fulltext	<- 	data.frame(stringr::str_locate_all(myFulltext, s@pattern))$start
				
				#=== get original annotations
				myAnnotations <- t@annotations
				
				if (s@search.normalized==TRUE) 	{
					#calculate the interval that contains the hit
					matches.recordsetNrs <- sapply(hit.pos.fulltext, getRecordsetForHit, start = myAnnotations$char.norm.bytier.start, end = myAnnotations$char.norm.bytier.end)
					
					#select the record sets that contain the match
					mySearchResults.byTier 	<-	myAnnotations[matches.recordsetNrs,]
					rm(myAnnotations)
					
					#calculate position start of hit in content
					hit.pos.content <- hit.pos.fulltext - mySearchResults.byTier$char.norm.bytier.start + 1
					
					#subtract length of separator
					hit.pos.content <- hit.pos.content - (mySearchResults.byTier$char.norm.bytier.end - mySearchResults.byTier$char.norm.bytier.start - nchar(mySearchResults.byTier$content.norm )+ 1)
					
				} else {
					#calculate the interval that contains the hit
					matches.recordsetNrs <- sapply(hit.pos.fulltext, getRecordsetForHit, start = myAnnotations$char.orig.bytier.start, end = myAnnotations$char.orig.bytier.end)
					
					#select the recordsets that contain the match
					mySearchResults.byTier 	<-	myAnnotations[matches.recordsetNrs,]
					rm(myAnnotations)
					
					#calculate position start of hit in content
					hit.pos.content <- hit.pos.fulltext - mySearchResults.byTier$char.orig.bytier.start + 1
					
					#subtract length of separator
					hit.pos.content <- hit.pos.content - (mySearchResults.byTier$char.orig.bytier.end - mySearchResults.byTier$char.orig.bytier.start - nchar(mySearchResults.byTier$content)+ 1)
				}
				
				#=== calculate if hit is across tiers
				hit.span 	<- unlist(lapply(hit, detectHitSpan))
				
				#=== add further columns
				mySearchResults.byTier 	<-	cbind(mySearchResults.byTier, hit, hit.nr, hit.length, hit.pos.fulltext, hit.pos.content, search.mode="byTier", hit.span=hit.span)
				rowNumbers				<-	row.names(mySearchResults.byTier)
				
				#=== delete results that are across tiers
				mySearchResults.byTier <- mySearchResults.byTier[mySearchResults.byTier$hit.span!="across tiers", ]
			}
		}
	}
	
	#=== get rid of duplicated results
	#only by time
	if (!is.null(mySearchResults.byTime) & is.null(mySearchResults.byTier)) {
		mySearchResults <- mySearchResults.byTime
		
		#only by Tier
	} else if (is.null(mySearchResults.byTime) & !is.null(mySearchResults.byTier)) {
		mySearchResults <- mySearchResults.byTier
		
		#both
	} else if (!is.null(mySearchResults.byTime) & !is.null(mySearchResults.byTier) ) {
		#merge the results of both searches
		mySearchResults <- rbind(mySearchResults.byTime, mySearchResults.byTier)
		
		#delete double hits
		compare <- mySearchResults[, c("annotationID", "hit.pos.content")]
		
		doubles <- duplicated(compare)
		mySearchResults <- mySearchResults[!doubles,]
		
	} else {
		mySearchResults <- NULL
	}
	
	#=== filter by time
	#--- time section
	if (length(s@filter.section.startsec)!=0) {
		if (!is.na(s@filter.section.startsec)) {
			mySearchResults <- mySearchResults[(mySearchResults$endSec>=s@filter.section.startsec), ]
		}
	}
	if (length(s@filter.section.endsec)!=0) {
		if (!is.na(s@filter.section.endsec)) {
			mySearchResults <- mySearchResults[(mySearchResults$startSec<s@filter.section.endsec), ]
		}
	}
	
	if(	is.null(mySearchResults)) {
		myColNames <- c("annotationID", "tier.name", "startSec","endSec", "content", "content.norm", "char.orig.bytime.start", "char.orig.bytime.end", "char.norm.bytime.start", "char.norm.bytime.end", "char.orig.bytier.start", "char.orig.bytier.end", "char.norm.bytier.start", "char.norm.bytier.end", "hit", "hit.nr" ,"hit.length", "hit.pos.fulltext", "hit.pos.content", "search.mode", "hit.span")
		mySearchResults <- data.frame(matrix(ncol = length(myColNames), nrow = 0))
		colnames(mySearchResults) <- myColNames	
	}
	
	#add column transcript name
	if(!is.null(mySearchResults)){
		if (nrow(mySearchResults)==0) {
			mySearchResults <- cbind(transcript.name=character(0), mySearchResults)
		} else {
			mySearchResults <- cbind(transcript.name=rep(t@name, times=nrow(mySearchResults)), mySearchResults)
		}
	}
	#=== return results
	return(mySearchResults)
}

