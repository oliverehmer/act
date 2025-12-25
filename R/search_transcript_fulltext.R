#' Search in full text of a single transcript
#'
#' @param t Transcript object; transcript to search in.
#' @param s Search object.
#' 
#' @return \code{Data.frame} data frame with search results.
#' 
#' @export
#'   
#' @example inst/examples/search_transcript_fulltext.R
#' 
#' 
search_transcript_fulltext <- function(t, s) {
	#progress
	helper_progress_tick()
	
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	}	else { if (!methods::is(t, "transcript")) 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		}	else { if (!methods::is(s, "search")	)	{stop("Parameter 's' needs to be a search object.") 	} }
	
	search.results 	 	    <- NULL
	search.results.byTime	<- NULL
	search.results.byTier	<- NULL
	
	
	#==== get  annotations ====
	ann <- t@annotations
	#---- only main columns from annotations ----
	names.col <- c("annotationID",
				   "tierName",
				   "startsec",
				   "endsec",
				   "content",
				   "content.norm",
				   "char.orig.bytime.start",
				   "char.orig.bytime.end",
				   "char.norm.bytime.start",
				   "char.norm.bytime.end",
				   "char.orig.bytier.start",
				   "char.orig.bytier.end",
				   "char.norm.bytier.start",
				   "char.norm.bytier.end")
	ann <- ann[,names.col]
	
	#==== helper functions ====
	#---- get the numbers of the record set where the hit starts
	getRecordsetForHit <- function(hit.length, start, end)	{
		#compare the position of the match with the cummulated positions 	= where the recordset ENDS in the big text
		# start <- ann$char.norm.bytime.start
		# end <- ann$char.norm.bytime.end
		# XXX XXX min(which(hit.length<end))
		
		#this gives an arror for hits that are length==1
		#which(hit.length>=start & hit.length<end)[1]
		which(hit.length>=start & hit.length<=end)[1]
	}
	
	#---- detect the extension of a hit, based on separators
	detectHitSpan <- function (myhit) {
		if (is.na(myhit)) 	{ return ("error")}
		if (myhit=="") 		{ return ("error")}
		
		#=== across tiers
		#check if hit contains separator
		p <- options()$act.separator_between_tiers
		if (stringr::str_detect(myhit, stringr::fixed(p))) {
			#check if separator ist not at the beginning of the hit
			results <- data.frame(stringr::str_locate_all(myhit, stringr::fixed(p)), 
								  stringsAsFactors		= FALSE)
			
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
			results <- data.frame(stringr::str_locate_all(myhit, stringr::fixed(p)), 
								  stringsAsFactors		= FALSE)
			
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
	
	#==== By TIME ====
	if (s@search.mode=="fulltext" | s@search.mode=="fulltext.byTime") {
		#---- get full text----
		if (s@search.normalized==TRUE)  {
			myFulltext <- t@fulltext.bytime.norm
		} else {
			myFulltext <- t@fulltext.bytime.orig
		}
		
		#----check if fulltext is given----
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
				#---- get the text and info of the matches----
				hit 		  		<- 	unlist(stringr::str_extract_all(myFulltext, s@pattern))
				hit.length			<- 	stringr::str_length(hit)
				hit.nr		  		<-	c(1:length(hit))
				hit.pos.fulltext	<- 	data.frame(stringr::str_locate_all(myFulltext, s@pattern), 
												stringsAsFactors		= FALSE)$start
				if (s@search.normalized==TRUE) 	{
					#calculate the interval that contains the hit
					matches.recordsetNrs <- sapply(hit.pos.fulltext, getRecordsetForHit, start = ann$char.norm.bytime.start, end = ann$char.norm.bytime.end)
					
					#select the recordsets that contain the match
					search.results.byTime 	<-	ann[matches.recordsetNrs,]

					# calculate position start of hit in content
					hit.pos.content <- hit.pos.fulltext - search.results.byTime$char.norm.bytime.start + 1
					
					#subtract length of separator
					hit.pos.content <- hit.pos.content - (search.results.byTime$char.norm.bytime.end - search.results.byTime$char.norm.bytime.start - nchar(search.results.byTime$content.norm) + 1)
				} else {
					#calculate the interval that contains the hit
					matches.recordsetNrs <- sapply(hit.pos.fulltext, getRecordsetForHit, start = ann$char.orig.bytime.start, end = ann$char.orig.bytime.end)
					
					#select the recordsets that contain the match
					search.results.byTime 	<-	ann[matches.recordsetNrs,]

					#calculate position start of hit in content
					hit.pos.content <- hit.pos.fulltext - search.results.byTime$char.orig.bytime.start + 1
					
					#subtract length of separator
					hit.pos.content <- hit.pos.content - (search.results.byTime$char.orig.bytime.end - search.results.byTime$char.orig.bytime.start - nchar(search.results.byTime$content) + 1)
				}
				
				
				#---- calculate if hit is across tiers ----
				hit.span 			<- unlist(lapply(hit, detectHitSpan))
				
				#---- add further columns----
				search.results.byTime 	<-	cbind(search.results.byTime, hit, hit.nr, hit.length, hit.pos.fulltext, hit.pos.content, searchMode="byTime", hit.span=hit.span)
				rowNumbers				<-	row.names(search.results.byTime)
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
				hit.pos.fulltext	<- 	data.frame(stringr::str_locate_all(myFulltext, s@pattern), 
												stringsAsFactors		= FALSE)$start
				if (s@search.normalized==TRUE) 	{
					#calculate the interval that contains the hit
					matches.recordsetNrs <- sapply(hit.pos.fulltext, getRecordsetForHit, start = ann$char.norm.bytier.start, end = ann$char.norm.bytier.end)
					
					#select the record sets that contain the match
					search.results.byTier 	<-	ann[matches.recordsetNrs,]

					#calculate position start of hit in content
					hit.pos.content <- hit.pos.fulltext - search.results.byTier$char.norm.bytier.start + 1
					
					#subtract length of separator
					hit.pos.content <- hit.pos.content - (search.results.byTier$char.norm.bytier.end - search.results.byTier$char.norm.bytier.start - nchar(search.results.byTier$content.norm )+ 1)
					
				} else {
					#calculate the interval that contains the hit
					matches.recordsetNrs <- sapply(hit.pos.fulltext, getRecordsetForHit, start = ann$char.orig.bytier.start, end = ann$char.orig.bytier.end)
					
					#select the recordsets that contain the match
					search.results.byTier 	<-	ann[matches.recordsetNrs,]

					#calculate position start of hit in content
					hit.pos.content <- hit.pos.fulltext - search.results.byTier$char.orig.bytier.start + 1
					
					#subtract length of separator
					hit.pos.content <- hit.pos.content - (search.results.byTier$char.orig.bytier.end - search.results.byTier$char.orig.bytier.start - nchar(search.results.byTier$content)+ 1)
				}
				
				#=== calculate if hit is across tiers
				hit.span 	<- unlist(lapply(hit, detectHitSpan))
				
				#=== add further columns
				search.results.byTier 	<-	cbind(search.results.byTier, hit, hit.nr, hit.length, hit.pos.fulltext, hit.pos.content, searchMode="byTier", hit.span=hit.span)
				rowNumbers				<-	row.names(search.results.byTier)
				
				#=== delete results that are across tiers
				search.results.byTier <- search.results.byTier[search.results.byTier$hit.span!="across tiers", ]
			}
		}
	}
	
	#=== get rid of duplicated results
	#only by time
	if (!is.null(search.results.byTime) & is.null(search.results.byTier)) {
		search.results <- search.results.byTime
		
		#only by Tier
	} else if (is.null(search.results.byTime) & !is.null(search.results.byTier)) {
		search.results <- search.results.byTier
		
		#both
	} else if (!is.null(search.results.byTime) & !is.null(search.results.byTier) ) {
		#merge the results of both searches
		search.results <- rbind(search.results.byTime, search.results.byTier)
		
		#delete double hits
		compare <- search.results[, c("annotationID", "hit.pos.content")]
		
		doubles <- duplicated(compare)
		search.results <- search.results[!doubles,]
		
	} else {
		search.results <- NULL
	}
	
	#=== filter by time
	#--- time section
	if (length(s@filter.section.startsec)!=0) {
		if (!is.na(s@filter.section.startsec)) {
			search.results <- search.results[(search.results$endsec>s@filter.section.startsec), ]
			#include: also annotations that only share the boundary
			#search.results <- search.results[(search.results$endsec>=s@filter.section.startsec), ]
		}
	}
	if (length(s@filter.section.endsec)!=0) {
		if (!is.na(s@filter.section.endsec)) {
			search.results <- search.results[(search.results$startsec<s@filter.section.endsec), ]
		}
	}
	
	if(	is.null(search.results)) {
		myColNames <- c("annotationID", "tierName", "startsec","endsec", "content", "content.norm", "char.orig.bytime.start", "char.orig.bytime.end", "char.norm.bytime.start", "char.norm.bytime.end", "char.orig.bytier.start", "char.orig.bytier.end", "char.norm.bytier.start", "char.norm.bytier.end", "hit", "hit.nr" ,"hit.length", "hit.pos.fulltext", "hit.pos.content", "searchMode", "hit.span")
		search.results <- data.frame(matrix(ncol = length(myColNames), nrow = 0), 
									  stringsAsFactors		= FALSE)
		colnames(search.results) <- myColNames	
	}
	
	#add column transcript name
	if(!is.null(search.results)){
		if (nrow(search.results)==0) {
			search.results <- cbind(transcriptName=character(0), search.results)
		} else {
			search.results <- cbind(transcriptName=rep(t@name, times=nrow(search.results)), search.results)
		}
	}
	#=== return results
	return(search.results)
}

