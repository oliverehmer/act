#' Search in original content of a single transcript
#'
#' @param t Transcript object; transcript to search in.
#' @param s Search object.
#' 
#' @return Data.frame; data frame with search results.
#'  
#' # @example inst/examples/search_transcript_content.R
#' 
#' 
search_transcript_content <- function(t, s) {
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	} else { if (class(t)[[1]]!="transcript") 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		} else { if (class(s)[[1]]!="search")		{stop("Parameter 's' needs to be a search object.") 	} }
	
	helper_progress_tick()
	
	temp <- NULL
	
	#=== get matches
	myAnnotations <- t@annotations
	
	#=== filter 
	#---tiers
	if(length(s@filter.tier.exclude)==0) {s@filter.tier.exclude <-""}
	if(length(s@filter.tier.include)==0) {s@filter.tier.include <-""} 
	
	include <- c(1:length(myAnnotations$content))
	#if any filter is set
	if (!s@filter.tier.exclude=="" | !s@filter.tier.include=="" ) 	{
		#if include filter is set
		if (s@filter.tier.include!="") {
			include <- grep(s@filter.tier.include, myAnnotations$tier.name, ignore.case =TRUE, perl = TRUE)
		}
		
		#if exclude filter is set
		if (!s@filter.tier.exclude=="") {
			exclude	<- grep(s@filter.tier.exclude, myAnnotations$tier.name, ignore.case =TRUE, perl = TRUE)
			include <- setdiff(include, exclude)
		}
		myAnnotations <- myAnnotations[include,]
	}
	
	#--- time section
	if (length(s@filter.section.startsec)!=0) {
		if (!is.na(s@filter.section.startsec)) {
			myAnnotations <- myAnnotations[(myAnnotations$endSec>=s@filter.section.startsec), ]
		}
	}
	if (length(s@filter.section.endsec)!=0) {
		if (!is.na(s@filter.section.endsec)) {
			myAnnotations <- myAnnotations[(myAnnotations$startSec<s@filter.section.endsec), ]
		}
	}
	
	if (!is.null(myAnnotations)) {
		if (s@search.normalized==TRUE) {
			if (is.na(myAnnotations$content.norm[1]))				{
				matches.df    <- NULL
			} else {
				indices 	<- stringr::str_detect(myAnnotations$content.norm, s@pattern)
				if (!any(indices)) {
					matches.df    <- NULL
				} else {
					hits.pos			<- stringr::str_locate_all(myAnnotations$content.norm[indices], s@pattern)
					hits.count 			<- stringr::str_count(myAnnotations$content.norm[indices], s@pattern)
					hits.match			<- stringr::str_extract_all(myAnnotations$content.norm[indices], s@pattern)
					annotationID 				<- myAnnotations$annotationID[indices]
					matches.df 			<- cbind(annotationID=annotationID[1], hits.pos[[1]], hit.nr=1, hit=hits.match[[1]])
					if (length(hits.pos)>1) {
						for(j in 2:length(hits.pos)) {
							matches.df <- rbind(matches.df, cbind(annotationID=annotationID[j], hits.pos[[j]], hit.nr=j, hit=hits.match[[j]]))
						}
					}
					colnames(matches.df)[2] <-"hit.pos.content"
				}
			}
		} else {
			if (is.na(myAnnotations$content[1]))				{
				matches.df    <- NULL
			} else {
				indices 	<- stringr::str_detect(myAnnotations$content, s@pattern)
				if (!any(indices)) {
					matches.df    <- NULL
				} else {
					hits.pos   			<- stringr::str_locate_all(myAnnotations$content[indices], s@pattern)
					hits.count 			<- stringr::str_count(myAnnotations$content[indices], s@pattern)
					hits.match			<- stringr::str_extract_all(myAnnotations$content[indices], s@pattern)
					annotationID 				<- myAnnotations$annotationID[indices]
					matches.df 			<- cbind(annotationID=annotationID[1], hits.pos[[1]], hit.nr=1, hit=hits.match[[1]])
					if (length(hits.pos)>1) {
						for(j in 2:length(hits.pos)) {
							matches.df <- rbind(matches.df, cbind(annotationID=annotationID[j], hits.pos[[j]], hit.nr=j, hit=hits.match[[j]]))
						}
					}
					colnames(matches.df)[2] <-"hit.pos.content"
				}
			}
		}
		
		if (!is.null(matches.df)) {
			if (nrow(matches.df)>0)	{
				#turn matrix into data frame
				sResults <- as.data.frame(matches.df)
				
				#add column with length of hit
				sResults <- cbind(sResults, hit.length=as.numeric(stringr::str_length(sResults$hit)))
				
				#add columns: hit.pos.fulltext, search.mode, hit.span
				sResults <- cbind(sResults, hit.pos.fulltext=as.numeric(NA), search.mode=as.character("byTier"), hit.span=as.character("within interval"))
				
				#turn factors into vectors
				sResults$annotationID       <-   as.numeric(sResults$annotationID) # as.numeric(levels(sResults$annotationID))[sResults$annotationID]
				sResults$hit       		   	<-   as.character(sResults$hit) #as.character(levels(sResults$hit))[sResults$hit]
				sResults$hit.pos.content	<-   as.numeric(sResults$hit.pos.content) #as.numeric(levels(sResults$hit.pos.content))[sResults$hit.pos.content]
				sResults$end				<-   as.numeric(sResults$end) #as.numeric(levels(sResults$hit.pos.content))[sResults$hit.pos.content]
				sResults$hit.nr				<-   as.numeric(sResults$hit.nr) #as.numeric(levels(sResults$hit.pos.content))[sResults$hit.pos.content]
				sResults$hit.length			<-   as.numeric(sResults$hit.length) #as.numeric(levels(sResults$hit.pos.content))[sResults$hit.pos.content]
				sResults$hit.pos.fulltext	<-   as.numeric(sResults$hit.pos.fulltext) #as.numeric(levels(sResults$hit.pos.content))[sResults$hit.pos.content]
				
				#merge results and annotations by column 
				temp         <- merge(x=myAnnotations, y=sResults , by.x = "annotationID", by.y ="annotationID", all.y = TRUE)
			}
		}
	}

	#add column transcript name
	if(!is.null (temp)) {
		if (nrow(temp)==0) {
			temp <- cbind(transcript.name=character(0), temp)
		} else {
			temp <- cbind(transcript.name=rep(t@name, times=nrow(temp)), temp)
		}		
	}
	return(temp)
}
