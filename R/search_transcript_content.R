#' Search in original content of a single transcript
#'
#' @param t Transcript object; transcript to search in.
#' @param s Search object.
#' 
#' @return \code{Data.frame} data frame with search results.
#' 
#' @export
#'  
#' @example inst/examples/search_transcript_content.R
#' 
#' 
search_transcript_content <- function(t, s) {
	helper_progress_tick()
	
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	}	else { if (!methods::is(t, "transcript")) 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		}	else { if (!methods::is(s, "search")	)	{stop("Parameter 's' needs to be a search object.") 	} }
	
	temp <- NULL
	
	#=== filter 
	#--- tiers
	#get all tier names from selected transcript
	tiernames.all    <- t@tiers$name
	if (is.null(tiernames.all)) {
		tiernames.all <- as.character()
	}
	
	#filter by tier names in search
	filterTierNames <- as.character() 
	if (!is.null(s@filter.tier.names)) {
		if (length(s@filter.tier.names)==0) {
			filterTierNames <- tiernames.all
		} else {
			filterTierNames <- intersect(tiernames.all, filterTierNames)
		}	
	}
	#filter by regex in search
		if (!is.null(s@filter.tier.includeRegEx)){
			if (length(s@filter.tier.includeRegEx)!=0) {
				if (s@filter.tier.includeRegEx!="") {
					filterTierNames <- grep(pattern=s@filter.tier.includeRegEx, filterTierNames, value=TRUE)
				}
			}	
		}
	if (!is.null(s@filter.tier.excludeRegEx)){
		if (length(s@filter.tier.excludeRegEx)!=0) {
			if (s@filter.tier.excludeRegEx!="") {
				#search for tier to exclude
				i <- grep(pattern=s@filter.tier.excludeRegEx, filterTierNames)
				if (length(i)>0) {
					filterTierNames <- filterTierNames[-i]
				}
			}
		}
	}
	myAnnotations <- t@annotations[t@annotations$tier.name %in% filterTierNames,]
	#View(myAnnotations)

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
					annotationID 		<- myAnnotations$annotationID[indices]
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
				sResults <- data.frame(matches.df,  
										  stringsAsFactors		= FALSE)
				
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
	#View(temp)

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
