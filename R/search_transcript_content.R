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
search_transcript_content <- function(t, s) {
	helper_progress_tick()
	
	if (missing(t)) 	{cli::cli_abort("Transcript object in parameter {.arg t} is missing.") 	}	else { if (!methods::is(t, "transcript")) 	{cli::cli_abort("Parameter {.arg t} needs to be a {.cls transcript} object.") 	} }
	if (missing(s)) 	{cli::cli_abort("Search object in parameter {.arg s} is missing.") 		}	else { if (!methods::is(s, "search")	)	{cli::cli_abort("Parameter {.arg s} needs to be a {.cls search} object.") 	} }
	
	temp <- NULL
	
	#==== filter ====
	#---- tiers ----
	#get all tier names from selected transcript
	tierNames.all    <- t@tiers$name
	if (is.null(tierNames.all)) {
		tierNames.all <- as.character()
	}
	#filter by tier names in search
	filterTierNames <- as.character() 
	if (!is.null(s@filter.tier.names)) {
		if (length(s@filter.tier.names)==0) {
			filterTierNames <- tierNames.all
		} else {
			filterTierNames <- intersect(tierNames.all, s@filter.tier.names)
		}	
	}
	#filter by regex in search
	filterTierNames <- helper_tiers_filter_create(tierNames              = filterTierNames,
												  filterTierIncludeRegEx = s@filter.tier.includeRegEx,
												  filterTierExcludeRegEx = s@filter.tier.excludeRegEx)
	ann <- t@annotations[t@annotations$tierName %in% filterTierNames,]
	#View(ann)
	
	#---- time section ----
	if (length(s@filter.section.startsec)!=0) {
		if (!is.na(s@filter.section.startsec)) {
			ann <- ann[(ann$endsec>s@filter.section.startsec), ]
			#include: also annotations that only share the boundary
			#ann <- ann[(ann$endsec>=s@filter.section.startsec), ]
		}
	}
	if (length(s@filter.section.endsec)!=0) {
		if (!is.na(s@filter.section.endsec)) {
			ann <- ann[(ann$startsec<s@filter.section.endsec), ]
		}
	}
	if (!is.null(ann)) {
		if (s@search.normalized==TRUE) {
			if (is.na(ann$content.norm[1]))				{
				matches.df    <- NULL
			} else {
				indices 	<- stringr::str_detect(ann$content.norm, s@pattern)
				if (!any(indices)) {
					matches.df    <- NULL
				} else {
					hits.pos			<- stringr::str_locate_all(ann$content.norm[indices], s@pattern)
					hits.count 			<- stringr::str_count(ann$content.norm[indices], s@pattern)
					hits.match			<- stringr::str_extract_all(ann$content.norm[indices], s@pattern)
					annotationID 		<- ann$annotationID[indices]
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
			if (is.na(ann$content[1]))				{
				matches.df    <- NULL
			} else {
				indices 	<- stringr::str_detect(ann$content, s@pattern)
				if (!any(indices)) {
					matches.df    <- NULL
				} else {
					hits.pos   			<- stringr::str_locate_all(ann$content[indices], s@pattern)
					hits.count 			<- stringr::str_count(ann$content[indices], s@pattern)
					hits.match			<- stringr::str_extract_all(ann$content[indices], s@pattern)
					annotationID 				<- ann$annotationID[indices]
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

		#no matches: matches.df will be NULL
		if (!is.null(matches.df)) {
			if (nrow(matches.df)>0)	{
				#turn matrix into data frame
				sResults <- data.frame(matches.df,  
									   stringsAsFactors		= FALSE)
				
				#add column with length of hit
				sResults <- cbind(sResults, hit.length=as.numeric(stringr::str_length(sResults$hit)))
				
				#add columns: hit.pos.fulltext, searchMode, hit.span
				sResults <- cbind(sResults, hit.pos.fulltext=as.numeric(NA), searchMode=as.character("byTier"), hit.span=as.character("within interval"))
				
				#turn factors into vectors
				sResults$annotationID       <-   as.numeric(sResults$annotationID)       # as.numeric(levels(sResults$annotationID))[sResults$annotationID]
				sResults$hit       		   	<-   as.character(sResults$hit)              #as.character(levels(sResults$hit))[sResults$hit]
				sResults$hit.pos.content	<-   as.numeric(sResults$hit.pos.content)    #as.numeric(levels(sResults$hit.pos.content))[sResults$hit.pos.content]
				sResults$end				<-   NULL
				sResults$hit.nr				<-   as.numeric(sResults$hit.nr)             #as.numeric(levels(sResults$hit.pos.content))[sResults$hit.pos.content]
				sResults$hit.length			<-   as.numeric(sResults$hit.length)         #as.numeric(levels(sResults$hit.pos.content))[sResults$hit.pos.content]
				sResults$hit.pos.fulltext	<-   as.numeric(sResults$hit.pos.fulltext)   #as.numeric(levels(sResults$hit.pos.content))[sResults$hit.pos.content]
				
				#merge results and annotations by column
				temp         <- merge(x=ann, y=sResults , by.x = "annotationID", by.y ="annotationID", all.y = TRUE)
			}
		}
	}
	#View(temp)
	
	#add column transcript name
	if(!is.null (temp)) {
		if (nrow(temp)==0) {
			temp <- cbind(transcriptName=character(0), temp)
		} else {
			temp <- cbind(transcriptName=rep(t@name, times=nrow(temp)), temp)
		}		
	}
	return(temp)
}
