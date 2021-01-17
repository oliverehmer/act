#' Add a sub search to a prior search
#' 
#' This function starts from the results of a prior search and performs a sub search. 
#' In the sub search all results from the prior search will be checked. 
#' The sub search will check annotations in other tiers that temporally overlap with the original search result. 
#' Those annotation will be checked if they match a search pattern.
#' If so, the search hit of the sub search will be added to a new column in the original search results data frame.
#' 
#' @param x Corpus object.
#' @param s Search object.
#' @param pattern Character string; search pattern as regular expression
#' @param filterTierInclude Character string; limit search to tiers that match the regular expression
#' @param filterTierExclude Character string; limit search to tiers that match the regular expression
#' @param destinationColumn Character string; name of column where results of sub search will be stored
#' @param deleteLinesWithNoResults Logical; if \code{TRUE} search results will be deleted for which the sub search does not give any results
#' @param excludeHitsWithinSameTier Logical; if \code{TRUE} the function will not add hits from the same tier as the original search result; if \code{FALSE} hits from the same tier as the original search result will be included.
#'
#' @return Data frame.
#' 
#' @seealso \link{search_new}, \link{search_run}, \link{search_meta}
#' 
#' @export
#'
#' @example inst/examples/search_sub.R

search_sub <- function(x, 
					   s, 
					   pattern, 
					   filterTierInclude="", 
					   filterTierExclude="", 
					   destinationColumn="subsearch", 
					   deleteLinesWithNoResults=FALSE, 
					   excludeHitsWithinSameTier=TRUE) {
	
	#x <-examplecorpus
	#s <-mysearch
	#pattern <- myRegEx
	#destinationColumn <-"subsearch"
	#filterTierInclude <-""
	#filterTierExclude <-""
	#deleteLinesWithNoResults <-FALSE
	#excludeHitsWithinSameTier <-TRUE
	
	
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		} else { if (class(s)[[1]]!="search")		{stop("Parameter 's' needs to be a search object.") 	} }
	
	if (destinationColumn=="") {
		stop("Destination column name may not be empty.")
	}
	if (destinationColumn %in% colnames(s@results)) {
		#add column
		newColumnName <- destinationColumn
		for (i in 1:1000) {
			
			newColumnName <- stringr::str_flatten(c(newColumnName, as.character(i)), collapse="")
			if (!newColumnName %in% colnames(s@results)) {
				destinationColumn <- newColumnName
				break
			}
		}
	}
	
	s@results <-cbind(s@results, newCol=as.character(rep(times=nrow(s@results), "")), stringsAsFactors=FALSE)
	colnames(s@results)[ncol(s@results)] <- destinationColumn
	#i <-1
	for (i in 1:nrow(s@results)) {
		#get all info
		search.sub <- act::search_new(x=x, pattern=pattern, filterTranscriptInclude=s@results$transcript.name[i], filterTierInclude=filterTierInclude, filterTierExclude=filterTierExclude, filterSectionStartsec=s@results$startSec[i], filterSectionEndsec=s@results$endSec[i], searchMode="content", searchNormalized=FALSE, concordanceMake=FALSE)
		searchResults.sub <- search.sub@results
		
		#add information to new column
		if (length(searchResults.sub)==0) {
			s@results[i, destinationColumn] <- NA
		} else {
			# if results from the same tier should be excluded
			if (excludeHitsWithinSameTier) {
				pos <-grep(pattern=s@results$tier[i], x=searchResults.sub$tier.name)
				if (length(pos)>0) {
					searchResults.sub <- searchResults.sub[-pos, ]
				}
			}
			
			s@results[i, destinationColumn]
			s@results[i, destinationColumn] <- stringr::str_flatten(searchResults.sub$content, collapse=" ")
			
		}
	}
	
	if (deleteLinesWithNoResults==TRUE) {
		s@results <- s@results[!is.na(s@results[, destinationColumn]), ]
	}
	
	return(s@results)
}


