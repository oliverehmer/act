#' Add a sub search to a prior search
#' 
#' This function starts from the results of a prior search and performs a sub search for a temporal co-occurence. 
#' In the sub search all results from the prior search will be checked. 
#' The sub search will check annotations in other tiers that temporally overlap with the original search result. 
#' Those annotation will be checked if they match a search pattern.
#' If so, the search hit of the sub search will be added to a new column in the original search results data frame.
#' 
#' @param x Corpus object.
#' @param s Search object.
#' @param pattern Character string; search pattern as regular expression
#' @param searchMode Character string; takes the following values: \code{content}, \code{fulltext} (=default, includes both full text modes), \code{fulltext.byTime}, \code{fulltext.byTier}.
#' @param searchNormalized Logical; if \code{TRUE} function will search in the normalized content, if \code{FALSE} function will search in the original content.
#' @param filterTierIncludeRegex Character string; limit search to tiers that match the regular expression
#' @param filterTierExcludeRegex Character string; limit search to tiers that match the regular expression
#' @param destinationColumn Character string; name of column where results of sub search will be stored
#' @param deleteEmptyLines Logical; if \code{TRUE} search results will be deleted for which the sub search does not give any results
#' @param excludeHitsInSameTier Logical; if \code{TRUE} the function will not add hits from the same tier as the original search result; if \code{FALSE} hits from the same tier as the original search result will be included.
#' @param collapseString Character string; Characters that will be used to separate multiple search hits
#'
#' @return Search object.
#' 
#' @seealso \link{search_new}, \link{search_run}, \link{search_makefilter}
#' 
#' @export
#'
#' @example inst/examples/search_sub.R

search_sub <- function(x, 
					   s, 
					   pattern, 
					   searchMode             = c("content", "fulltext", "fulltext.byTime", "fulltext.byTier"),
					   searchNormalized       = TRUE,
					   filterTierIncludeRegex = "", 
					   filterTierExcludeRegex = "", 
					   destinationColumn      = "subsearch", 
					   deleteEmptyLines       = FALSE, 
					   excludeHitsInSameTier  = TRUE,
					   collapseString         = " | ") {
	
#	x <- corpus
#	s <- mysearch
#	pattern <- ".+"
#	searchMode<-"content"
#	searchNormalized<-TRUE
#	destinationColumn <-"stills"
#	filterTierIncludeRegex <-"stills"
#	filterTierExcludeRegex <-""
#	deleteEmptyLines <- FALSE
#	excludeHitsInSameTier <- TRUE

	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{stop("Parameter 'x' needs to be a corpus object.") } }
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		}	else { if (!methods::is(s, "search")	)	{stop("Parameter 's' needs to be a search object.") 	} }
	
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
	
	s@results <- cbind(s@results, newCol=as.character(rep(times=nrow(s@results), "")), stringsAsFactors=FALSE)
	colnames(s@results)[ncol(s@results)] <- destinationColumn
	#View(s@results)
	#i <- 1
	for (i in 1:nrow(s@results)) {
		#get all info
		search.sub <- act::search_new(x=x, 
									  pattern                     =pattern, 
									  searchMode                  =searchMode,
									  searchNormalized            =searchNormalized,
									  filterTranscriptIncludeRegex=s@results$transcriptName[i], 
									  filterTierIncludeRegex      =filterTierIncludeRegex, 
									  filterTierExcludeRegex      =filterTierExcludeRegex, 
									  filterSectionStartsec       =s@results$startsec[i], 
									  filterSectionEndsec         =s@results$endsec[i], 
									  concordanceMake             =FALSE)
		searchResults.sub <- search.sub@results
	#View(searchResults.sub)
		
		#add information to new column
		if (length(searchResults.sub)==0) {
			s@results[i, destinationColumn] <- NA
		} else {
			# if results from the same tier should be excluded
			if (excludeHitsInSameTier) {
				pos <- grep(pattern=s@results$tier[i], x=searchResults.sub$tierName)
				if (length(pos)>0) {
					searchResults.sub <- searchResults.sub[-pos, ]
				}
			}
			
			s@results[i, destinationColumn]
			s@results[i, destinationColumn] <- stringr::str_flatten(searchResults.sub$content, collapse=collapseString)
			
		}
	}
	
	if (deleteEmptyLines==TRUE) {
		s@results <- s@results[!is.na(s@results[, destinationColumn]), ]
	}
	
	return(s)
}


