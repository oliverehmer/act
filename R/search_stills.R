#' Search stills for a search
#' 
#' Only \code{x}, \code{s} and \code{pattern} are obligatory. 
#' The other arguments can be left to their default values.
#'
#' @param x Corpus object; basis in which will be searched.
#' @param s Search object.
#' @param pattern Character string; search pattern as regular expression.
#' @param searchMode Character string; takes the following values: \code{content}, \code{fulltext} (=default, includes both full text modes), \code{fulltext.byTime}, \code{fulltext.byTier}.
#' @param searchNormalized Logical; if \code{TRUE} function will search in the normalized content, if \code{FALSE} function will search in the original content.
#' @param filterTierNames Vector of character strings; names of tiers to be included. 
#' @param filterTierIncludeRegex Character string; as regular expression, limit search to certain tiers matching the expression.
#' @param filterTierExcludeRegex Character string; as regular expression, exclude some tiers from search matching the expression.
#' @param resultids Vector of Integer; By default all results in the search object will be processed.  if \code{resultids} is set, still will only be searched for the results specified. 
#' @param stillsFolder Character string; name of the sub folder where to store the stills. folder will be created recursively
#' 
#' 
#' @return Search object.
#' 
#' @seealso \link{search_cuts_media}, \link{search_new}
#'  
#' @export
#'
#' @example inst/examples/search_new.R
#' 
search_stills <- function (	x, 
							s,
							pattern, 
							searchMode             = c("content", "fulltext", "fulltext.byTime", "fulltext.byTier"),
							searchNormalized       = FALSE, 
							filterTierNames        = NULL,
							filterTierIncludeRegex = 'still', 
							filterTierExcludeRegex = NULL,
							resultids              = NULL,
							stillsFolder           = "stills"
							) {
	if (1==3) {
	#	x<-readRDS("/Users/oliverehmer/Downloads/x.rds")
	#	s<-readRDS("/Users/oliverehmer/Downloads/s.rds")
		
		pattern                <- ".+" 
		searchMode             <- "content"
		searchNormalized       <- FALSE
		filterTierNames        <- NULL
		filterTierIncludeRegex <- 'still'
		filterTierExcludeRegex <- NULL
		resultids              <- 1
	}
	
	if ( missing(x) )	{stop("Corpus object in parameter 'x' is missing.") }
	if ( missing(s) )	{stop("Search object in parameter 's' is missing.") }
			
	if(is.null(s)) {
		if (nrow(s@results)==0) {
			return(s)
		}
	}	
	#no search results
	if (nrow(s@results)==0) {
		return(s)
	}
	
	#no resultids specified
	if (is.null(resultids)) {
		resultids <- seq(1:nrow(s@results))
	}

	#loop through results
	act::helper_progress_set("Searching stills", length(resultids))
	i<-1
	for (i in resultids) {
		act::helper_progress_tick()
		
		s.stills <- act::search_new(
			#values that are only handed over
			x                            = x, 
			pattern                      = pattern,                #".+", #pattern, 
			searchMode                   = searchMode,             #"content", #searchMode,
			searchNormalized             = searchNormalized,       #FALSE, #searchNormalized, 
			filterTierNames              = filterTierNames,        #NULL,# filterTierNames,
			filterTierIncludeRegex       = filterTierIncludeRegex, #NULL, #filterTierIncludeRegex, 
			filterTierExcludeRegex       = filterTierExcludeRegex, #NULL, #filterTierExcludeRegex, 

			#values set by default
			concordanceMake              = FALSE,

			#values from search result
			filterTranscriptNames        = s@results$transcriptName[i],
			filterSectionStartsec        = s@results$startsec[i],
			filterSectionEndsec          = s@results$endsec[i]
		)
		#View(s.stills@results)
		#View(x@transcripts[[s@results$transcriptName[i]]]@annotations)
		
		#save stills values to main search
		if(nrow(s.stills@results) > 0) {
			still.name                  = stringr::str_trim(s.stills@results$hit)
			still.startsec              = s.stills@results$startsec
			names(still.startsec)       <- still.name
			s@results$stills.values[i]  <- list(still.startsec)
			
			#set name of destination folder
			s@results$stills.folder[i] <- 	stillsFolder[1]
		}
	}
	return(s)
}