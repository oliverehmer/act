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

	.assert_corpus(x, missing = missing(x))
	.assert_search(s, missing = missing(s))

	searchMode <- match.arg(searchMode)

	if (destinationColumn=="") {
		cli::cli_abort("Destination column name may not be empty.")
	}
	if (destinationColumn %in% colnames(s@results)) {
		#add column
		newColumnName <- destinationColumn
		for (i in 1:1000) {
			newColumnName <- paste0(destinationColumn, i)
			if (!newColumnName %in% colnames(s@results)) {
				destinationColumn <- newColumnName
				break
			}
		}
	}
	
	s@results <- cbind(s@results, newCol=as.character(rep(times=nrow(s@results), "")), stringsAsFactors=FALSE)
	colnames(s@results)[ncol(s@results)] <- destinationColumn
	#View(s@results)

	if (nrow(s@results)==0) {
		return(s)
	}

	if (searchMode=="content") {
		search.all <- act::search_new(x=x,
									  pattern                     =pattern,
									  searchMode                  ="content",
									  searchNormalized            =searchNormalized,
									  filterTranscriptNames       =unique(s@results$transcriptName),
									  filterTierIncludeRegex      =filterTierIncludeRegex,
									  filterTierExcludeRegex      =filterTierExcludeRegex,
									  concordanceMake             =FALSE)
		searchResults.all <- search.all@results
		results           <- s@results
		newValues         <- character(nrow(results))
		rowsByTranscript  <- split(seq_len(nrow(searchResults.all)), searchResults.all$transcriptName)
		for (i in 1:nrow(results)) {
			rows <- rowsByTranscript[[results$transcriptName[i]]]
			if (is.null(rows)) {
				rows <- integer(0)
			}
			if (length(rows)>0 && !is.na(results$startsec[i])) {
				rows <- rows[searchResults.all$endsec[rows] > results$startsec[i]]
			}
			if (length(rows)>0 && !is.na(results$endsec[i])) {
				rows <- rows[searchResults.all$startsec[rows] < results$endsec[i]]
			}

			# if results from the same tier should be excluded
			# (the name itself, not as a pattern: "Entrevistador" also shut out "Entrevistador2")
			if (excludeHitsInSameTier && length(rows)>0) {
				rows <- rows[searchResults.all$tierName[rows] != results$tierName[i]]
			}
			# an annotation with several hits is taken once
			if (length(rows)>1 && "annotationID" %in% names(searchResults.all)) {
				rows <- rows[!duplicated(searchResults.all$annotationID[rows])]
			}

			newValues[i] <- stringr::str_flatten(searchResults.all$content[rows], collapse=collapseString)
		}
		results[[destinationColumn]] <- newValues
		s@results <- results

	} else {
		#i <- 1
		for (i in 1:nrow(s@results)) {
			#get all info
			search.sub <- act::search_new(x=x,
										  pattern                     =pattern,
										  searchMode                  =searchMode,
										  searchNormalized            =searchNormalized,
										  filterTranscriptNames       =s@results$transcriptName[i],
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
					searchResults.sub <- searchResults.sub[searchResults.sub$tierName != s@results$tierName[i], , drop=FALSE]
				}
				if (nrow(searchResults.sub)>1 && "annotationID" %in% names(searchResults.sub)) {
					searchResults.sub <- searchResults.sub[!duplicated(searchResults.sub$annotationID), , drop=FALSE]
				}

				s@results[i, destinationColumn] <- stringr::str_flatten(searchResults.sub$content, collapse=collapseString)

			}
		}
	}

	# a sub search without hits leaves "" in the column (an empty flatten), never NA
	if (deleteEmptyLines==TRUE) {
		found <- s@results[, destinationColumn]
		s@results <- s@results[!is.na(found) & found != "", , drop=FALSE]
	}
	
	return(s)
}


