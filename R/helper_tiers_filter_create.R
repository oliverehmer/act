#' Helper: Create filter for tier names
#' 
#' Creates a tier name filter based on a vector of character strings and the values 'filterTierIncludeRegEx' and 'filterTierExcludeRegEx' in a layout object.
#' 
#' @param tierNames Vector of character strings; names of the tiers. 
#' @param filterTierIncludeRegEx Character string; as regular expression, tiers matching the expression will be included in the print transcript.
#' @param filterTierExcludeRegEx Character string; as regular expression, tiers matching the expression will be excluded from the print transcript.
#'
#' @return Vector of character strings; names of tiers
#' 
#' @export
#'
#' @example inst/examples/helper_tiers_new_table.R
#' 
#' 
#' 

helper_tiers_filter_create <- function(tierNames, 
									   filterTierIncludeRegEx='',
									   filterTierExcludeRegEx='') {
	if (missing(tierNames)) {
		stop("The parameter 'tierNames' may not be missing.")
	}

	#filter the tierNames by regular expressions
	if (!is.null(filterTierIncludeRegEx)) {
		if (length(filterTierIncludeRegEx)>0) {
			if (!is.na(filterTierIncludeRegEx)) {
				if (filterTierIncludeRegEx!="") {
					#	tierNames <- grep(pattern=filterTierIncludeRegEx, tierNames, value=TRUE)
					tierNames <- tierNames[grep(pattern=filterTierIncludeRegEx, tierNames)]
				}
			}
		}
	}
	if (!is.null(filterTierExcludeRegEx)) {
		if (length(filterTierExcludeRegEx)>0) {
			if (!is.na(filterTierExcludeRegEx)) {
				if (filterTierExcludeRegEx!="") {
					IDs <- grep(pattern=filterTierExcludeRegEx, tierNames)
					if (length(IDs)>0) {
						tierNames <- tierNames[-IDs]
					}
				}
			}
		}
	}
	return(tierNames)
}