#' Helper: Create filter for tier names
#' 
#' Creates a tier name filter based on a vector of character strings and the values 'filterTierIncludeRegEx' and 'filterTierExcludeRegEx' in a layout object. First, tiers matching 'filterTierExcludeRegEx' are removed. Then, tiers matching 'filterTierIncludeRegEx' are added back from the original tier list, ensuring they are always included regardless of the exclude filter. The original tier order is preserved.
#'
#' @param tierNames Vector of character strings; names of the tiers.
#' @param filterTierIncludeRegEx Character string; as regular expression, tiers matching the expression will always be included, even if they match the exclude expression.
#' @param filterTierExcludeRegEx Character string; as regular expression, tiers matching the expression will be excluded from the print transcript.
#'
#' @return Vector of character strings; names of tiers
#' 
#' @export
#'
#' @example inst/examples/helper_tiers_new_table.R
#' 
helper_tiers_filter_create <- function(tierNames, 
									   filterTierIncludeRegEx='',
									   filterTierExcludeRegEx='') {
	if (missing(tierNames)) {
		cli::cli_abort("The parameter {.arg tierNames} may not be missing.")
	}

	allTierNames <- tierNames

	#filter the tierNames by regular expressions: first exclude, then include back
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
	if (!is.null(filterTierIncludeRegEx)) {
		if (length(filterTierIncludeRegEx)>0) {
			if (!is.na(filterTierIncludeRegEx)) {
				if (filterTierIncludeRegEx!="") {
					tierNamesToInclude <- allTierNames[grep(pattern=filterTierIncludeRegEx, allTierNames)]
					tierNames <- allTierNames[allTierNames %in% unique(c(tierNames, tierNamesToInclude))]
				}
			}
		}
	}
	return(tierNames)
}