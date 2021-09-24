#' Helper: Create a tier table
#' 
#' Creates a new tier table as necessary in \code{@tiers} of a transcript object.
#' 
#' NOTE: To actually modify the tiers in a transcript object or a corpus object corpus use the functions of the package.
#' This function is only a helper function and for people that like experiments.
#'
#' @param tierNames Vector of character strings; names of the tiers. 
#' @param tierTypes Vector of character strings; types of the tiers. Allowed values: "IntervalTier","TextTier". Needs to have the same length as 'tierNames'.
#' @param tierPositions Vector of integer values; Sort order of the tiers. Needs to have the same length as 'tierNames'.
#'
#' @return Data.frame
#' 
#' @seealso \link{helper_tiers_sort_table}, \link{helper_tiers_merge_tables}, \link{tiers_convert}, \link{tiers_rename}, \link{tiers_sort}
#' 
#' @export
#'
#' @example inst/examples/helper_tiers_new_table.R
#' 
helper_tiers_new_table <- function(tierNames, 
								   tierTypes=NULL,
								   tierPositions=NULL) {
	
	#---- check tier names
	tierNames <- as.character(tierNames)
	if (length(tierNames)!=length(unique(tierNames))) {
		stop("The values given in 'tierNames' are not unique.")
	}
	
	#--- set and check tier types
	if(is.null(tierTypes)){
		tierTypes <- rep("IntervalTier", length(tierNames))
	} else {
		if (length(tierNames)!=length(tierTypes)) {
			stop("The parameters 'tierNames' and 'tierTypes' do not have the same length.")	
		}
		if(length(setdiff(	tierTypes, 		c("IntervalTier","TextTier")))!=0) {
			stop("The parameters 'tierTypes' contains unallowed values. Only 'IntervalTier' and 'TextTier' are allowed.")	
		}
	}
	
	#--- set positions
	if(is.null(tierPositions)){
		if(length(tierNames)==0) {
			tierPositions <- integer()
		} else {
			tierPositions <- seq(1:length(tierNames))
		}
	} else {
		if (length(tierNames)!=length(tierPositions)) {
			stop("The parameters 'tierNames' and 'tierPositions' do not have the same length.")	
		}

		tierPositions <- as.integer(tierPositions)
		if (length(tierPositions)!=length(unique(tierPositions))) {
			stop("The values given in 'tierPositions' are not unique.")	
		}
	}
	
	#--- make data frame
	tiers <- data.frame( name		= as.character(tierNames), 
						 type		= as.character(tierTypes), 
						 position	= as.integer(tierPositions), 
						 stringsAsFactors= FALSE)
	
	#--- sort by position
	tiers <- tiers[order(tiers$position),]
	
	#--- set row names
	rownames(tiers) <- tiers$name
	return(tiers)
}