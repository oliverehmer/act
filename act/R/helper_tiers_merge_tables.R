#' Helper: Merge tier tables
#'
#' Merges several the tier tables into one tier table. 
#' 
#' NOTE: To actually modify the tiers in a transcript object or a corpus object corpus use the functions of the package, e.g. \code{act::transcripts_merge}.
#' This function is only a helper function and for people that like experiments.
#
#' If tiers with the same name are of different types ('IntervalTier', 'TextTier') an error will be raised.
#' In that case can use, for example,  'act::tier_convert()' to change the tier types. 
#' 
#' @param ... accepts different kinds of objects; transcript objects, lists of transcript objects (as in @transcripts of a corpus object) and tier tables (as in @tiers of a transcript object).
#'
#' @return Data.frame
#' 
#' @seealso \link{helper_tiers_sort_table}, \link{helper_tiers_merge_tables}, \link{tiers_convert}, \link{tiers_rename}, \link{tiers_sort}, \link{transcripts_merge}
#' 
#' @export
#'
#' @example inst/examples/helper_tiers_merge_tables.R
#' 
helper_tiers_merge_tables <- function (...) {
	arguments <- list(...)
	#--- merge all tier tables in all arguments to a data frame
	# accepted values: tier table, transcript, list of transcripts
	tiers.table.merged <- data.frame(stringsAsFactors = FALSE)
	for (argument in arguments) {
		if (class(argument)=="data.frame") {
			if (all(c("name", "type", "position") %in% colnames(argument))) {
				tiers.table.merged <- rbind(tiers.table.merged, argument)	
			}
		}
		if (class(argument)=="transcript") {
			tiers.table.merged <- rbind(tiers.table.merged, argument@tiers)
		}
		if (class(argument)=="list") {
			for (i in argument) {
				if (class(i)=="transcript") {
					tiers.table.merged <- rbind(tiers.table.merged, i@tiers)
				}
				if (class(i)=="data.frame") {
					if (all(c("name", "type", "position") %in% colnames(i))) {
						tiers.table.merged <- rbind(tiers.table.merged, i)	
					}
				}
			}
		}
	}
	
	#--- check if tiers with the same name are all of the same type
	differing_tier_types <-c()
	for (i in nrow(tiers.table.merged)) {
		tier1 <- tiers.table.merged[i,]
		#compare with the other updates
		for (j in nrow(tiers.table.merged)) {
			tier2 <- tiers.table.merged[j,]
			temp         <- merge(x=tier1, y=tier2 , by.x = "name", by.y ="name", all.x=FALSE, all.y = FALSE)
			#check if types of any of the tiers differ
			ids <-which(temp$type.x!=temp$type.y)
			if(length(ids) >0) {
				differing_tier_types <- c(differing_tier_types, temp$name[ids])		
			}
		}
	}
	differing_tier_types <-unique(differing_tier_types)
	if (length(differing_tier_types)>0) {
		stop(paste("Some of the tiers in the transcripts have the same name but are of different types ('IntervalTier', 'TextTier'). The tiers with differing types are: ", paste(differing_tier_types, sep=", "), sep=""))
	}

	#drop rows with non unique names
	tiers.table.merged <-tiers.table.merged[-which(duplicated(tiers.table.merged$name)),]
		if (nrow(tiers.table.merged)==0 ){
		tiers.table.merged$position <- NA
	} else {
		tiers.table.merged$position <-seq(1, nrow(tiers.table.merged))
	}
	rownames(tiers.table.merged) <- tiers.table.merged$name
	return(tiers.table.merged)
}







