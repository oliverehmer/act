#' Helper: Sort a tier table 
#'
#' NOTE: To actually reorder the tiers in a transcript object or a corpus object corpus use \code{act::tiers_sort}.
#' This function is only a helper function and for people that like experiments.
#' 
#' Sort a tier table by a predefined vector of regular expression strings.
#' Tiers that are missing in the table but are present in the sort vector may be inserted.
#' Tiers that are present in the table but not in the sort vector may be deleted or inserted. These tiers will be inserted by default at the end of the table. You may also use a element '\\*' in  'sortVector' to define the position where they should be placed..
#' 
#' @param tierTable Data frame; tiers as specified and necessary in \code{@tiers} of a transcript object.
#' @param sortVector Vector of character strings; regular expressions to match the tier names. The order within the vector presents the new order of the tiers. Use "\\*" (=two backslashes and a star) to indicate where tiers that are not present in the sort vector but in the transcript should be inserted.
#' @param addMissingTiers Logical; if \code{TRUE} all tiers that are given in 'the 'sortVector' but are missing in 'tierTable' will be added.
#' @param deleteTiersThatAreNotInTheSortVector Logical; if \code{TRUE} tiers that are not matched by the regular expressions in 'sortVector' will be deleted. Otherwise the will be inserted at the end of the table or at the position defined by '"\\*' in  'sortVector.
#'
#' @return Data.frame
#' 
#' @seealso \link{tiers_sort}, \link{helper_tiers_new_table}, \link{helper_tiers_merge_tables}
#' 
#' @export
#'
#' @example inst/examples/helper_tiers_sort_table.R
#' 
helper_tiers_sort_table <- function (tierTable, 
									 sortVector, 
									 addMissingTiers=TRUE, 
									 deleteTiersThatAreNotInTheSortVector=FALSE) {
	
	#set old and new data frame
	oldTable <- tierTable[order(tierTable$position),]
	oldTable$counter <- NA
	
	newTable <- tierTable[0,]
	if (nrow(oldTable)>0) {
		newRow <- oldTable[1,]
		newRow[1,]<- NA
	} else {
		newRow <- .emptyTiers
		#newRow <- rbind(newRow, list(name=as.character("NA"), type=as.character("NA"), position=as.integer(NA)))
		newRow <- rbind(newRow, list(name=NA, type=NA, position=NA))
	}
	
	#check each pattern in the sort vector
	insertPosition <- -1
	counter <- 0
	sortVector <- as.character(sortVector) #needs to be a as.character and not a factor
	for (myPattern in sortVector) {
		#increase the counter
		counter <- counter+1
		
		#if this is the insert position, remember the counter and increase greatly
		if (myPattern=="\\*" |myPattern=="*") {
			insertPosition <- counter+1
			counter <- counter+100000
		} else {
			#are there hits for this pattern?
			hits <- stringr::str_which(oldTable$name, myPattern)
			
			#if there are
			if (length(hits) >0) {
				#set new position of tier in counter 
				oldTable$counter[hits] <- seq(counter,counter+(length(hits)-1))
				
				#add items to new list
				newTable <- rbind(newTable, oldTable[hits,])
				
				#delete items from old list
				oldTable <- oldTable[-hits, ]
			} else {
				#if missing tiers should be added
				if (addMissingTiers) {
					newRow$name <- as.character(myPattern)
					newRow$type <- as.character("IntervalTier")
					newRow$counter <- as.integer(counter)
					newTable <- rbind(newTable, newRow)
				}
			}
		}
	}
	
	#if there are still elements in the old list, insert those too
	if (deleteTiersThatAreNotInTheSortVector==FALSE) {
		if (nrow(oldTable)>0) {
			#--- set new positions in counter
			
			#if no insert position has been found, insert at the end
			if (insertPosition==-1) {
				insertPosition <- counter+1
			}
			oldTable$counter <- seq(insertPosition, (insertPosition+nrow(oldTable)-1))
			
			newTable <-	rbind(newTable, oldTable)
		}
	}
	
	#--- reorder and set new positions, set row names
	if (nrow(newTable)>0) {
		newTable <- newTable[order(newTable$counter),]
		newTable$position <- seq(1,nrow(newTable))
		rownames(newTable) <- newTable$name
	}
	
	#--- remove column "counter"
	newTable <- newTable[ ,!colnames(newTable)=="counter"]

	return(newTable)
}
