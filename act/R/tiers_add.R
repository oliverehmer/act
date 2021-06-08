#' Add tiers 
#' 
#' Adds a tiers in all transcript objects of a corpus.
#' If tiers should be added only in certain transcripts, set the parameter \code{filterTranscriptNames}. 
#' In case that you want to select transcripts by using regular expressions use the function \code{act::search_makefilter} first.
#' 
#' You can either insert the new tier at a specific position (e.g. 'absolutePosition=1') or in relation to a existing tier (e.g. destinationTier='speaker1').
#' To insert a tier at the end, leave 'absolutePosition' and 'destinationTier' open.
#' 
#' Results will be reported in \code{@history} of the transcript objects.
#'
#' @param x Corpus object.
#' @param tierName Character string; names of the tier to be added.
#' @param tierType Character string; type of the tier to be added.
#' @param absolutePosition Integer; Absolute position where the tier will be inserted. Value 1 and values beloe 1 will insert the tier in the first position; To insert the tier at the end, leave 'absolutePosition' and 'destinationTier' open.
#' @param destinationTier Character string; insert the tier relative to this tier.
#' @param relativePositionToDestinationTier Integer; position relative to the destination tier; 1=immediately after; 0 and -1=immediately before;  bigger numbers are also allowed.
#' @param insertOnlyIfDestinationExists Logical; if \code{TRUE} the new tier will only be added if the destination tier 'destinationTier' exists in the transcript object. If \code{FALSE} the new tier will only be added in any case. If the destination tier 'destinationTier' does not exist in the transcript object, the tier will be inserted at the end. 
#' @param filterTranscriptNames Vector of character strings; names of the transcripts to be modified. If left open, the tier will be added to all transcripts in the corpus. 
#' @param skipIfTierAlreadyExists Logical; if \code{TRUE} the new tier will be skipped if a tier with this name already exists in the transcript; if \code{FALSE} an error will be raised. 
#'
#' @return Corpus object.
#' 
#' @seealso \link{tiers_delete}, \link{tiers_rename}, \link{tiers_convert}, \link{tiers_sort}
#' 
#' @export
#'
#' @example inst/examples/tiers_add.R
#' 
tiers_add <- function( x,  
					   tierName,
					   tierType=c("IntervalTier", "TextTier"),
					   absolutePosition=NULL,
					   destinationTier=NULL,
					   relativePositionToDestinationTier=0,
					   insertOnlyIfDestinationExists=FALSE,
					   filterTranscriptNames=NULL,
					   skipIfTierAlreadyExists=TRUE) {
	
	if (missing(x)) 	   {stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	if (missing(tierName))  									{stop("Parameter 'tierName' is missing.") 		}
	if (!is.null(absolutePosition) & !is.null(destinationTier)) {stop("You mey define either 'absolutePosition' or 'destinationTier', not both.") 			}
	
	
	#=== get the transcript names
	#if none are given, take all names
	if (is.null(filterTranscriptNames)) {		
		filterTranscriptNames <- NULL
	} else if (length(filterTranscriptNames)==0) {
		filterTranscriptNames <- NULL
	} else if (length(filterTranscriptNames)==1) {
		if (filterTranscriptNames[1]=="") { filterTranscriptNames <- NULL }
	}
	if (is.null(filterTranscriptNames)) {	filterTranscriptNames <- names(x@transcripts)	}
	
	#---correct values
	tierName<- tierName[1]
	tierType<- tierType[1]
	if(!is.null(absolutePosition)) {
		absolutePosition<- as.integer(absolutePosition)
		if (absolutePosition<1) {absolutePosition<- 1}
	}
	if(!is.null(destinationTier)) {
		destinationTier<- as.character(destinationTier)
		relativePositionToDestinationTier<- as.integer(relativePositionToDestinationTier)
	}
	
	#=== counters
	tiers_added_count_all <- 0
	transcripts_modified_names <- c()
	alreadyExistsInTranscripts <- c()
	
	#=== run through the transcripts
	i<- filterTranscriptNames[1]
	for (i in filterTranscriptNames) {
		#print(i)
		#---get the tier table
		newTable <-	.emptyTiers
		if (!is.null(x@transcripts[[i]]@tiers)) {
			newTable <- x@transcripts[[i]]@tiers
		} 
		if (nrow(newTable)>0) {
			newRow <- newTable[1,]
			newRow[1,]<- NA
		} else {
			newRow <- .emptyTiers
			#newRow <- rbind(newRow, list(name=as.character("NA"), type=as.character("NA"), position=as.integer(NA)))
			newRow <- rbind(newRow, list(name=NA, type=NA, position=NA))
		}
		
		#--- check if tier already exists
		skipThisTranscript <- FALSE
		if(tierName %in% newTable$name) {
			alreadyExistsInTranscripts <- c(alreadyExistsInTranscripts, i)
			if (skipIfTierAlreadyExists) {
				skipThisTranscript <- TRUE
			}
		} 
		
		#--- if transcript shall not be skipped
		if(!skipThisTranscript) {
			#--- set name and type of new row
			newRow$name <- tierName
			newRow$type <- tierType
			rownames(newRow) <- tierName
			
			#--- set position
			addThisTier <- FALSE
			#- insert at the very end
			if (is.null(absolutePosition) & is.null(destinationTier)) {
				newRow$position<- max(newTable$position)+1
				addThisTier <- TRUE
			}
			
			#- insert in absolute position
			if(!is.null(absolutePosition)) {
				#get all tiers that are in this position or later
				ids<- which(newTable$position>=absolutePosition)
				#increase their position
				newTable$position[ids] <- newTable$position[ids] +1 
				
				#set  position of new row
				newRow$position <- absolutePosition
				
				addThisTier <- TRUE
			}
			
			#- insert after a certain tier
			if(!is.null(destinationTier)) {
				
				#check if transcript contains some of the tiers
				id.destination <- which(newTable$name==destinationTier)
				
				#if destination tier not found
				if (length(id.destination)==0) {
					#if existing destination tier is necessary
					if(insertOnlyIfDestinationExists) {
						
					} else {
						#if existing destination tier is not necessary
						#insert tier in the end
						newRow$position<- max(newTable$position)+1
						addThisTier <- TRUE
					}
					
				} else {
					#if destination tier is found
					
					#get position of destination tier
					pos.dest <- newTable$position[id.destination]
					
					#calculate position of new tier 
					pos.new <- pos.dest + relativePositionToDestinationTier
					newRow$position <- pos.new
					
					#calculate new positions of existing tiers
					# get all tiers that are in this position or later
					ids<- which(newTable$position>=pos.new)
					# increase their position
					newTable$position[ids] <- newTable$position[ids] +1 
					
					addThisTier <- TRUE
				}
			}
			
			#--- add this tier if conditions are met
			if (addThisTier) {
				#add the row
				newTable <- rbind(newTable, newRow)
				
				#reorder and set new positions, set row names
				if (nrow(newTable)>0) {
					newTable           <- newTable[order(newTable$position),]
					newTable$position  <- seq(1,nrow(newTable))
					rownames(newTable) <- newTable$name
				}
				
				#set to object
				x@transcripts[[i]]@tiers <- newTable
				
				#history
				x@transcripts[[i]]@modification.systime <- Sys.time()
				x@transcripts[[i]]@history[[length(x@transcripts[[i]]@history)+1]] <-	list( 
					modification        = "tiers_add",
					systime             = Sys.time(),
					tier.name           = newRow$name,
					tier.position       = newRow$position
				)
				
				#remember name of the transcripts
				transcripts_modified_names <- c(transcripts_modified_names, i)
				
				#increase the counter
				tiers_added_count_all <- tiers_added_count_all +1
			}
		}
	}
	
	
	#--- raise error?
	if(!skipIfTierAlreadyExists) {
		if (length(alreadyExistsInTranscripts)>0) {
			message <- sprintf("The tier '%s' already exists in the following transcripts: ",tierName)
			m       <- stringr::str_c("    ", alreadyExistsInTranscripts, collapse="\n")
			message <- stringr::str_c(message,"\n", m, collapse="\n")
			stop(message)
		}
	}
	
	#history
	x@history[[length(x@history)+1]] <- list(  
		modification                 ="tiers_add",
		systime                      = Sys.time(),
		tiers.added.count            = tiers_added_count_all,
		transcripts.modified.count   = length(transcripts_modified_names),
		transcripts.modified.names   = transcripts_modified_names,
		tier.already.existed.in.transcripts.count = length(alreadyExistsInTranscripts),
		tier.already.existed.in.transcripts.names = alreadyExistsInTranscripts
	)
	
	return (x)
}