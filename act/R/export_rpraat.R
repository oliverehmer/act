#' Export a transcript object to a 'rPraat' TextGrid object
#' 
#' Advice: In most situations it is more convenient to use \code{act::corpus_export} for exporting annotation files.
#'
#' This function is to create compatibility with the \code{rPraat} package. 
#' It converts an act transcript to a rPraat TextGrid object.
#' 
#' Credits to Tomáš Bořil, the author of the rPraat package, for commenting on the exchange functions.
#' 
#' @param t Transcript object; transcript to be converted.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param filterSectionStartsec Double; start of selection in seconds.
#' @param filterSectionEndsec Double; end of selection in seconds.
#' 
#' @return rPraat TextGrid object
#' @export
#' 
#' @seealso \code{import_rpraat}, \code{corpus_export}, \code{export_eaf}, \code{export_exb}, \code{export_printtranscript}, \code{export_srt}, \code{export_textgrid}  
#' 
#' @example inst/examples/export_rPraat.R
#' 
export_rpraat <- function(t, 
						  filterTierNames=NULL,
						  filterSectionStartsec = NULL, 
						  filterSectionEndsec = NULL) {
	
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	} else { if (class(t)[[1]]!="transcript") 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	
	#=== Get data
	#--- Filter and cure transcript
	t <- act::transcripts_filter_single(t, filterTierNames=filterTierNames, filterSectionStartsec = filterSectionStartsec, filterSectionEndsec = filterSectionEndsec)
	t <- act::transcripts_cure_single(t, annotationsWithReversedTimes=TRUE, overlappingAnnotations=TRUE, annotationsWithTimesBelowZero=FALSE, missingTiers=TRUE, showWarning=TRUE)
	
	#--- get data from transcript
	myAnnotations <- t@annotations

	#--- get only relevant columns
	myCols <- c("tier.name", "startSec","endSec","content")
	if (!all(myCols %in% colnames(myAnnotations))) {
		stop(paste("Missing colums. Annotations need to contain: ", paste(myCols, collapse = " ", sep="")))
	}
	myAnnotations <- myAnnotations[,myCols]
	
	#--- sort annotations by start time
	myAnnotations <- myAnnotations[order(myAnnotations$startSec), ]
	
	#--- get min and max times of textgrid
	textgrid.startSec <- min(0, myAnnotations$startSec, myAnnotations$endSec)
	textgrid.endSec   <- max(c(t@length, myAnnotations$startSec, myAnnotations$endSec))

	myTG <- list()
	if (nrow(t@tiers)>0) {
		#iterate though all tierNames
		for (tierNr in 1:nrow(t@tiers)) 		{
			#get annotations within tier
			annotations.tier <- myAnnotations[myAnnotations$tier.name==t@tiers$name[tierNr],]
			if (t@tiers$type[tierNr] == "IntervalTier") {
				#get number of intervals in tier
				intervalNr <- nrow(annotations.tier)
				if (intervalNr==0) 	{
					addLevel <- function(x, newlevel = NULL) {
						if(is.factor(x)) {
							if (is.na(match(newlevel, levels(x))))
								return(factor(x, levels=c(levels(x), newlevel)))
						}
						return(x)
					}
					annotations.tier$tier.name <- addLevel(annotations.tier$tier.name, t@tiers$name[tierNr])
					
					#add an empty interval
					annotations.tier[1, ]     <- c(tier.name=t@tiers$name[tierNr], startSec=as.double(textgrid.startSec), endSec=as.double(textgrid.endSec), content="")
					annotations.tier$startSec <- as.double(annotations.tier$startSec)
					annotations.tier$endSec   <- as.double(annotations.tier$endSec)
				} else {
					#check for overlap of intervals, if there are more than one
					if (intervalNr>1) {
						#get intervals whose endSec is bigger then the startSec of the following
						overlaps <- annotations.tier$endSec[1:intervalNr-1]>annotations.tier$startSec[2:intervalNr]
						
						#if there are
						if (length(overlaps)>0) {
							if (any(overlaps==TRUE)) {
								#get the indices of those intervals
								overlaps <- c(1:length(overlaps))[overlaps]
								
								#replace endSec with startSec of the following interval
								annotations.tier$endSec[overlaps]<- annotations.tier$startSec[overlaps+1]
							}
						}
					}
					#get all times from tier, and add 0 and textgrid.endSec
					allTimes <- sort(unique(c(textgrid.startSec, textgrid.endSec, annotations.tier$startSec, annotations.tier$endSec )))
					
					#create empty intervals for all times
					newAnnotations			<- data.frame(
						tier.name=t@tiers$name[tierNr], 
						startSec=as.double(allTimes[1:length(allTimes)-1]), 
						endSec=as.double(allTimes[2:length(allTimes)]), 
						content="", 
						stringsAsFactors=FALSE		)
					
					#merge new and actual annotations
					merged <- merge(annotations.tier, newAnnotations, all.y =TRUE, by= c("tier.name","startSec", "endSec"))
					
					#set empty content to ""
					merged$content.x[is.na(merged$content.x)]<-""
					
					#remove superflous colums
					merged$content.y <- NULL
					
					annotations.tier <- merged
				}
				
				#add consecutive numbers
				annotations.tier <- cbind(as.character(1:nrow(annotations.tier)),annotations.tier)
				
				#rename columns
				colnames(annotations.tier) <- c("intervalNr", "tier.name", "startSec","endSec", "content")
				
				#get number of intervals
				intervalNr <- nrow(annotations.tier)
				
				#add tier to list
				myTier <- list(name=t@tiers$name[tierNr], type="interval", t1=annotations.tier$startSec, t2=annotations.tier$endSec, label=annotations.tier$content)
				myTG[[t@tiers$name[tierNr]]] <- myTier
			} else if (t@tiers$type[tierNr] == "TextTier") {
				#get number of points in tier
				pointnr <- nrow(annotations.tier)
				
				#add consecutive numbers
				annotations.tier <- cbind(as.character(1:nrow(annotations.tier)),annotations.tier)
				
				#add tier to list
				myTier <- list(name=t@tiers$name[tierNr], type="point", t=annotations.tier$startSec, label=annotations.tier$content)
				myTG[[t@tiers$name[tierNr]]] <- myTier
			}
		}
	}

	#set meta information
	attr(myTG, "class") <- c(	"list", tmin=as.character(textgrid.startSec), 
							            tmax=as.character(textgrid.endSec), 
							            type="TextGrid", 
							            name=t@name)
	attr(myTG, "names") <- t@tiers$name
	return(myTG)
}

