#' Export 'rPraat' TextGrid object
#' 
#' Advice: In most situations it is more convenient to use \code{act::corpus_export} for exporting annotation files.
#'
#' This function is to create compatibility with the \code{rPraat} package. 
#' It converts an act transcript to a rPraat TextGrid object.
#' 
#' Credits: Thanks to Tomáš Bořil, the author of the rPraat package, for commenting on the exchange functions.
#' 
#' @param t Transcript object; transcript to be converted.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param filterSectionStartsec Double; start of selection in seconds.
#' @param filterSectionEndsec Double; end of selection in seconds.
#' 
#' @return rPraat TextGrid object
#' @export
#' 
#' @seealso \link{import_rpraat}, \link{corpus_export}, \link{export_eaf}, \link{export_exb}, \link{export_txt}, \link{export_docx}, \link{export_srt}, \link{export_textgrid}  
#' 
#' @example inst/examples/export_rPraat.R
#' 
export_rpraat <- function(t, 
						  filterTierNames=NULL,
						  filterSectionStartsec = NULL, 
						  filterSectionEndsec = NULL) {
	
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	}	else { if (!methods::is(t, "transcript")) 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	
	#=== Get data
	#--- Filter and cure transcript
	t <- act::transcripts_filter_single(t, filterTierNames=filterTierNames, filterSectionStartsec = filterSectionStartsec, filterSectionEndsec = filterSectionEndsec)
	t <- act::transcripts_cure_single(t, annotationsTimesReversed=TRUE, annotationsOverlap=TRUE, annotationsTimesBelowZero=FALSE, tiersMissing=TRUE, warning=TRUE)
	
	#--- get data from transcript
	ann <- t@annotations

	#--- get only relevant columns
	myCols <- c("tierName", "startsec","endsec","content")
	if (!all(myCols %in% colnames(ann))) {
		stop(paste("Missing colums. Annotations need to contain: ", paste(myCols, collapse = " ", sep="")))
	}
	ann <- ann[,myCols]
	
	#--- sort annotations by start time
	ann <- ann[order(ann$startsec), ]
	
	#--- get min and max times of textgrid
	textgrid.startsec <- min(0, ann$startsec, ann$endsec)
	textgrid.endsec   <- max(c(t@length.sec, ann$startsec, ann$endsec))

	myTG <- list()
	if (nrow(t@tiers)>0) {
		#iterate though all tierNames
		for (tierNr in 1:nrow(t@tiers)) 		{
			#get annotations within tier
			annotations.tier <- ann[ann$tierName==t@tiers$name[tierNr],]
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
					annotations.tier$tierName <- addLevel(annotations.tier$tierName, t@tiers$name[tierNr])
					
					#add an empty interval
					annotations.tier[1, ]     <- c(tierName=t@tiers$name[tierNr], startsec=as.double(textgrid.startsec), endsec=as.double(textgrid.endsec), content="")
					annotations.tier$startsec <- as.double(annotations.tier$startsec)
					annotations.tier$endsec   <- as.double(annotations.tier$endsec)
				} else {
					#check for overlap of intervals, if there are more than one
					if (intervalNr>1) {
						#get intervals whose endsec is bigger then the startsec of the following
						overlaps <- annotations.tier$endsec[1:intervalNr-1]>annotations.tier$startsec[2:intervalNr]
						
						#if there are
						if (length(overlaps)>0) {
							if (any(overlaps==TRUE)) {
								#get the indices of those intervals
								overlaps <- c(1:length(overlaps))[overlaps]
								
								#replace endsec with startsec of the following interval
								annotations.tier$endsec[overlaps]<- annotations.tier$startsec[overlaps+1]
							}
						}
					}
					#get all times from tier, and add 0 and textgrid.endsec
					allTimes <- sort(unique(c(textgrid.startsec, textgrid.endsec, annotations.tier$startsec, annotations.tier$endsec )))
					
					#create empty intervals for all times
					newAnnotations			<- data.frame(
						tierName=t@tiers$name[tierNr], 
						startsec=as.double(allTimes[1:length(allTimes)-1]), 
						endsec=as.double(allTimes[2:length(allTimes)]), 
						content="", 
						stringsAsFactors=FALSE		)
					
					#merge new and actual annotations
					merged <- merge(annotations.tier, newAnnotations, all.y =TRUE, by= c("tierName","startsec", "endsec"))
					
					#set empty content to ""
					merged$content.x[is.na(merged$content.x)]<-""
					
					#remove superflous colums
					merged$content.y <- NULL
					
					annotations.tier <- merged
				}
				
				#add consecutive numbers
				annotations.tier <- cbind(as.character(1:nrow(annotations.tier)),annotations.tier)
				
				#rename columns
				colnames(annotations.tier) <- c("intervalNr", "tierName", "startsec","endsec", "content")
				
				#get number of intervals
				intervalNr <- nrow(annotations.tier)
				
				#add tier to list
				myTier <- list(name=t@tiers$name[tierNr], type="interval", t1=annotations.tier$startsec, t2=annotations.tier$endsec, label=annotations.tier$content)
				myTG[[t@tiers$name[tierNr]]] <- myTier
			} else if (t@tiers$type[tierNr] == "TextTier") {
				#get number of points in tier
				pointnr <- nrow(annotations.tier)
				
				#add consecutive numbers
				annotations.tier <- cbind(as.character(1:nrow(annotations.tier)),annotations.tier)
				
				#add tier to list
				myTier <- list(name=t@tiers$name[tierNr], type="point", t=annotations.tier$startsec, label=annotations.tier$content)
				myTG[[t@tiers$name[tierNr]]] <- myTier
			}
		}
	}

	#set meta information
	attr(myTG, "class") <- c(	"list", tmin=as.character(textgrid.startsec), 
							            tmax=as.character(textgrid.endsec), 
							            type="TextGrid", 
							            name=t@name)
	attr(myTG, "names") <- t@tiers$name
	return(myTG)
}

