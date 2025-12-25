#' Export 'Praat' .TextGrid file
#' 
#' Advice: In most situations it is more convenient to use \code{act::corpus_export} for exporting annotation files.
#'
#' The .TextGrid file will be written to the file specified in \code{pathOutput}.
#' If \code{pathOutput} is left empty, the function will return the contents of the .TextGrid itself.
#' 
#' @param t Transcript object; transcript to be saved.
#' @param pathOutput Character string; path where .TextGrid will be saved.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param filterSectionStartsec Double; start of selection in seconds.
#' @param filterSectionEndsec Double; end of selection in seconds.
#' 
#' @return Contents of the .TextGrid file (only if \code{pathOutput} is left empty)
#' @export
#' 
#' @seealso \link{corpus_export}, \link{export_eaf}, \link{export_exb}, \link{export_txt}, \link{export_docx}, \link{export_rpraat}, \link{export_srt}  
#' 
#' @example inst/examples/export_textgrid.R
#' 
#' 
export_textgrid <- function(t, 
							pathOutput = NULL, 
							filterTierNames = NULL, 
							filterSectionStartsec = NULL, 
							filterSectionEndsec = NULL) {
	
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	}	else { if (!methods::is(t, "transcript")) 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	
	#--- check if output folder exists
	if (!is.null(pathOutput)) {
		if (!dir.exists(dirname(pathOutput))) {
			stop("Output folder does not exist. Modify parameter 'pathOutput'.")
		}
	}
	
	#=== Get data
	#--- Filter and cure transcript
	t <- act::transcripts_filter_single(t, filterTierNames=filterTierNames, filterSectionStartsec = filterSectionStartsec, filterSectionEndsec = filterSectionEndsec)
	t <- act::transcripts_cure_single(t, annotationsTimesReversed=TRUE, annotationsOverlap=TRUE, annotationsTimesBelowZero=FALSE, tiersMissing=TRUE, warning=TRUE)
	
	if (nrow(t@tiers)==0) {
		warning(unique(sprintf('Textgrid for transcript "%s" not exported. Transcript did not contain any tiers (after filtering).', t@name)))
	} else {
		#--- get only relevant columns
		myCols <- c("tierName", "startsec","endsec","content")
		if (!all(myCols %in% colnames(t@annotations))) {
			stop(paste("Missing colums. Annotations need to contain: ", paste(myCols, collapse = " ", sep="")))
		}
		ann <- t@annotations[,myCols]
		
		#sort annotations by start time
		ann <- ann[order(ann$startsec), ]
		
		#--- get min and max times of textgrid
		textgrid.startsec <- min(0, ann$startsec, ann$endsec)
		textgrid.endsec   <- max(c(t@length.sec, ann$startsec, ann$endsec))
		
		#--- create TextGrid header
		myTG <- 			"File type = \"ooTextFile\""
		myTG <- append(myTG, "Object class = \"TextGrid\"" )
		myTG <- append(myTG, "" )
		myTG <- append(myTG, sprintf("xmin = %s ", as.character(textgrid.startsec)))
		myTG <- append(myTG, sprintf("xmax = %s ", as.character(textgrid.endsec)))
		myTG <- append(myTG, "tiers? <exists> ")
		myTG <- append(myTG, sprintf("size = %s ", nrow(t@tiers) ))
		myTG <- append(myTG, "item []: ")
		#cat(myTG)
		
		
		#iterate though all tiers
		for (tierNr in 1:nrow(t@tiers)) 		{
			# tierNr <- 2
			
			#get annotations within tier
			annotations.tier <- ann[ann$tierName==t@tiers$name[tierNr],]
			
			if (t@tiers$type[tierNr] == "IntervalTier") {
				#get number of intervals in tier
				intervalNr <- nrow(annotations.tier)
				if (intervalNr==0) 	{
					#no data in this tier
					addLevel <- function(x, newlevel = NULL) {
						if(is.factor(x)) {
							if (is.na(match(newlevel, levels(x))))
								return(factor(x, levels=c(levels(x), newlevel)))
						}
						return(x)
					}
					annotations.tier$tierName  <- addLevel(annotations.tier$tierName, t@tiers$name[tierNr])
					
					#add an empty interval
					annotations.tier[1, ]		<- c(tierName=t@tiers$name[tierNr], startsec=as.double(textgrid.startsec), endsec=as.double(textgrid.endsec), content="")
					annotations.tier$startsec   <- as.double(annotations.tier$startsec)
					annotations.tier$endsec     <- as.double(annotations.tier$endsec)
				} else {
					#get all times from tier, and add 0 and TextGrid-Length
					allTimes <- sort(unique(c(textgrid.startsec, textgrid.endsec, annotations.tier$startsec, annotations.tier$endsec )))
					
					#create empty intervals for all times
					newAnnotations			<- data.frame(
						tierName=t@tiers$name[tierNr], 
						startsec=as.double(allTimes[1:length(allTimes)-1]), 
						endsec=as.double(allTimes[2:length(allTimes)]), 
						content="", 
						stringsAsFactors=FALSE		)
					
					
					
					#merge new and actual annotations
					merged <- merge(x=annotations.tier, y=newAnnotations, all.y =TRUE, by= c("tierName","startsec", "endsec"))
					
					#set empty content to ""
					merged$content.x[is.na(merged$content.x)]<-""
					
					#remove superfluous columns
					merged$content.y <- NULL
					
					annotations.tier <- merged
				}
				
				#add consecutive numbers
				annotations.tier <- cbind(as.character(1:nrow(annotations.tier)), annotations.tier)
				
				#sort annotations by start time
				annotations.tier <- annotations.tier[order(annotations.tier$startsec), ]
				
				#rename columns
				colnames(annotations.tier) <- c("intervalNr", "tierName", "startsec","endsec", "content")
				
				#get number of intervals
				intervalNr <- nrow(annotations.tier)
				
				myTG <- append(myTG, sprintf("    item [%s]:", tierNr))
				myTG <- append(myTG,         "        class = \"IntervalTier\" ")
				myTG <- append(myTG, sprintf("        name = \"%s\" " , t@tiers$name[tierNr]))
				myTG <- append(myTG, sprintf("        xmin = %s ", as.character(textgrid.startsec)))
				myTG <- append(myTG, sprintf("        xmax = %s ", as.character(textgrid.endsec)))
				myTG <- append(myTG, sprintf("        intervals: size = %s " , intervalNr))
				
				
				myInter <- paste(   '        intervals [%s]:', 
									'            xmin = %s ',  
									'            xmax = %s ', 
									'            text = \"%s\" ',
									sep="\n", collapse="\n")
				
				allInter<- c()
				for (i in 1:intervalNr) {
					allInter[i] <- sprintf(myInter, annotations.tier[i,1], as.character(annotations.tier[i, 3]), as.character(annotations.tier[i, 4]), stringr::str_replace_all( annotations.tier[i, 5], "\"", "\"\""))
				}
				a<- paste(allInter, sep="", collapse='\n')
				
				myTG <- append(myTG, a)
			} else if (t@tiers$type[tierNr] == "TextTier") {
				
				#get number of points in tier
				pointNr <- nrow(annotations.tier)
				
				myTG <- append(myTG, sprintf("    item [%s]:", tierNr))
				myTG <- append(myTG,         "        class = \"TextTier\" ")
				myTG <- append(myTG, sprintf("        name = \"%s\" " , t@tiers$name[tierNr]))
				myTG <- append(myTG, sprintf("        xmin = %s ", as.character(textgrid.startsec)))
				myTG <- append(myTG, sprintf("        xmax = %s ", as.character(textgrid.endsec)))
				myTG <- append(myTG, sprintf("        points: size = %s " , pointNr))
				
				if (pointNr>0) {
					#add consecutive numbers
					annotations.tier <- cbind(as.character(1:nrow(annotations.tier)),annotations.tier)
					
					#createPointBlock <- function(myPoint) {
					#	myInter <- sprintf(                 "        points [%s]:" , myPoint[1])
					#	myInter <- append(myInter, sprintf(	"            number = %s " , as.character(myPoint[3])))
					#	myInter <- append(myInter, sprintf( "            mark = \"%s\" " , stringr::str_replace_all(  myPoint[5], "\"", "\"\"")))
					#	return(myInter)
					#}
					#a <- unlist(apply(annotations.tier,  1, FUN=createPointBlock))
					
					myPoint <- paste(   '        points [%s]:', 
										'            number = %s ',  
										'            mark = \"%s\" ',
										sep="\n", collapse="\n")
					
					allPoints<- c()
					for (i in 1:pointNr) {
						allPoints[i] <- sprintf(myPoint, annotations.tier[i,1], as.character(annotations.tier[i, 3]), stringr::str_replace_all( annotations.tier[i, 5], "\"", "\"\""))
					}
					a <- paste(allPoints, sep="", collapse='\n')
					
					myTG <- append(myTG, a)	
				}
			}
		}
		
		
		if (is.null(pathOutput)) {
			return(myTG)
		} else {
			#---write to file
			fileConn <- file(pathOutput)
			writeLines(myTG, fileConn)
			close(fileConn)		
		}
	}
}