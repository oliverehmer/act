#' Export a transcript object to a 'Praat' .TextGrid file
#' 
#' Advice: In most situations it is more convenient to use \code{act::corpus_export} for exporting annotation files.
#'
#' The .TextGrid file will be written to the file specified in \code{outputPath}.
#' If \code{outputPath} is left empty, the function will return the contents of the .TextGrid itself.
#' 
#' @param t Transcript object; transcript to be saved.
#' @param outputPath Character string; path where .TextGrid will be saved.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param filterSectionStartsec Double; start of selection in seconds.
#' @param filterSectionEndsec Double; end of selection in seconds.
#' 
#' @return Contents of the .TextGrid file (only if \code{outputPath} is left empty)
#' @export
#' 
#' @seealso \code{corpus_export}, \code{export_eaf}, \code{export_exb}, \code{export_printtranscript}, \code{export_rpraat}, \code{export_srt}  
#' 
#' @example inst/examples/export_textgrid.R
#' 
#' 
export_textgrid <- function(t, 
							outputPath=NULL, 
							filterTierNames=NULL, 
							filterSectionStartsec = NULL, 
							filterSectionEndsec = NULL) {
	
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	} else { if (class(t)[[1]]!="transcript") 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	
	#--- check if output folder exists
	if (!is.null(outputPath)) {
		if (!dir.exists(dirname(outputPath))) {
			stop("Output folder does not exist. Modify parameter 'outputPath'.")
		}
	}
	
	#=== Get data
	#--- Filter and cure transcript
	t <- act::transcripts_filter_single(t, filterTierNames=filterTierNames, filterSectionStartsec = filterSectionStartsec, filterSectionEndsec = filterSectionEndsec)
	t <- act::transcripts_cure_single(t, annotationsWithReversedTimes=TRUE, overlappingAnnotations=TRUE, annotationsWithTimesBelowZero=FALSE, missingTiers=TRUE, showWarning=TRUE)
	#--- get only relevant columns
	myCols <- c("tier.name", "startSec","endSec","content")
	if (!all(myCols %in% colnames(t@annotations))) {
		stop(paste("Missing colums. Annotations need to contain: ", paste(myCols, collapse = " ", sep="")))
	}
	myAnnotations <- t@annotations[,myCols]
	
	#sort annotations by start time
	myAnnotations <- myAnnotations[order(myAnnotations$startSec), ]
	
	#--- get min and max times of textgrid
	textgrid.startSec <- min(0, myAnnotations$startSec, myAnnotations$endSec)
	textgrid.endSec   <- max(c(t@length, myAnnotations$startSec, myAnnotations$endSec))
	
	#--- create TextGrid header
	myTG <- 			"File type = \"ooTextFile\""
	myTG <- append(myTG, "Object class = \"TextGrid\"" )
	myTG <- append(myTG, "" )
	myTG <- append(myTG, sprintf("xmin = %s ", as.character(textgrid.startSec)))
	myTG <- append(myTG, sprintf("xmax = %s ", as.character(textgrid.endSec)))
	myTG <- append(myTG, "tiers? <exists> ")
	myTG <- append(myTG, sprintf("size = %s ", nrow(t@tiers) ))
	myTG <- append(myTG, "item []: ")
	#cat(myTG)
	
	if (nrow(t@tiers)>0) {
		#iterate though all tiers
		for (tierNr in 1:nrow(t@tiers)) 		{
			# tierNr <- 2

			#get annotations within tier
			annotations.tier <- myAnnotations[myAnnotations$tier.name==t@tiers$name[tierNr],]
			
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
					annotations.tier$tier.name  <- addLevel(annotations.tier$tier.name, t@tiers$name[tierNr])
					
					#add an empty interval
					annotations.tier[1, ]		<- c(tier.name=t@tiers$name[tierNr], startSec=as.double(textgrid.startSec), endSec=as.double(textgrid.endSec), content="")
					annotations.tier$startSec   <- as.double(annotations.tier$startSec)
					annotations.tier$endSec     <- as.double(annotations.tier$endSec)
				} else {
					#get all times from tier, and add 0 and TextGrid-Length
					allTimes <- sort(unique(c(textgrid.startSec, textgrid.endSec, annotations.tier$startSec, annotations.tier$endSec )))
					
					#create empty intervals for all times
					newAnnotations			<- data.frame(
						tier.name=t@tiers$name[tierNr], 
						startSec=as.double(allTimes[1:length(allTimes)-1]), 
						endSec=as.double(allTimes[2:length(allTimes)]), 
						content="", 
						stringsAsFactors=FALSE		)
					
	
					
					#merge new and actual annotations
					merged <- merge(x=annotations.tier, y=newAnnotations, all.y =TRUE, by= c("tier.name","startSec", "endSec"))
					
					#set empty content to ""
					merged$content.x[is.na(merged$content.x)]<-""
					
					#remove superfluous columns
					merged$content.y <- NULL
					
					annotations.tier <- merged
				}
				
				#add consecutive numbers
				annotations.tier <- cbind(as.character(1:nrow(annotations.tier)), annotations.tier)
				
				#sort annotations by start time
				annotations.tier <- annotations.tier[order(annotations.tier$startSec), ]
				
				#rename columns
				colnames(annotations.tier) <- c("intervalNr", "tier.name", "startSec","endSec", "content")
				
				#get number of intervals
				intervalNr <- nrow(annotations.tier)
				
				myTG <- append(myTG, sprintf("    item [%s]:", tierNr))
				myTG <- append(myTG,         "        class = \"IntervalTier\" ")
				myTG <- append(myTG, sprintf("        name = \"%s\" " , t@tiers$name[tierNr]))
				myTG <- append(myTG, sprintf("        xmin = %s ", as.character(textgrid.startSec)))
				myTG <- append(myTG, sprintf("        xmax = %s ", as.character(textgrid.endSec)))
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
				myTG <- append(myTG, sprintf("        xmin = %s ", as.character(textgrid.startSec)))
				myTG <- append(myTG, sprintf("        xmax = %s ", as.character(textgrid.endSec)))
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
	}

	if (is.null(outputPath)) {
		return(myTG)
	} else {
		#---write to file
		fileConn <- file(outputPath)
		writeLines(myTG, fileConn)
		close(fileConn)		
	}
}
