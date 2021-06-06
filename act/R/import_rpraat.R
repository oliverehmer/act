#' Import a single 'rPraat' TextGrid object
#'
#' This function is to create compatibility with the \code{rPraat} package. 
#' It converts a 'rPraat' TextGrid object into an act transcript object.
#' 
#' Please note:
#' - Time values of annotations in TextGrids may be below 0 seconds. Negative time values will be recognized corretly in the first place. When exporting transcript object to other formats like 'ELAN' .eaf, 'EXMARaLDA' .exb ect. annotations that are completely before 0 sec will be deleted, annotations that start before but end after 0 sec will be truncated. Please see also the function \code{act::transcripts_cure_single}.  
#' - TextGrids and contained tiers may start and end at different times. These times do not need to match each other. The act package does not support start and end times of TextGrids and tiers and will. The default start of a TextGrid will be 0 seconds or the lowest value in case that annotations start below 0 seconds.
#' 
#' Credits: Thanks to Tomáš Bořil, the author of the rPraat package, for commenting on the exchange functions.
#' 
#' @param rPraatTextGrid List; rPraat TextGrid object.
#' @param transcriptName Character string; name of the transcript.
#' 
#' @return Transcript object.
#' 
#' @seealso \code{corpus_import}, \code{corpus_new}, \code{import}, \code{import_eaf}, \code{import_exb}, \code{import_textgrid}  
#' 
#' @export
#' 
#' @seealso \link{export_rpraat}, \link{import}, \link{import_textgrid}, \link{import_eaf}
#'
#' @example inst/examples/import_rpraat.R
#' 
import_rpraat <- function(rPraatTextGrid, 
						  transcriptName=NULL) {
	
	#rPraatTextGrid <- tg
	#transcriptName<-"test"
	
	if (length(setdiff(c("tmin", "tmax", "type", "name"), names(attr(rPraatTextGrid, "class"))))!=0) {
		stop("The object that you have passed as 'rPraatTextGrid' does not seem to be a valid rPraat TextGrid.")
	}
	
	t 					<- methods::new("transcript")
	t@file.path 		<- ""
	
	#--- get transcript name
	if (!is.null(transcriptName)) {
		t@name <- transcriptName
	} else {
		t@name				<- stringr::str_replace(attr(rPraatTextGrid, "class")["name"], ".TextGrid","")
	}
	t@file.type 			 <- "rpraat"
	t@import.result 		 <- "ok"
	t@load.message    	     <- ""
	t@modification.systime   <- character()
	t@length.sec        	 <- as.double(attr(rPraatTextGrid, "class")["tmax"])
	if(getOption("act.import.storeFileContentInTranscript", default=TRUE)) {
		t@file.content <- rPraatTextGrid
	}
	
	tierNames					<- 	attr(rPraatTextGrid, "names")
	
	#---create unique tierNames
	if (length(tierNames[duplicated(tierNames)])>0) {
		tierNames <- make.unique(tierNames)
		t@import.result    <- "ok"
		t@load.message   <- "Some tiers have been renamed since their names were not unique."
	}
	
	myTiers <- rep("", length(tierNames))
	names(myTiers) <- tierNames
	
	#--- convert annotations 
	myAnnotations <- data.frame()
	if (length(rPraatTextGrid)>0) {
		for (i in 1:length(rPraatTextGrid)) {
			if (rPraatTextGrid[[i]]$type=="interval") {
				myAnnotations <- rbind(myAnnotations, 
									  cbind(
									  	tier.name = tierNames[[i]], 
									  	startSec  = as.double(rPraatTextGrid[[i]]$t1), 
									  	endSec    = as.double(rPraatTextGrid[[i]]$t2), 
									  	content   = rPraatTextGrid[[i]]$label
									  	)
									  )
			} else {
				myAnnotations <- rbind(myAnnotations, 
									  cbind(
									  	tier.name = tierNames[[i]], 
									  	startSec  = as.double(rPraatTextGrid[[i]]$t), 
									  	endSec    = as.double(rPraatTextGrid[[i]]$t), 
									  	content   = rPraatTextGrid[[i]]$label
									  	)
									  )
			}
			myTiers[i] <- if (rPraatTextGrid[[i]]$type=="interval") {"IntervalTier"} else {"TextTier"}
		}
	}
	
	if (nrow(myAnnotations)==0) {
		t@annotations 				<- .emptyAnnotations
	} else {
		annotationID <- c(1:nrow(myAnnotations))
		myAnnotations <- cbind(
			annotationID 			= as.integer(annotationID),
			
			myAnnotations,	
			
			content.norm 			= as.character(""),
			char.orig.bytime.start	= rep(as.integer(NA), nrow(myAnnotations)),
			char.orig.bytime.end	= rep(as.integer(NA), nrow(myAnnotations)),
			char.norm.bytime.start	= rep(as.integer(NA), nrow(myAnnotations)),
			char.norm.bytime.end	= rep(as.integer(NA), nrow(myAnnotations)),
			char.orig.bytier.start	= rep(as.integer(NA), nrow(myAnnotations)),
			char.orig.bytier.end	= rep(as.integer(NA), nrow(myAnnotations)),
			char.norm.bytier.start	= rep(as.integer(NA), nrow(myAnnotations)),
			char.norm.bytier.end	= rep(as.integer(NA), nrow(myAnnotations)),
			row.names				= annotationID, 
			stringsAsFactors		= FALSE) 
		rownames(myAnnotations) 	<-  myAnnotations$annotationID
		
		#===set correct column format
		myAnnotations$annotationID	<- as.integer(myAnnotations$annotationID)
		myAnnotations$startSec		<- as.double(myAnnotations$startSec)
		myAnnotations$endSec  		<- as.double(myAnnotations$endSec)
		myAnnotations$content  		<- as.character(myAnnotations$content)
		
		#=== get rid of empty intervals
		if (options()$act.import.readEmptyIntervals==FALSE) 		{
			myAnnotations <- myAnnotations[myAnnotations$content!="",]
		}
		myAnnotations <- myAnnotations[is.na(myAnnotations["content"])==FALSE,]
		
		if (nrow(myAnnotations)>0) 		{
			#=== sort transcript by start times
			myAnnotations <- myAnnotations[order(myAnnotations$startSec, myAnnotations$tier.name), ]
			
			#=== set endSec of points to startSec
			myAnnotations$endSec[is.na(myAnnotations$endSec)] <- myAnnotations$startSec[is.na(myAnnotations$endSec)]
			
			#=== set annotations.id again
			myAnnotations$annotationID <- c(1:nrow(myAnnotations))
			
			#=== set the new row names
			rownames(myAnnotations) <- myAnnotations$annotationID
		}	
		t@annotations 	<- myAnnotations
	}
	
	
	#=== assign other values to object
	t@tiers <- act::helper_tiers_new_table(tierNames=names(myTiers), tierTypes = myTiers)
	
	
	t@history <- list( 
						 list(modification                               = "import_rpraat",
						 	 systime                                       = Sys.time()
						 )
	)
	return(t)
}
