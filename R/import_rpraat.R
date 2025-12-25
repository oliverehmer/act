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
#' @param rpraatTextgrid List; rPraat TextGrid object.
#' @param transcriptName Character string; name of the transcript.
#' 
#' @return Transcript object.
#' 
#' @seealso \link{corpus_import}, \link{corpus_new}, \link{import}, \link{import_eaf}, \link{import_exb}, \link{import_textgrid}, \link{export_rpraat}  
#' 
#' @export
#' 
#' @example inst/examples/import_rpraat.R
#' 
import_rpraat <- function(rpraatTextgrid, 
						  transcriptName=NULL) {
	
	#rpraatTextgrid <- tg
	#transcriptName<-"test"
	
	if (length(setdiff(c("tmin", "tmax", "type", "name"), names(attr(rpraatTextgrid, "class"))))!=0) {
		stop("The object that you have passed as 'rpraatTextgrid' does not seem to be a valid rPraat TextGrid.")
	}
	
	t 					<- methods::new("transcript")
	t@file.path 		<- ""
	
	#--- get transcript name
	if (!is.null(transcriptName)) {
		t@name <- transcriptName
	} else {
		t@name				<- stringr::str_replace(attr(rpraatTextgrid, "class")["name"], ".TextGrid","")
	}
	t@file.type 			 <- "rpraat"
	t@import.result 		 <- "ok"
	t@load.message    	     <- ""
	t@modification.systime   <- character()
	t@length.sec        	 <- as.double(attr(rpraatTextgrid, "class")["tmax"])
	if(getOption("act.import.storefileContentInTranscript", default=TRUE)) {
		t@file.content <- rpraatTextgrid
	}
	
	tierNames					<- 	attr(rpraatTextgrid, "names")
	
	#---create unique tierNames
	if (length(tierNames[duplicated(tierNames)])>0) {
		tierNames <- make.unique(tierNames)
		t@import.result    <- "ok"
		t@load.message   <- "Some tiers have been renamed since their names were not unique."
	}
	
	myTiers <- rep("", length(tierNames))
	names(myTiers) <- tierNames
	
	#--- convert annotations 
	ann <- data.frame()
	if (length(rpraatTextgrid)>0) {
		for (i in 1:length(rpraatTextgrid)) {
			if (rpraatTextgrid[[i]]$type=="interval") {
				ann <- rbind(ann, 
									  cbind(
									  	tierName = tierNames[[i]], 
									  	startsec  = as.double(rpraatTextgrid[[i]]$t1), 
									  	endsec    = as.double(rpraatTextgrid[[i]]$t2), 
									  	content   = rpraatTextgrid[[i]]$label
									  	)
									  )
			} else {
				ann <- rbind(ann, 
									  cbind(
									  	tierName = tierNames[[i]], 
									  	startsec  = as.double(rpraatTextgrid[[i]]$t), 
									  	endsec    = as.double(rpraatTextgrid[[i]]$t), 
									  	content   = rpraatTextgrid[[i]]$label
									  	)
									  )
			}
			myTiers[i] <- if (rpraatTextgrid[[i]]$type=="interval") {"IntervalTier"} else {"TextTier"}
		}
	}
	
	if (nrow(ann)==0) {
		t@annotations 				<- .emptyAnnotations
	} else {
		annotationID <- c(1:nrow(ann))
		ann <- cbind(
			annotationID 			= as.integer(annotationID),
			
			ann,	
			
			content.norm 			= as.character(""),
			char.orig.bytime.start	= rep(as.integer(NA), nrow(ann)),
			char.orig.bytime.end	= rep(as.integer(NA), nrow(ann)),
			char.norm.bytime.start	= rep(as.integer(NA), nrow(ann)),
			char.norm.bytime.end	= rep(as.integer(NA), nrow(ann)),
			char.orig.bytier.start	= rep(as.integer(NA), nrow(ann)),
			char.orig.bytier.end	= rep(as.integer(NA), nrow(ann)),
			char.norm.bytier.start	= rep(as.integer(NA), nrow(ann)),
			char.norm.bytier.end	= rep(as.integer(NA), nrow(ann)),
			row.names				= annotationID, 
			stringsAsFactors		= FALSE) 
		rownames(ann) 	<-  ann$annotationID
		
		#===set correct column format
		ann$annotationID	<- as.integer(ann$annotationID)
		ann$startsec		<- as.double(ann$startsec)
		ann$endsec  		<- as.double(ann$endsec)
		ann$content  		<- as.character(ann$content)
		
		#=== get rid of empty intervals
		if (options()$act.import.readEmptyIntervals==FALSE) 		{
			ann <- ann[ann$content!="",]
		}
		ann <- ann[is.na(ann["content"])==FALSE,]
		
		if (nrow(ann)>0) 		{
			#=== sort transcript by start times
			ann <- ann[order(ann$startsec, ann$tierName), ]
			
			#=== set endsec of points to startsec
			ann$endsec[is.na(ann$endsec)] <- ann$startsec[is.na(ann$endsec)]
			
			#=== set annotations.id again
			ann$annotationID <- c(1:nrow(ann))
			
			#=== set the new row names
			rownames(ann) <- ann$annotationID
		}	
		t@annotations 	<- ann
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
