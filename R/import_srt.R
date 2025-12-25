#' Import a single .srt 'subrib title'  object
#'
#' Advice: In most situations it is more convenient to use \code{act::corpus_new}, \code{act::corpus_import} for importing annotation files.
#'  
#' @param filePath Character string; input path of a single 'subribtitle' .srt file.
#' @param transcriptName Character string; name of the transcript.
#' @param tierName Character string; name of the imported tier
#' 
#' @return Transcript object.
#' 
#' @seealso \link{corpus_import}, \link{corpus_new}, \link{import}, \link{import_eaf}, \link{import_exb}, \code{import_textgrid}, \link{import_rpraat}    
#' 
#' @export
#'
#' @example inst/examples/import_rpraat.R
#' 
import_srt <- function(filePath, 
							transcriptName=NULL, 
							tierName='subtitle'
							) {
	#filePath <- '/Users/oliverehmer/Desktop/whisper/spa_everyday/spa_1201_ac.srt'
	#transcriptName<-"test"
	#tierName<-'subtitle'
	
	t 					<- methods::new("transcript")
	t@file.path 		<- if(is.null(filePath)) {""} else {filePath}
	
	#--- get transcript name
	if (!is.null(transcriptName)) {
		t@name <- transcriptName
	} else {
		t@name				<- stringr::str_replace(basename(filePath), ".srt","")
	}
	t@file.type 			<- "srt"
	t@import.result 		<- "ok"
	t@file.encoding			<- "UTF8"
	t@load.message    	    <- ""
	t@modification.systime  <- character()
	
	
	
	#=== load file
	if (!is.null(filePath)) {
		#--- check if file exists
		if (!file.exists(filePath)) {
			t@import.result  <- "error"
			t@load.message   <- "File does not exist."
			return(t)
		}
		
		#--- check if the file can be opened
		mysrt <- srt::read_srt(	t@file.path 		)	
	}
	if(getOption("act.import.storefileContentInTranscript", default=TRUE)) {
		myCon <- file(filePath, encoding = "UTF-8")
		mytxt <- readLines(myCon, warn=FALSE)
		close(myCon)
		t@file.content <- mytxt
	}
	
	
	#---create unique tierNames
	tierNames <- 	tierName[1]
	myTiers <- rep("", length(tierNames))
	names(myTiers) <- tierNames
	myTiers[1] <- "IntervalTier"
	
	#--- convert annotations 
	ann <- data.frame()
	
	if (nrow(mysrt)>0) {
		#set annotations
		ann <- mysrt
		
		#set col names		
		colnames(ann) <- c('tierName', 'startsec', 'endsec','content')
		
		#insert name of tier
		ann$tierName<-tierNames[1]
	}
	
	#View(mysrt)
	
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
	#View(t@annotations)
	
	#=== assign other important values to object
	t@tiers <- act::helper_tiers_new_table(tierNames=names(myTiers), tierTypes = myTiers)
	#make transcript 1 sec longer than last annotation
	t@length.sec <- max(as.double(ann$startsec)+1, as.double(ann$endsec)+1)
	
	t@history <- list( 
						 list(modification                               = "import_rpraat",
						 	 systime                                       = Sys.time()
						 )
	)
	return(t)
}
