#' Import a single 'EXMARaLDA' .exb file
#'
#' Advice: In most situations it is more convenient to use \code{act::corpus_new}, \code{act::corpus_import} for importing annotation files.
#' 
#' Imports the contents of a 'EXMARaLDA' .exb file and returns a transcript object.
#' The source is either the path to a .exb file or the contents of a .exb file obtained from the \code{@file.content} of an existing transcript object.
#' If you pass 'fileContent' you need to pass 'transcriptName' as parameter, too.
#' 
#' Please note: 
#' - 'EXMARaLDA' allows for empty time slots without a time values. Missing values will be interpolated during the import. You will not be able to recognize interpolated values in the data.  
#' - Meta data for tiers (such as the display name etc.) will not be imported.
#' - Media files are referenced not by their path but only as file names in .exb files. The names will be imported but will not work as paths in act. 
#' 
#' @param filePath Character string; input path of a single 'EXMARaLDA' .exb file.
#' @param fileContent Vector of character strings; contents of a 'EXMARaLDA' .exb file .
#' @param transcriptName Character string; name of the transcript.
#' 
#' @return Transcript object.
#' 
#' @seealso \code{corpus_import}, \code{corpus_new}, \code{import}, \code{import_eaf}, \code{import_rpraat}, \code{import_textgrid}  
#' 
#' @export
#'
#' @example inst/examples/import_exb.R
#' 
import_exb <- function(filePath=NULL, 
					   fileContent=NULL, 
					   transcriptName=NULL) {
	
	if (is.null(filePath) & is.null(fileContent)) {
		stop("You need to pass as parameter eiter a file path to a EXMARaLDA file (filePath) or the contents of a EXMARaLDA file (fileContent) as parameter.")
	}
	if (!is.null(filePath) & !is.null(fileContent)) {
		stop("Please pass only filePath or fileContent as parameter, not both.")
	}
	if (!is.null(fileContent) & is.null(transcriptName)) {
		stop("If you pass 'fileContent' you need to pass 'transcriptName' as parameter, too.")
	}
	
	#--- new transcript
	t 					<- methods::new("transcript")
	t@file.path 			<- if(is.null(filePath)) {""} else {filePath}
	
	#--- get transcript name
	if (!is.null(transcriptName)) {
		t@name <- transcriptName
	} else {
		if(!is.null(filePath)) {
			t@name <- tools::file_path_sans_ext(basename(filePath)	)		
		} else {
			t@name <- "imported transcript"
		}
	}
	t@file.type 			 <- "exb"
	t@import.result 		 <- "ok"
	t@load.message 	     <- ""
	t@file.encoding		 <- "UTF8"
	t@modification.systime <- character()
	
	#=== take file content
	if (!is.null(fileContent)) {
		myexb <- fileContent 
	}
	
	#=== load file
	if (!missing(filePath)) {
		#--- check if file exists
		if (!file.exists(filePath)) {
			t@import.result 		<- "error"
			t@load.message   <- "File does not exist."
			return(t)
		}
		
		#--- check if the file can be opened
		out <- tryCatch(
			{ xml2::read_xml(filePath) 	},
			error=function(cond) { 	NULL }
		)
		if (is.null(out)) 						{	
			t@import.result 		<- "error"
			t@load.message   <- "File not recognized as .exb"
			return(t)
		}
		myexb <- out
	}
	if(getOption("act.import.storeFileContentInTranscript", default=TRUE)) {
		myCon <- file(filePath, encoding = "UTF-8")
		mytxt <- readLines(myCon, warn=FALSE)
		close(myCon)
		t@file.content <- mytxt
	}
	
	#Check if it is an exb file
	out <- tryCatch(
		{ meta <- xml2::xml_find_all(myexb, "head/meta-information")
		timeline <- xml2::xml_find_all(myexb, "basic-body/common-timeline")
		tiers <- xml2::xml_find_all(myexb, "basic-body/tier")
		if (length(meta)==0 | length(timeline)==0 |length (tiers)==0) {NULL} else {"exb"}
		},
		error=function(cond) { 	NULL }
	)
	if (is.null(out)) 						{	
		t@import.result 		<- "error"
		t@load.message   <- "File not recognized as .exb"
		return(t)
	}
	
	#=== head / mediafile
	meta <- xml2::xml_find_all(myexb, "head/meta-information")
	transName <- xml2::xml_text(xml2::xml_find_all(meta, "transcription-name"))
	mediafiles <- xml2::xml_find_all(meta, "referenced-file")
	mediafiles <- xml2::xml_attr(mediafiles, "url")
	t@media.path <- mediafiles
	
	#=== speakers metainfo
	# ...
	
	#=== time slots
	timeline <- xml2::xml_find_all(myexb, "basic-body/common-timeline")
	tli <- xml2::xml_find_all(timeline, "tli")
	tsID <-xml2::xml_attr(tli, "id")
	tsTime <- xml2::xml_attr(tli, "time")	
	tsType <- xml2::xml_attr(tli, "type")
	
	if (length(tsTime)==0) {
		#there are no timeslots 
		#--> treat as empty transcript
		t@length <- 0
		t@annotations <- .emptyAnnotations
		t@tiers       <- .emptyTiers
		
	} else {
		#--- interpolate missing values
		#correct first timeslot being NA
		if (is.na(tsTime[1])) {
			tsTime[1]<-0
		}
		#correct last timeslot being NA
		if (is.na(tsTime[length(tsTime)])) {
			if (length(tsTime)==2) {
				tsTime[2]=1
			} else {
				tsTime[length(tsTime)]=tsTime[length(tsTime)-1]
			}
		}
		
		#if there are NA values in the time slots
		if (any(is.na(tsTime)))  {
			#get the sequences of NAs
			rl <- rle(is.na(tsTime)) 
			cumsums <-cumsum(rl$length)+1
			rl$start <- c(0,cumsums[1:length(cumsums)-1])-1
			rl$end <- rl$start + rl$length +1
			nasequences <-data.frame(value=rl$values, start=rl$start, end=rl$end)
			nasequences <- nasequences[nasequences$value==TRUE,]
			for (i in 1:nrow(nasequences)) {
				timesequence <- seq(from = tsTime[nasequences$start[i]], to = tsTime[nasequences$end[i]], length.out = nasequences$end[i]-nasequences$start[i]+1)
				tsTime[ nasequences$start[i]:nasequences$end[i]]<-timesequence
			}
		}
		timeslots <- data.frame(id = tsID, value = as.double(tsTime), tsType=tsType, stringsAsFactors = FALSE)
		#View(timeslots)
		
		#--- set transcript length
		t@length <- max(timeslots$value)
		
		#=== extract tier info
		tiers <- xml2::xml_find_all(myexb, "basic-body/tier")
		tier.name <- xml2::xml_attr(tiers, "id")
		tier.speaker <- xml2::xml_attr(tiers, "speaker")
		tier.category <- xml2::xml_attr(tiers, "category")
		tier.type <- xml2::xml_attr(tiers, "type")
		tier.displayName <- xml2::xml_attr(tiers, "display-name")
		mytiers <- as.data.frame(cbind(tier.name, tier.speaker, tier.category, tier.type, tier.displayName))
		
		#=== extract annotations
		annotations <- data.frame(stringsAsFactors = FALSE)
		i <-7
		for (i in 1:length(tiers)) {
			tierID <- xml2::xml_attr(tiers[[i]], "id")
	
			content <- xml2::xml_text(xml2::xml_find_all(tiers[[i]], "event"))
			#if (length(content) == 0) { content <- "" }
			content <-content[!is.na(content)]
			
			ts1     <- xml2::xml_attr(xml2::xml_children(tiers[[i]]), "start")
			#if (length(ts1) == 0) {	ts1 <- "" }
			ts1 <-ts1[!is.na(ts1)]
			
			ts2     <- xml2::xml_attr(xml2::xml_children(tiers[[i]]), "end")
			#if (length(ts2) == 0) { ts2 <- "" }
			ts2 <-ts2[!is.na(ts2)]
			tierID <- rep(tierID, length(content))
			
			test <-c( length(tierID), length(content), length(ts1),  length(ts2))
			if (length(unique(test))!=1) {
				print(i)
				print(t@file.path)
			}
			
			annotations <- rbind( annotations, cbind(tierID, content, ts1, ts2))
		}
		
		
		if (nrow(annotations)==0)  	{
			t@annotations  <- .emptyAnnotations
			t@tiers        <- .emptyTiers 
		} else {
			#--- merge annotations with timeslots
			annotations <- merge(annotations, timeslots, by.x = "ts1", by.y = "id")
			annotations <- merge(annotations, timeslots, by.x = "ts2", by.y = "id")
			annotations <- annotations[,c(3,5,7,4)]
			names(annotations) <- c("tier.name","startSec","endSec", "content")
			
			annotationID <-c(1:nrow(annotations))
			t@annotations <-data.frame(
				annotationID = as.integer(annotationID),
				
				tier.name  				= annotations$tier.name,
				startSec  				= as.double(annotations$startSec),
				endSec  				= as.double(annotations$endSec),
				content  				= as.character(annotations$content),
				
				content.norm            = as.character(""),
				char.orig.bytime.start 	= rep(as.integer(NA),length(annotationID)),
				char.orig.bytime.end	= rep(as.integer(NA),length(annotationID)),
				char.norm.bytime.start	= rep(as.integer(NA),length(annotationID)),
				char.norm.bytime.end	= rep(as.integer(NA),length(annotationID)),
				char.orig.bytier.start	= rep(as.integer(NA),length(annotationID)),
				char.orig.bytier.end 	= rep(as.integer(NA),length(annotationID)),
				char.norm.bytier.start 	= rep(as.integer(NA),length(annotationID)),
				char.norm.bytier.end 	= rep(as.integer(NA),length(annotationID)),
				row.names				= annotationID, 
				stringsAsFactors		= FALSE)
			rownames(t@annotations) 	<-  t@annotations$annotationID
			
			#===set correct column format
			t@annotations$annotationID	<- as.integer(t@annotations$annotationID)
			t@annotations$startSec		<- as.double(t@annotations$startSec)
			t@annotations$endSec  		<- as.double(t@annotations$endSec)
			t@annotations$content  		<- as.character(t@annotations$content)
			
			
			
			#=== get rid of empty intervals
			if (options()$act.import.readEmptyIntervals==FALSE) 		{
				t@annotations <- t@annotations[t@annotations$content!="",]
			}
			t@annotations <- t@annotations[is.na(t@annotations["content"])==FALSE,]
			
			if (nrow(t@annotations)>0) 		{
				#=== sort transcript by start times
				t@annotations <- t@annotations[order(t@annotations$startSec, t@annotations$tier.name), ]
				
				#=== set annotations.id again
				t@annotations$annotationID <- c(1:nrow(t@annotations))
				
				#=== set the new row names
				rownames(t@annotations) <- t@annotations$annotationID
			}
		}
		
		#=== html conversion
		t@annotations$content      <- textutils::HTMLdecode(t@annotations$content)
		t@annotations$tier.name    <- textutils::HTMLdecode(t@annotations$tier.name)
		mytiers$tier.name                <- textutils::HTMLdecode(mytiers$tier.name)
		
		#--- tiers to object
		t@tiers <- act::helper_tiers_new_table(tierNames=mytiers$tier.name)
	}
	
	t@history <- list(
						 list(modification                               = "import_exb",
						 	 systime                                       = Sys.time()
						 )
	)
	return(t)
}

