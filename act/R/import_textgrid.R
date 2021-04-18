#' Import a single 'Praat' .TextGrid file
#' 
#' Advice: In most situations it is more convenient to use \code{act::corpus_new}, \code{act::corpus_import} for importing annotation files.
#' 
#' Imports the contents of a 'Praat' .TextGrid file and returns a transcript object.
#' The source is either the path to a .TextGrid file or the contents of a .TextGrid file obtained from the \code{@file.content} of an existing transcript object by \code{readLines()}.
#' If you pass 'fileContent' you need to pass 'transcriptName' as parameter, too.
#' 
#' Please note:
#' - Time values of annotations in TextGrids may be below 0 seconds. Negative time values will be recognized corretly in the first place. When exporting transcript object to other formats like 'ELAN' .eaf, 'EXMARaLDA' .exb ect. annotations that are completely before 0 sec will be deleted, annotations that start before but end after 0 sec will be truncated. Please see also the function \code{act::transcripts_cure_single}.  
#' - TextGrids and contained tiers may start and end at different times. These times do not need to match each other. The act package does not support start and end times of TextGrids and tiers and will. The default start of a TextGrid will be 0 seconds or the lowest value in case that annotations start below 0 seconds.
#'
#' @param filePath Character string; input path of a single 'Praat' .TextGrid file.
#' @param fileContent Vector of character strings; contents of a 'Praat' .TextGrid file read with \code{readLines()}.
#' @param transcriptName Character string; name of the transcript.
#' 
#' @return Transcript object.
#' 
#' @seealso \code{corpus_import}, \code{corpus_new}, \code{import}, \code{import_eaf}, \code{import_exb}, \code{import_rpraat} 
#' 
#' @export
#'
#' @example inst/examples/import_textgrid.R
#' 
import_textgrid <- function(filePath=NULL, 
							fileContent=NULL, 
							transcriptName=NULL) {
	
	#filePath<-	'/Users/oliverehmer/Desktop/Mary_John_bell.TextGrid'
	
	if (is.null(filePath) & is.null(fileContent)) {
		stop("You need to pass as parameter eiter a file path to a TextGrid file (filePath) or the contents of a TextGrid file (fileContent) as parameter.")
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
			t@name <- tools::file_path_sans_ext(basename(filePath))
		} else {
			t@name <- "imported transcript"
		}
	}
	
	t@file.type 			   <- "textgrid"
	t@import.result 		   <- "ok"
	t@load.message 	           <- ""
	t@modification.systime     <- character()
	
	if (!missing(filePath)) {
		#--- check if file exists
		if (!file.exists(filePath)) {
			t@import.result    <- "error"
			t@load.message   <- "File does not exist."
			return(t)
		}
		
		#=========================================================================================
		# Trying to read files (actually in LATIN1)  with option enc=UTF-18/-8 will result in an error.
		# Function will not work when trying to read files (actually in UTF8) with enc=LATIN1		# (
		#--> that's why i need to try first with utf
		myEncodings	<- c("UTF-16", "UTF-8", "LATIN1")
		
		#test all encondings, tell me in the end which worked
		mytg <- NULL
		t@file.encoding <- "unknown"
		#myEncoding <- "UTF-8"
		#result <- readLines(con=filePath, encoding=myEncoding)
		for (myEncoding in myEncodings)	{
			#try to read first 2 lines
			result	<- helper_test_read(filePath, myEncoding, 2)
			
			#if that worked
			if (result[1]!="error")		{
				# try to read all lines
				result	<- helper_test_read(filePath, myEncoding, -1)
				
				#if that worked
				if (result[1]!="error") 			{
					#  Check here if firstline[1] contains "ooTextFile"
					#  check if the second line says textgrid
					if(length(grep("ooTextFile", result[1]))!=0) {
						if(length(grep("TextGrid", result[2]))!=0) {
							mytg <- result
							t@file.encoding 		<- myEncoding
							break()
						}
					}
				}
			}
		}
	}
	if (!is.null(fileContent)) {
		mytg <- fileContent 
		t@file.encoding <-"UTF8"
	}
	
	if(is.null(mytg)) 	{
		t@import.result  <- "error"
		t@load.message   <- "File not recognized as TextGrid."
		return(t)
	} else {
		if (!any(!is.na(stringr::str_match(mytg, pattern="ooTextFile")))) {
			t@import.result    <- "error"
			t@load.message   <- "File not recognized as TextGrid."
			return(t)
		}
	}
	if(getOption("act.import.storeFileContentInTranscript", default=TRUE)) {
		t@file.content <- mytg
	}
	
	#=== merge lines into a long text
	mytg.merge <- stringr::str_c(mytg, collapse = "\n")
	#remove a strange unicode character that occurred in one of the TextGrids
	mytg.merge <- stringr::str_replace_all(mytg.merge, pattern='\\x1B', replacement='')
	
	#===set transcript length
	rexeg_alltimes <- '((?:xmin|number)\\s=\\D*)([\\d\\.]*)(?:(?:[\\r\\n\\s]*xmax\\s=\\D*)([\\d\\.]*))'
	alltimes <- stringr::str_match_all(mytg.merge, rexeg_alltimes)
	t@length <- max(as.double(alltimes[[1]][,4]))
	
	#== extract tier info
	regex_tierinfo <- '(?<!Object\\s)(?:class\\s=\\s")(.+?)(?s:\\".*?name\\s=\\s")(.*?)(?s:\\".*?xmin\\s=)(.*\\d)(?s:.*?xmax\\s=)(.*\\d)(?s:.*?(?:intervals|points):\\ssize\\s=)(.*\\d)'
	tierinfo <- stringr::str_match_all(mytg.merge, regex_tierinfo)
	tierinfo <- do.call(rbind, lapply(tierinfo, data.frame, stringsAsFactors=FALSE))
	colnames(tierinfo) <- c("none","type","tier.name", "xmin","xmax","size")
	tierinfo <- tierinfo[,c("type","tier.name","xmin","xmax","size")]
	
	tierinfo$xmin <- as.double(tierinfo$xmin)
	tierinfo$xmax <- as.double(tierinfo$xmax)
	tierinfo$size <- as.integer(tierinfo$size)
	
	if (nrow(tierinfo)==0)  	{
		t@annotations  <- .emptyAnnotations
		t@tiers        <- .emptyTiers
	} else {
		#---create unique tierNames
		if (length(tierinfo$tier.name[duplicated(tierinfo$tier.name)])>0) {
			tierinfo$tier.name <- make.unique(tierinfo$tier.name)
			t@import.result 		<- "ok"
			t@load.message   <- "Some tiers have been renamed since their names were not unique."
		}
		alltierNames <- rep(tierinfo$tier.name, tierinfo$size)
		
		#== extract info
		regex_main <- '(?:(?:intervals|points)\\s*\\[)(.*\\d)(?:\\]:*[\\r\\n\\s]*(?:xmin|number|time)\\s=)(.*\\d)(?:(?:[\\r\\n\\s]*xmax\\s=)(.*\\d)){0,1}(?:[\\r\\n\\s]*(?:text|mark)\\s=\\s")((.|\\r|\\n)*?)(?="[\\r\\n\\s]*(?:item\\s*\\[\\d|intervals\\s*\\[\\d|points\\s*\\[\\d|$))'
		tiercontent <- stringr::str_match_all(mytg.merge, regex_main)

		#bind all rows together and rename columns
		tiercontent <- do.call(rbind, lapply(tiercontent, data.frame, stringsAsFactors=FALSE))
		colnames(tiercontent) <- c("none1","intervalnr","startSec","endSec", "content","none6")
		
		#replace double "" from praat TextGrids
		tiercontent$content <- stringr::str_replace_all(tiercontent$content, "\"\"", "\"")
		
		#check if actual and calculated values are the same
		if(	length(alltierNames)!=nrow(tiercontent) ) 	{
			t@import.result 		<- "Error"
			t@load.message   <- "Unkown error."
			return(t)
		}
		
		if (nrow(tiercontent)==0)  	{
			t@annotations  <- .emptyAnnotations
			t@tiers		   <- .emptyTiers
		} else {
			annotationID <- c(1:nrow(tiercontent))
			t@annotations <- data.frame(
				annotationID = as.integer(annotationID),
				
				tier.name = alltierNames,
				startSec  = round(as.double(tiercontent$startSec),15),
				endSec    = round(as.double(tiercontent$endSec),15),
				content   = as.character(tiercontent$content),
				
				content.norm            = as.character(""),
				char.orig.bytime.start 	= rep(as.integer(NA),length(annotationID)),
				char.orig.bytime.end	= rep(as.integer(NA),length(annotationID)),
				char.norm.bytime.start	= rep(as.integer(NA),length(annotationID)),
				char.norm.bytime.end	= rep(as.integer(NA),length(annotationID)),
				char.orig.bytier.start 	= rep(as.integer(NA),length(annotationID)),
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
				
				#=== set endSec of points to startSec
				t@annotations$endSec[is.na(t@annotations$endSec)] <- t@annotations$startSec[is.na(t@annotations$endSec)]
				
				#=== set annotations.id again
				t@annotations$annotationID <- c(1:nrow(t@annotations))
				
				#=== set the new row names
				rownames(t@annotations) <- t@annotations$annotationID
			}
		}
		
		#=== tiers to object
		t@tiers <- act::helper_tiers_new_table(tierNames=tierinfo$tier.name, tierTypes=tierinfo$type)
	}
	
	t@history <- list( 
						 list(modification                               = "import_textgrid",
						 	 systime                                       = Sys.time()
						 )
	)
	return(t)
}
