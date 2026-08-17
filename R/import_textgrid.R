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
#' @param verbose Logical; if \code{TRUE} warnings about encoding issues are printed to the console. If \code{FALSE} messages are only stored in \code{t@load.message}. Default is \code{TRUE}.
#'
#' @return Transcript object.
#'
#' @seealso \link{corpus_import}, \link{corpus_new}, \link{import}, \link{import_eaf}, \link{import_exb}, \link{import_rpraat}
#'
#' @export
#'
#' @example inst/examples/import_textgrid.R
#'
import_textgrid <- function(filePath=NULL,
							fileContent=NULL,
							transcriptName=NULL,
							verbose=TRUE) {
	
	#filePath<-	'/Users/oliverehmer/Desktop/Mary_John_bell.TextGrid'
	
	if (is.null(filePath) & is.null(fileContent)) {
		cli::cli_abort("You need to pass as parameter eiter a file path to a TextGrid file (filePath) or the contents of a TextGrid file (fileContent) as parameter.")
	}
	if (!is.null(filePath) & !is.null(fileContent)) {
		cli::cli_abort("Please pass only filePath or fileContent as parameter, not both.")
	}
	if (!is.null(fileContent) & is.null(transcriptName)) {
		cli::cli_abort("If you pass {.arg fileContent} you need to pass {.arg transcriptName} as parameter, too.")
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

	mytg <- NULL
	t@file.encoding <- "unknown"

	if (!is.null(filePath)) {
		#--- check if file exists
		if (!file.exists(filePath)) {
			t@import.result    <- "error"
			t@load.message   <- "File does not exist."
			return(t)
		}

		#--- read file via robust encoding detection
		read_result <- helper_read_annotation_file(
			filePath       = filePath,
			expectedHeader = c('File type = "ooTextFile"',
							   'Object class = "TextGrid"'),
			fileType       = "textgrid",
			verbose        = verbose
		)
		if (read_result$status != "ok") {
			t@import.result <- "error"
			t@load.message  <- if (nzchar(read_result$message))
								read_result$message
							   else
								"File not recognized as TextGrid."
			return(t)
		}
		mytg <- read_result$lines
		t@file.encoding <- read_result$encoding_detected
	}
	if (!is.null(fileContent)) {
		mytg <- fileContent
		t@file.encoding <-"UTF8"
	}

	if(is.null(mytg)) 	{
		t@import.result  <- "error"
		t@load.message   <- "File not recognized as TextGrid."
		return(t)
	}
	if(getOption("act.import.storefileContentInTranscript", default=TRUE)) {
		t@file.content <- mytg
	}
	
	#=== merge lines into a long text
	mytg.merge <- stringr::str_c(mytg, collapse = "\n")
	#remove a strange unicode character that occurred in one of the TextGrids
	mytg.merge <- stringr::str_replace_all(mytg.merge, pattern='\\x1B', replacement='')
	
	#===set transcript length
	rexeg_alltimes <- '((?:xmin|number)\\s=\\D*)([\\d\\.]*)(?:(?:[\\r\\n\\s]*xmax\\s=\\D*)([\\d\\.]*))'
	alltimes <- stringr::str_match_all(mytg.merge, rexeg_alltimes)
	t@length.sec <- max(as.double(alltimes[[1]][,4]))
	
	#== extract tier info
	regex_tierinfo <- '(?<!Object\\s)(?:class\\s=\\s")(.+?)(?s:\\".*?name\\s=\\s")(.*?)(?s:\\".*?xmin\\s=)(.*\\d)(?s:.*?xmax\\s=)(.*\\d)(?s:.*?(?:intervals|points):\\ssize\\s=)(.*\\d)'
	tierinfo <- stringr::str_match_all(mytg.merge, regex_tierinfo)
	tierinfo <- do.call(rbind, lapply(tierinfo, data.frame, stringsAsFactors=FALSE))
	colnames(tierinfo) <- c("none","type","tierName", "xmin","xmax","size")
	tierinfo <- tierinfo[,c("type","tierName","xmin","xmax","size")]
	
	tierinfo$xmin <- as.double(tierinfo$xmin)
	tierinfo$xmax <- as.double(tierinfo$xmax)
	tierinfo$size <- as.integer(tierinfo$size)
	
	if (nrow(tierinfo)==0)  	{
		t@annotations  <- .emptyAnnotations
		t@tiers        <- .emptyTiers
	} else {
		#---create unique tierNames
		if (length(tierinfo$tierName[duplicated(tierinfo$tierName)])>0) {
			renamed_tiers <- unique(tierinfo$tierName[duplicated(tierinfo$tierName)])
			tierinfo$tierName <- make.unique(tierinfo$tierName)
			t@import.result 		<- "ok"
			t@load.message   <- paste0("Some tiers have been renamed since their names were not unique: ", paste(renamed_tiers, collapse=", "))
		}
		alltierNames <- rep(tierinfo$tierName, tierinfo$size)
		
		#== extract info
		regex_main <- '(?:(?:intervals|points)\\s*\\[)(.*\\d)(?:\\]:*[\\r\\n\\s]*(?:xmin|number|time)\\s=)(.*\\d)(?:(?:[\\r\\n\\s]*xmax\\s=)(.*\\d)){0,1}(?:[\\r\\n\\s]*(?:text|mark)\\s=\\s")((.|\\r|\\n)*?)(?="[\\r\\n\\s]*(?:item\\s*\\[\\d|intervals\\s*\\[\\d|points\\s*\\[\\d|$))'
		tiercontent <- stringr::str_match_all(mytg.merge, regex_main)

		#bind all rows together and rename columns
		tiercontent <- do.call(rbind, lapply(tiercontent, data.frame, stringsAsFactors=FALSE))
		colnames(tiercontent) <- c("none1","intervalnr","startsec","endsec", "content","none6")
		
		#replace double "" from praat TextGrids
		tiercontent$content <- stringr::str_replace_all(tiercontent$content, "\"\"", "\"")
		tiercontent$content <- .strip_invalid_xml_chars(tiercontent$content)
		tiercontent$content <- .replace_newlines(tiercontent$content)
		
		#check if actual and calculated values are the same
		if(	length(alltierNames)!=nrow(tiercontent) ) 	{
			t@import.result  <- "Error"
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
				
				tierName = alltierNames,
				startsec  = round(as.double(tiercontent$startsec),15),
				endsec    = round(as.double(tiercontent$endsec),15),
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
			t@annotations$startsec		<- as.double(t@annotations$startsec)
			t@annotations$endsec  		<- as.double(t@annotations$endsec)
			t@annotations$content  		<- as.character(t@annotations$content)
			
			#=== get rid of empty intervals
			if (options()$act.import.readEmptyIntervals==FALSE) 		{
				t@annotations <- t@annotations[t@annotations$content!="",]
			}
			t@annotations <- t@annotations[is.na(t@annotations["content"])==FALSE,]
			
			if (nrow(t@annotations)>0) 		{
				#=== sort transcript by start times
				t@annotations <- t@annotations[order(t@annotations$startsec, t@annotations$tierName), ]
				
				#=== set endsec of points to startsec
				t@annotations$endsec[is.na(t@annotations$endsec)] <- t@annotations$startsec[is.na(t@annotations$endsec)]
				
				#=== set annotations.id again
				t@annotations$annotationID <- c(1:nrow(t@annotations))
				
				#=== set the new row names
				rownames(t@annotations) <- t@annotations$annotationID
			}
		}
		
		#=== tiers to object
		t@tiers <- act::helper_tiers_new_table(tierNames=tierinfo$tierName, tierTypes=tierinfo$type)
	}
	
	t@history <- list( 
						 list(modification                               = "import_textgrid",
						 	 systime                                       = Sys.time()
						 )
	)
	return(t)
}
