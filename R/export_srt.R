#' Export a transcript object to a .srt subtitle file
#' 
#' Advice: In most situations it is more convenient to use \code{act::corpus_export} for exporting annotation files.
#'
#' Creates a 'Subrip title' .srt subtitle file.
#' It will be written to the file specified in \code{outputPath}.
#' If \code{outputPath} is left empty, the function will return the contents of the .srt itself.
#' 
#' @param t Transcript object; transcript to be saved.
#' @param outputPath Character string; path where .srt will be saved.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param filterSectionStartsec Double; start of selection in seconds.
#' @param filterSectionEndsec Double; end of selection in seconds.
#' @param speaker.show Logical; if \code{TRUE} name of speaker will be shown before the content of the annotation.
#' @param speaker.width Integer; width of speaker abbreviation, -1 for full name without shortening.
#' @param speaker.ending Character string; string that is added at the end of the speaker name.
#' 
#' @return Contents of the .srt file (only if \code{outputPath} is left empty)
#' @export
#' 
#' @seealso \code{corpus_export}, \code{export_eaf}, \code{export_exb}, \code{export_printtranscript}, \code{export_rpraat}, \code{export_textgrid}  
#'
#' @example inst/examples/export_srt.R
#' 
export_srt <- function(t, 
					   outputPath=NULL,
					   filterTierNames=NULL,
					   filterSectionStartsec = NULL,
					   filterSectionEndsec = NULL, 
					   speaker.show=TRUE, 
					   speaker.width=3,
					   speaker.ending=":") {
	
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	} else { if (class(t)[[1]]!="transcript") 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	
	#--- check if output folder exists
	if (!is.null(outputPath)) {
		if (!dir.exists(dirname(outputPath))) {
			stop("Output folder does not exist. Modify parameter 'outputPath'.")
		}
	}
	#=== Get data
	#--- Filter and cure transcript
	t <- act::transcripts_filter_single(t, filterTierNames=filterTierNames, filterSectionStartsec = filterSectionStartsec, filterSectionEndsec = filterSectionEndsec, sort="tier>startSec")
	t <- act::transcripts_cure_single(t, annotationsWithReversedTimes=TRUE, overlappingAnnotations=TRUE, annotationsWithTimesBelowZero=TRUE, missingTiers=FALSE, showWarning=TRUE)
	
	#--- get all times
	alltimes <- c(	t@annotations$startSec, t@annotations$endSec)
	alltimes <- alltimes[order(alltimes)]
	alltimes <- unique(alltimes)
	
	#--- function for formatting times
	dhms <- function(time){
		paste(paste(formatC(time %/% (60*60) %% 24, width = 2, format = "d", flag = "0")
					,":", formatC(time %/% 60 %% 60, width = 2, format = "d", flag = "0")
					,":", formatC(time %% 60, width = 2, format = "d", flag = "0")
					, ",", substr(as.character(round(time %% 1,3)),3,6)
					,sep='' 
		)
		)
	}
	
	#--- iterate all times
	text <-""
	counter <- 0
	for (i in 1:(length(alltimes)-1)) {
		#which annotations span this interval
		
		#starts before the end of the segment, ends after the start of the segment
		ids <- which(t@annotations$startSec<alltimes[i+1] & t@annotations$end > alltimes[i])
		
		#if content is not empty
		if (paste(t@annotations$content[ids],collapse='', sep='')!='') {
			
			#--- add new line if not the first entry
			if (text!="") {
				text <- paste(text,'\n')
			}
			
			#--- add block
			#counter
			counter <- counter+1
			text <- paste(text, counter, '\n', sep='')
			
			#times
			text <- paste(text, dhms(alltimes[i]), ' --> ', dhms(alltimes[i+1]), '\n', sep='')
			
			#content
			if (speaker.show) {
				tiernames  <- substr(t@annotations$tier.name[ids], 1, speaker.width)
				tiernames <- paste(tiernames, speaker.ending, sep='')
				text <- paste(text,paste(tiernames, ' ', t@annotations$content[ids], collapse='\n'),'\n', sep='')
			} else {
				text <- paste(text,paste(t@annotations$content[ids],collapse='\n'),'\n', sep='')
			}
			
		}
		
	}

	if (is.null(outputPath)) {
		return(text)
	} else {
		#---write to file
		fileConn <- file(outputPath)
		writeLines(text, fileConn)
		close(fileConn)		
	}
}

