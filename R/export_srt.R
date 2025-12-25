#' Export .srt subtitle file
#' 
#' Advice: In most situations it is more convenient to use \code{act::corpus_export} for exporting annotation files.
#'
#' Creates a 'Subrip title' .srt subtitle file.
#' It will be written to the file specified in \code{pathOutput}.
#' If \code{pathOutput} is left empty, the function will return the contents of the .srt itself.
#' 
#' @param t Transcript object; transcript to be saved.
#' @param pathOutput Character string; path where .srt will be saved.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param filterSectionStartsec Double; start of selection in seconds.
#' @param filterSectionEndsec Double; end of selection in seconds.
#' @param speakerShow Logical; if \code{TRUE} name of speaker will be shown before the content of the annotation.
#' @param speakerWidth Integer; width of speaker abbreviation, -1 for full name without shortening.
#' @param speakerEnding Character string; string that is added at the end of the speaker name.
#' 
#' @return Contents of the .srt file (only if \code{pathOutput} is left empty)
#' @export
#' 
#' @seealso \code{corpus_export}, \link{export_eaf}, \link{export_exb}, \link{export_txt}, \link{export_docx}, \link{export_rpraat}, \link{export_textgrid}  
#'
#' @example inst/examples/export_srt.R
#' 
export_srt <- function(t, 
					   pathOutput            = NULL,
					   filterTierNames       = NULL,
					   filterSectionStartsec = NULL,
					   filterSectionEndsec   = NULL, 
					   speakerShow          = TRUE, 
					   speakerWidth         = 3,
					   speakerEnding        = ":") {
	
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	}	else { if (!methods::is(t, "transcript")) 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	
	#--- check if output folder exists
	if (!is.null(pathOutput)) {
		if (!dir.exists(dirname(pathOutput))) {
			stop("Output folder does not exist. Modify parameter 'pathOutput'.")
		}
	}
	#=== Get data
	#--- Filter and cure transcript
	t <- act::transcripts_filter_single(t, filterTierNames=filterTierNames, filterSectionStartsec = filterSectionStartsec, filterSectionEndsec = filterSectionEndsec, sort="tier>startsec")
	t <- act::transcripts_cure_single(t, annotationsTimesReversed=TRUE, annotationsOverlap=TRUE, annotationsTimesBelowZero=TRUE, tiersMissing=FALSE, warning=TRUE)
	
	#--- get all times
	alltimes <- c(	t@annotations$startsec, t@annotations$endsec)
	alltimes <- alltimes[order(alltimes)]
	alltimes <- unique(alltimes)
	
	#--- function for formatting times
	hhmmsscomma <- function(time){
		paste(paste(
			formatC(time %/% (60*60) %% 24, width = 2, format = "d", flag = "0")
					,":", formatC(time %/% 60 %% 60, width = 2, format = "d", flag = "0")
					,":", formatC(time %% 60, width = 2, format = "d", flag = "0")
					, ",", substr(sprintf("%1.3f", round(time %% 1,3)),3,6)
					, sep='' 
					)
			)
	}
	
	#--- iterate all times
	text <-""
	counter <- 0
	for (i in 1:(length(alltimes)-1)) {
		#which annotations span this interval
		
		#starts before the end of the segment, ends after the start of the segment
		ids <- which(t@annotations$startsec<alltimes[i+1] & t@annotations$end > alltimes[i])
		
		#if content is not empty
		if (paste(t@annotations$content[ids],collapse='', sep='')!='') {
			
			#--- add new line if not the first entry
			if (text!="") {
				text <- paste(text,'\n', sep='')
			}
			
			#--- add block
			#counter
			counter <- counter+1
			text <- paste(text, counter, '\n', sep='')
			
			#times
			text <- paste(text, hhmmsscomma(alltimes[i]), ' --> ', hhmmsscomma(alltimes[i+1]), '\n', sep='')
			
			#content
			if (speakerShow) {
				tierNames  <- substr(t@annotations$tierName[ids], 1, speakerWidth)
				tierNames <- paste(tierNames, speakerEnding, sep='')
				text <- paste(text,paste(tierNames, ' ', t@annotations$content[ids], collapse='\n'),'\n', sep='')
			} else {
				text <- paste(text, paste(t@annotations$content[ids],collapse='\n'),'\n', sep='')
			}
			
		}
		
	}

	if (is.null(pathOutput)) {
		return(text)
	} else {
		#---write to file
		fileConn <- file(pathOutput)
		writeLines(text, fileConn)
		close(fileConn)		
	}
}

