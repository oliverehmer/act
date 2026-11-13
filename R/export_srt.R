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
	
	.assert_transcript(t, missing = missing(t))
	
	#--- check if output folder exists
	if (!is.null(pathOutput)) {
		if (!dir.exists(dirname(pathOutput))) {
			cli::cli_abort("Output folder does not exist. Modify parameter {.arg pathOutput}.")
		}
	}
	#=== Get data
	#--- Filter and cure transcript
	t <- act::transcripts_filter_single(t, filterTierNames=filterTierNames, filterSectionStartsec = filterSectionStartsec, filterSectionEndsec = filterSectionEndsec, sort="tier>startsec")
	t <- act::transcripts_cure_single(t, annotationsTimesReversed=TRUE, annotationsOverlap=TRUE, annotationsTimesBelowZero=TRUE, transcriptLengthZero=TRUE, annotationsZeroDuration=TRUE, tiersMissing=FALSE, warning=TRUE)
	
	#--- get all times
	alltimes <- c(	t@annotations$startsec, t@annotations$endsec)
	alltimes <- alltimes[order(alltimes)]
	alltimes <- unique(alltimes)
	
	#--- function for formatting times
	# from whole milliseconds: with the seconds cut off and the fraction rounded apart,
	# 12.9996 came out as 00:00:12,000 - a second early, and before its own start
	hhmmsscomma <- function(time){
		ms <- round(time * 1000)
		sprintf("%02d:%02d:%02d,%03d", as.integer(ms %/% 3600000 %% 24), as.integer(ms %/% 60000 %% 60), as.integer(ms %/% 1000 %% 60), as.integer(ms %% 1000))
	}
	
	#--- iterate all times
	text <-""
	counter <- 0
	for (i in 1:(length(alltimes)-1)) {
		#which annotations span this interval
		
		#starts before the end of the segment, ends after the start of the segment
		ids <- which(t@annotations$startsec<alltimes[i+1] & t@annotations$endsec > alltimes[i])
		
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
				tierNames  <- t@annotations$tierName[ids]
				if (speakerWidth >= 0) tierNames <- substr(tierNames, 1, speakerWidth)
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
		#---write to file as UTF-8
		fileConn <- file(pathOutput, open="wb")
		writeLines(enc2utf8(text), fileConn, sep="\n", useBytes=TRUE)
		close(fileConn)
	}
}

