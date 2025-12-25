#' Export .edl marker file for '"'Davinci Resolve'
#' 
#' Advice: In most situations it is more convenient to use \code{act::corpus_export} for exporting annotation files.
#'
#' Creates a 'event list text' .edl file with definitions for Blackmagic DaVinci Resolve
#' It will be written to the file specified in \code{pathOutput}.
#' If \code{pathOutput} is left empty, the function will return the contents of the .edl itself.
#' 
#' @param t Transcript object; transcript to be saved.
#' @param pathOutput Character string; path where .edl will be saved.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param filterSectionStartsec Double; start of selection in seconds.
#' @param filterSectionEndsec Double; end of selection in seconds.
#' @param fps Double; Frame rate per seconds of your project, e.g. 60, 50, 30, 29
#' 
#' @return Contents of the .edl file (only if \code{pathOutput} is left empty)
#' @export
#' 
#' @seealso \code{corpus_export}, \link{export_eaf}, \link{export_exb}, \link{export_srt}, \link{export_txt}, \link{export_docx}, \link{export_rpraat}, \link{export_textgrid}  
#'
#' @example inst/examples/export_srt.R
#' 
#' 
#'

export_edl <- function(t, 
					   pathOutput            = NULL,
					   filterTierNames       = NULL,
					   filterSectionStartsec = NULL,
					   filterSectionEndsec   = NULL, 
					   fps                   = 50) {
	if (1==3) {
	#	t  <- examplecorpus@transcripts[[i]]
	#	pathOutput            <- '/Users/oliverehmer/Desktop/test.edl' #NULL
	#	filterTierNames       <- NULL
	#	filterSectionStartsec <- NULL
	#	filterSectionEndsec   <- NULL 
	#	fps                   <- 50
	}
	
	resolveColors <- c("ResolveColorBlue",
					   "ResolveColorCyan",
					   "ResolveColorGreen",
					   "ResolveColorYellow",
					   "ResolveColorRed",
					   "ResolveColorPink",
					   "ResolveColorPurple",
					   "ResolveColorFuchsia",
					   "ResolveColorRose",
					   "ResolveColorLavender",
					   "ResolveColorSky",
					   "ResolveColorMint",
					   "ResolveColorLemon",
					   "ResolveColorSand",
					   "ResolveColorCocoa",
					   "ResolveColorCream")
	resolveColors <- rep(resolveColors,20)
	
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	}	else { if (!methods::is(t, "transcript")) 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	
	#--- check if output folder exists
	if (!is.null(pathOutput)) {
		if (!dir.exists(dirname(pathOutput))) {
			stop("Output folder does not exist. Modify parameter 'pathOutput'.")
		}
	}
	
	#=== Get data
	#--- Filter and cure transcript
	t <- act::transcripts_filter_single(t, 
										filterTierNames       = filterTierNames, 
										filterSectionStartsec = filterSectionStartsec, 
										filterSectionEndsec   = filterSectionEndsec, 
										sort="tier>startsec")
	
	if (nrow(t@annotations)==0) {
		return('')
	}
	
	t <- act::transcripts_cure_single(t, annotationsTimesReversed=TRUE, annotationsOverlap=TRUE, annotationsTimesBelowZero=TRUE, tiersMissing=FALSE, warning=TRUE)
	
	#--- function for formatting times
	hhmmssframes <- function(time, fps){
		paste(paste(
			formatC(time %/% (60*60) %% 24, width = 2, format = "d", flag = "0")                              #hh
					,":", formatC(time %/% 60 %% 60, width = 2, format = "d", flag = "0")                     #mm
					,":", formatC(time %% 60, width = 2, format = "d", flag = "0")                            #ss
			        ,":", formatC(as.integer(round((time %% 1) * fps)) , width = 2, format = "d", flag = "0") #frames
					#, ":", as.integer(round((time %% 1) * fps))                              #frames
					, sep='' 
					)
			)
	}
	
	#---- times formatted/as frames ----
	t@annotations$duration.frames <- as.integer(round((t@annotations$endsec - t@annotations$startsec)  * fps))
	t@annotations$start.formatted <- hhmmssframes(t@annotations$startsec,fps)  
	t@annotations$end.formatted   <- hhmmssframes(t@annotations$endsec,fps) 
	
	
	#error if tierNames is NULL
	if (is.null(filterTierNames)) {
		filterTierNames <- t@tiers$name
	}
	
	#filter tiers only if tier filter makes sense
	tiers <- t@tiers[t@tiers$name %in% filterTierNames,]

	if (nrow(tiers)>0 && nrow(t@annotations)>0) {
		#---- colors ----
		t@tiers$color <- resolveColors[1:nrow(tiers)]
		
		#---- data ----
		#merge data frames
		ann <- merge(t@annotations, t@tiers, by.x="tierName", by.y="name", all.x="TRUE")
		#replace | and linebreaks
		ann$content<- stringr::str_replace_all(ann$content, "[|\n]", "_")
		
		#---- tiers ----
		#set duration of point tiers to 1 frame
		ann$duration.frames[ann$type=='TextTier'] <-1
		
		#---- IDs ----
		ann$id <- seq(1:nrow(ann))
		ann$id <- sprintf("%03d", ann$id)		
		
		#---- build the list ---
		example<-c()
		example[1] <- '001  001      V     C        01:00:00:04 01:00:00:05 01:00:00:04 01:00:00:05'  
		example[2]<- 'notiz |C:ResolveColorBlue |M:Marker 1 |D:1'
		
		#template command
		cmd<-c()
		cmd[1] <- "{id}  001      V     C        {start.formatted} {end.formatted} {start.formatted} {end.formatted}  \n"
		cmd[2] <- "{tierName} |C:{color} |M:{content} |D:{duration.frames}\n\n\n"
		cmd<-paste0(cmd, collapse='')
		
		#glue
		library(glue)
		text <- glue::glue_data(ann, cmd)
		text <- paste0(text, collapse='')
		
		#join with header
		header <- sprintf('TITLE: %s\nFCM: NON-DROP FRAME\n\n', t@name)
		
		text <- paste0(header, text)
		
		#cat(text)
		#print(text)
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

