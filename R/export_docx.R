#' Export print transcript in .docx format
#' 
#' LAYOUT
#‘ If you want to modify the layout of the print transcripts, create a new layout object with \code{mylayout <- methods::new("layout")}, modify the settings and pass it as argument \code{l}.
#' Using the layout object you may
#' - Adjust with, abbreviation of speakers, etc.
#' - set filters to include/exclude tiers matching regular expressions.
#' – assign template files for .docx formatting using format templates
#'
#' FORMATING
# To format the transcript you can 
#' - adjust the the defaults format templates in the default .docx template.
#' - define further templates and add them to a styles matrix.
#' The paths to both files need to be set in your l layout object. Please check the slots l@docx.template.path and l@docx.styles.
#' You can see the structure of the default styles matrix in each new layout object in l@ l@docx.styles. Use the l@docx.styles<-act:: export_docx_styles_load(...) to assign a custom styles matrix from a csv files.
#' The default format templates are
#' * Header: 
#' - header.preface (formats: s@results$header.description) 
#' - header.title (formats: s@results$header.description) 
#' - header.subtitle (formats: s@results$header.description) 
#' - header.description (formats: s@results$header.description) 
#' * Transcript body
#' - body.default (formats: any annotation in „t@annotations“
#' 
#' * LAYERS
#' In addition to the original transcript content in transcript@annotations$content you can output further layers. 
#' The layers need to be assigned to the data.frame transcript@annotations as new columns. To output the layers, pass the name(s) of the repective column(s) as character vector in the parameter layerNames.
#' 			   
#' @param t Transcript object.
#' @param l Layout object.
#' @param pathOutput Character string; path where to save the transcript.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param filterSectionStartsec Double; start of selection in seconds.
#' @param filterSectionEndsec Double; end of selection in seconds.
#' @param insertArrowAnnotationID Integer; ID of the annotation in front of which the arrow will be placed.
#' @param headerPreface Character string; text used as preface before title.
#' @param headerTitle Character string; text used as title.
#' @param headerSubtitle Character string; text  used as sub title.
#' @param headerDescription Character string; text used as description after sub title.
#' @param headerInsertSource Logical; if \code{TRUE} standard information about the source and location of the sequence will be inserted after the heading.
#' @param layerNames Vector of character strings; Names of columns present in 't@annotations' to be exported (in addition to column content').

#' 
#' @return Officer doc; transcript as object from library officer.
#' 
#' @seealso \link{corpus_export}, \link{export_eaf}, \link{export_exb}, \link{export_rpraat}, \link{export_srt}, \link{export_textgrid} 
#' 
#' @export
#'
#' @example inst/examples/export_docx.R
#' 
export_docx <- function (   t, 
							l                            = NULL, 
							pathOutput                   = NULL,
							filterTierNames              = NULL, 
							filterSectionStartsec        = NULL, 
							filterSectionEndsec          = NULL,  
							insertArrowAnnotationID      = "", 
							headerPreface                = NULL, 
							headerTitle                  = NULL, 
							headerSubtitle               = NULL,
							headerDescription            = NULL,
							headerInsertSource           = TRUE,
							layerNames                   = NULL
) {
	# 
	
	if (1==2) {
	#	#t <- corpus@transcripts[[1]]
	#	i<-1
	#	t <- corpus@transcripts[[s@results$transcriptName[i ]]]
	#	l <- l
	#	pathOutput             <- '/Users/oliverehmer/Downloads/text.docx'
	#	filterTierNames        <- NULL
	#	filterSectionStartsec  <- s@results$startsec[i]
	#	filterSectionEndsec    <- s@results$endsec[i]
	#	insertArrowAnnotationID<- s@results$annotationID[i]
	#	headerTitle            <- s@results$header.title[i]
	#	headerSubtitle         <- s@results$header.subtitle[i]
	#	headerInsertSource     <- FALSE
	#	layerNames             <- c("transcript.translation", "transcript.space")
	}
	
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	}	else { if (!methods::is(t, "transcript")) 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	if (missing(l)) 	{
		l <- methods::new("layout")
		l@docx.template.path <- "" 
	} else {
		if (is.null(l)) 	{
			l <- methods::new("layout")
			l@docx.template.path <- "" 
		}
	}
	
	# Required packages
	if (!requireNamespace("officer", quietly = TRUE)) stop("Please install the 'officer' package.")
	
	# === Define template and style variables
	if (l@docx.template.path=="") {
		inputTemplate <- system.file("extdata", "docx", "template_transcript.docx", package="act")
		if (!file.exists(inputTemplate)) {
			stop("Unable to find 'template_transcript.docx'. Please install the act package again.")
		} 
	} else {
		inputTemplate <- l@docx.template.path
		if (!file.exists(inputTemplate)) {
			stop("File defined in 'l@docx.template.path' not found.")
		}
	}
	
	# === Input validation
	#transcript
	if (missing(t)) 	{stop("ERROR: Transcript object in parameter 't' is missing.") 	}	else { if (!methods::is(t, "transcript")) 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	#layout
	if (is.null(l)) 	{
		l <- methods::new("layout")
	}
	#layers: check if all layers to be shown are defined
	if (!is.null(layerNames)) {
		if (any(is.na(layerNames))) {
			stop("At least one of your layers in 'layerNames' is NA.")
		} else {
			missingLayers <- setdiff(layerNames, colnames(t@annotations))
			if (length(missingLayers)>0) {
				stop(paste0("Not all layers defined 'layerNames' are present as columns in data.frame 't@annotations'. Missing: ", paste0(missingLayers, collapse=" ")))
			}
		}
	}
	
	#folder output: check if output folder exists
	if (!is.null(pathOutput)) {
		if (!dir.exists(dirname(pathOutput))) {
			stop("Output folder does not exist. Modify parameter 'pathOutput'.")
		}
	}
	
	#==== CHECK user inputs ====
	#--- exit if transcript width is too short or too long
	if (l@transcript.width == -1) {
	} else if (l@transcript.width < 40) {
		return( "ERROR: The width of the transcript is to low. Minimum is 40. Check option 'l@transcript.width'")
	}
	
	#--- exit if tier length is too short or too long
	if (l@speaker.width == -1) {
	} else if (l@speaker.width==0) {
		return( "ERROR: Length of tier names is to short. Minimum is 1. Check option 'l@speaker.width'.")
	} else if (l@speaker.width < -1) {
		return("ERROR: Length of tier names is to short. Minimum is 1. Check option 'l@speaker.width'.")
	} else if (l@speaker.width > 25) {
		return("ERROR: Length of tier names is to long. Maximum is 25. Check option 'l@speaker.width'.")
	}
	
	#--- increase
	options(act.transcript.spacesbefore= max(0, l@spacesbefore))
	
	#==== GET data ====
	#---- . tier names ----
	if (is.null(filterTierNames)) {
		filterTierNames<- t@tiers$name
	}
	filterTierNames <- helper_tiers_filter_create(tierNames=filterTierNames, 
												  filterTierIncludeRegEx=l@filter.tier.includeRegEx,
												  filterTierExcludeRegEx=l@filter.tier.excludeRegEx)
	
	#---- . filter ----
	t <- act::transcripts_filter_single(t, 
										filterTierNames       = filterTierNames, 
										filterSectionStartsec = filterSectionStartsec, 
										filterSectionEndsec   = filterSectionEndsec)
	
	#---- . cure ----
	t <- act::transcripts_cure_single(t, 
									  annotationsTimesReversed=TRUE, 
									  annotationsOverlap=TRUE, 
									  annotationsTimesBelowZero=FALSE, 
									  tiersMissing=FALSE, 
									  warning=TRUE)
	
	#---- . annotations  ----
	ann <- t@annotations
	#exit ?
	if (nrow(ann)==0) { 	return("No annotations found") }
	
	#check if all relevant cols are present 
	myCols <- c("tierName", "startsec","endsec","content")
	if (!all(myCols %in% colnames(ann))) {
		return(paste("ERROR: Missing columns. Annotations needs to contain: ", paste(myCols, collapse = " ", sep="")))
	}
	
	#add annotationID if not present
	#if ("annotationID" %in% colnames(ann)) {
	#	ann <- ann[, c("annotationID", myCols)]
	#} else {
	#		ann <- cbind(annotationID=NA, ann[,myCols])
	#}
	if (!"annotationID" %in% colnames(ann)) {
		ann$annotationID<-NA
	}
	#convert col tierName to character
	ann$tierName <- as.character(ann$tierName)
	
	#exit ?
	if (nrow(ann)==0) { 	return("No annotations found") }
	
	#sort annotations by start time
	ann <- ann[order(ann$startsec), ]
	
	#==== PREFIXES ====
	#---- . spaces before ----
	ann$spacebefore <- stringr::str_pad("", width=l@spacesbefore, side="left", pad=" ")
	#insert arrow
	if (!is.na(insertArrowAnnotationID)) {
		if (length(which(ann$annotationID==insertArrowAnnotationID))>0) {
			ann$spacebefore[which(ann$annotationID==insertArrowAnnotationID)[1]] <- stringr::str_pad(l@arrow.shape, width=l@spacesbefore, side="right", pad=" ")
		}
	}
	
	#---- . line numbers ----
	line_numbers <- as.character(1:nrow(ann))
	#add 0 for 1-9
	line_numbers[1:min(length(line_numbers),9)] <- stringr::str_pad(line_numbers[1:min(length(line_numbers),9)], width=2, side="left", pad="0")
	#set line numbers
	ann$line <- line_numbers
	
	#---- . speaker ----
	#get tier names
	tierNames 				<- as.character(unique(ann$tierName))
	
	#width of speakers calculate width of the speaker abbreviations (without end character)
	text_body_width_speaker <- max(nchar(as.character(tierNames)))
	if (!is.na( l@speaker.width)) {
		#if full name : stay with maximum	
		if (l@speaker.width == -1) {
		} else {
			#otherwise set value
			text_body_width_speaker <- l@speaker.width
		}
	}

	
	#==== TEXT PREP ====
	#---- . gat formatting speaker acronyms ----
	#take the tier name
	ann$speaker <- ann$tierName
	
	#TRP pause 1: temporarily insert Identifier
	trppauses_pos <- stringr::str_detect(ann$content, options()$act.pauseIdentifierGATRegEx)
	if (length(trppauses_pos) > 0) {
		ann$speaker[trppauses_pos] <- "%TRPPAUSE%$(§($&§%/%"
	}
	
	#calculate if same speaker as before
	ann_speaker_previous <- c("", ann$speaker[1:length(ann$speaker)-1] )
	sameSpeaker_pos <- ann_speaker_previous==ann$speaker
	
	#set same speakers as before to ""
	ann$speaker[sameSpeaker_pos] <- ""
	included_speakers_pos <- !sameSpeaker_pos
	
	#TRP pause 2: 
	if (length(trppauses_pos) > 0) {
		#remove temporary insert
		ann$speaker[trppauses_pos] <- ""
		#set to false
		included_speakers_pos[which(trppauses_pos)] <- FALSE
	}
	
	#all other tiers extract using regex
	if (!is.na(l@speaker.regex)) {
		ann$speaker[included_speakers_pos] <- stringr::str_extract(ann$speaker[included_speakers_pos], l@speaker.regex)
	}
	
	#cut to correct length and add end sign
	ann$speaker[included_speakers_pos] <- paste(substr(ann$speaker[included_speakers_pos],1, text_body_width_speaker), l@speaker.ending, sep="")
	
	#add missing spaces
	ann$speaker <- stringr::str_pad(ann$speaker, width=text_body_width_speaker+nchar(l@speaker.ending), side="right", pad=" ")
	
	#---- . main variables ----
	text_exdent       <- l@spacesbefore + text_body_width_speaker + nchar(l@speaker.ending) + 3
	text_body_width   <- l@transcript.width - text_exdent
	text_all          <- c()	
	ann               <- ann[order(ann$startsec), ]
	ann$text          <- stringr::str_trim(ann$content)
	
	#---- . brackets ----
	ann$firstBracketIsAligned         <- FALSE
	ann$bracketAlignedWithAnnotationI <- 0
	#i<-6
	#ann$text[i]
	if (l@brackets.align 		== TRUE) {
		#each annotation
		for (i in 1:length(ann$text)) {
			
			#check if the first bracket is already aligned
			if (ann$firstBracketIsAligned[i]) {
				startWithBracketI <- 2					
			} else { 
				startWithBracketI <- 1	
			}
			
			#number of opening brackets
			openingBracketsI.nr 	<- 	stringr::str_count(ann$text[i], "\\[")
			
			#all brackets in an annotation : check if there are more brackets to be aligned
			if (startWithBracketI<=openingBracketsI.nr) {
				
				#search correspondence for each opening bracket
				x<-1
				for (x in startWithBracketI:openingBracketsI.nr) {
					
					#exit if next record set would be outside the data frame 
					startSearching <- i+1
					if (startSearching>length(ann$text)) {
						break
					}
					
					#start: process all annotations FOLLOWING the current annotation i
					j<-startSearching
					for (j in startSearching:length(ann$text)) {
						
						#exit: if this annotation does not temporally overlap
						if (ann$startsec[j]> ann$endsec[i]) {
							break
						} 
						
						#skip annotation : if it has already been moved/aligned
						if (!ann$firstBracketIsAligned[j]) {
							openingBracketsJ.nr <- 		stringr::str_count(ann$text[j], "\\[")
							
							#if there are opening brackets that can still be aligned
							if (openingBracketsJ.nr>0 ) {
								#get positions and contents of brackets i
								bracket.i <- data.frame(stringr::str_locate_all(ann$text[i], "\\[.*?\\]"), stringsAsFactors = FALSE)
								bracket.i <- bracket.i[x, ]
								bracket.i <- cbind(bracket.i, content=as.character(unlist(stringr::str_extract_all(ann$text[i], "\\[.*?\\]"))[x]), row.names = NULL) #mind the x
								bracket.i <- cbind(bracket.i, bracketLength=stringr::str_length(bracket.i$content) , row.names = NULL)
								bracket.i <- cbind(bracket.i, before=stringr::str_trim(stringr::str_sub(ann$text[i], bracket.i$start-1, bracket.i$start-1 )), row.names = NULL)
								bracket.i <- cbind(bracket.i, after=stringr::str_trim(stringr::str_sub(ann$text[i], bracket.i$end+1, bracket.i$end+1 )), row.names = NULL)	
								bracket.i <- cbind(bracket.i, 
												   posSpaceInside=data.frame(
												   	stringi::stri_locate(as.character(bracket.i$content), regex=" ", mode="last"), 
												   	stringsAsFactors= FALSE)$start, 
												   row.names = NULL)
								
								
								#something went wrong, probably missing closing bracket
								if (is.null(bracket.i$bracketLength)) {		break}
								if (is.na(bracket.i$bracketLength)) {		break}
								
								
								#get positions and contents of brackets j
								bracket.j <- data.frame(stringr::str_locate_all(ann$text[j], "\\[.*?\\]"), stringsAsFactors = FALSE)
								bracket.j <- bracket.j[1, ]
								bracket.j <- cbind(bracket.j, content        = as.character(unlist(stringr::str_extract_all(ann$text[j], "\\[.*?\\]"))[1]), row.names = NULL) #mind the 1
								bracket.j <- cbind(bracket.j, bracketLength  = stringr::str_length(bracket.j$content) , row.names = NULL)
								bracket.j <- cbind(bracket.j, before         = stringr::str_trim(stringr::str_sub(ann$text[j], bracket.j$start-1, bracket.j$start-1 )), row.names = NULL)
								bracket.j <- cbind(bracket.j, after          = stringr::str_trim(stringr::str_sub(ann$text[j], bracket.j$end+1, bracket.j$end+1 )), row.names = NULL) #mind the 1
								bracket.j <- cbind(bracket.j, posSpaceInside = data.frame(stringi::stri_locate(as.character(bracket.j$content), regex=" ", mode="last"), stringsAsFactors= FALSE)$start, row.names = NULL)
								
								#something went wrong, probably missing closing bracket
								if (is.null(bracket.j$bracketLength)) {		break}
								if (is.na(bracket.j$bracketLength)) {		break}
								
								
								#--- put start of annotation j in the right place 
								ann$text[j] <- stringr::str_flatten(c(strrep(" ", abs(bracket.i$start-bracket.j$start)), ann$text[j]) , collapse="")
								ann$firstBracketIsAligned[j] <- TRUE
								
								#get positions and contents of brackets j AGAIN, because overal positino changed
								bracket.j <- data.frame(stringr::str_locate_all(ann$text[j], "\\[.*?\\]"), stringsAsFactors = FALSE)
								bracket.j <- bracket.j[1, ]
								bracket.j <- cbind(bracket.j, content        = as.character(unlist(stringr::str_extract_all(ann$text[j], "\\[.*?\\]"))[1]), row.names = NULL) #mind the 1
								bracket.j <- cbind(bracket.j, bracketLength  = stringr::str_length(bracket.j$content) , row.names = NULL)
								bracket.j <- cbind(bracket.j, before         = stringr::str_trim(stringr::str_sub(ann$text[j], bracket.j$start-1, bracket.j$start-1 )), row.names = NULL)
								bracket.j <- cbind(bracket.j, after          = stringr::str_trim(stringr::str_sub(ann$text[j], bracket.j$end+1, bracket.j$end+1 )), row.names = NULL) #mind the 1
								bracket.j <- cbind(bracket.j, posSpaceInside = data.frame(stringi::stri_locate(as.character(bracket.j$content), regex=" ", mode="last"), stringsAsFactors= FALSE)$start, row.names = NULL)
								
								#something went wrong, probably missing closing bracket
								if (is.null(bracket.j$bracketLength)) {		break}
								if (is.na(bracket.j$bracketLength)) {		break}
								
								#---- adjust length of brackets by inserting spaces inside the brackets
								#check which is the longer bracket
								difference <- bracket.j$bracketLength - bracket.i$bracketLength
								
								if (difference>0) {
									#modify i
									difference <- abs(difference)
									insert.char <- " "									
									if (bracket.i$after=="" | stringr::str_detect(bracket.i$after,"\\W")) {
										#if content ends after ] or a non-word character follows --> space immediately before ]
										insert.pos <- bracket.i$end-1
									} else if (!is.na(bracket.i$posSpaceInside)) {
										#if there is a space inside the bracket --> insert spaces there
										insert.pos <- bracket.i$start + bracket.i$posSpaceInside-1
									} else if (bracket.i$before=="") {
										#	if blanks or annotation starts before [ -> insert spaces after the opening bracket 
										insert.pos <- bracket.i$start
									} else {
										#insert underscores before ]
										insert.pos <- bracket.i$end-1
										insert.char <- "_"
									}
									ann$text[i] <- stringr::str_c(stringr::str_sub(ann$text[i], start=1, end=insert.pos), 
																  strrep(insert.char, difference), 
																  stringr::str_sub(ann$text[i], start=insert.pos+1, end=stringr::str_length(ann$text[i])), 
																  sep="",
																  collapse="")
									
								} else if (difference<0) {
									difference <- abs(difference)
									
									#modify j
									insert.char <- " "
									if (bracket.j$after=="" | stringr::str_detect(bracket.j$after,"\\W")) {
										#if content ends after ] or a non-word charater follows --> space immediately before closing ]
										insert.pos <- bracket.j$end-1
									} else if (!is.na(bracket.j$posSpaceInside)) {
										#if there is a space inside the bracket --> insert spaces there
										insert.pos <- bracket.j$start + bracket.j$posSpaceInside -1
									} else if (bracket.j$before=="") {
										#if blanks or annotation starts before [ -> insert spaces after the opening bracket 
										insert.pos <- bracket.j$start
									} else {
										#insert underscores before ]
										insert.pos <- bracket.i$end-1
										insert.char <- "_"
									}
									ann$text[j] <- stringr::str_c(stringr::str_sub( ann$text[j], start=1, end=insert.pos), 
																  strrep(insert.char, difference), 
																  stringr::str_sub(ann$text[j], insert.pos+1,  end=stringr::str_length(ann$text[j])), sep="",
																  collapse="")
								}
								
								#### - the following section is where things go wrong
								# 1) after the wrapping of i, every following annotation should be again pre processed
								# or
								# 2) the wrapping should be done manually by inserting spaces based on where the break was necessary
								
								#wrap i without initial or exdent
								text_i_wrapped <- ann$text[i]
								text_i_wrapped <-  stringi::stri_wrap(text_i_wrapped, width = text_body_width, indent=0, normalize=FALSE, whitespace_only=TRUE)
								
								#add spaces in the end to fill to full width of text
								text_i_wrapped <- stringr::str_pad(string=text_i_wrapped, width=text_body_width, side="right")
								
								#joint text to long string
								text_i_wrapped <- stringr::str_flatten(text_i_wrapped, collapse="")
								
								#calculate positions of brackets again
								bracket.i <- as.data.frame(stringr::str_locate_all(text_i_wrapped, "\\[.*?\\]"), stringsAsFactors = FALSE)
								bracket.i <- bracket.i[x, ]
								bracket.i <- cbind(bracket.i, content=as.character(unlist(stringr::str_extract_all(text_i_wrapped, "\\[.*?\\]"))[x]), row.names = NULL) #mind the x
								bracket.i <- cbind(bracket.i, bracketLength=stringr::str_length(bracket.i$content) , row.names = NULL)
								bracket.i <- cbind(bracket.i, before=stringr::str_trim(stringr::str_sub(text_i_wrapped, bracket.i$start-1, bracket.i$start-1 )), row.names = NULL)
								bracket.i <- cbind(bracket.i, after=stringr::str_trim(stringr::str_sub(text_i_wrapped, bracket.i$end+1, bracket.i$end+1 )), row.names = NULL)	
								bracket.i <- cbind(bracket.i, posSpaceInside=data.frame(stringi::stri_locate(as.character(bracket.i$content), regex=" ", mode="last"), stringsAsFactors		= FALSE)$start, row.names = NULL)
								
							} #there are opening brackets in j
						} #annotation has already been aligned
					} # end: all annotations following i
				} #end: all brackets in an annotation
			} #all brackets in an annotation
		} #next annotation
	} #if brackets need to be aligned
	
	#---- . clean up ----
	#remove unnecessary spaces that make up for an entire line	
	searchString <- paste("^ {", as.character(text_body_width),"}", sep="")
	for (i in 1:length(ann$text)) {
		
		for (j in 1:10){
			ann$text[i] <- stringr::str_replace(ann$text[i], searchString, "")
		}
	}
	#remove line breaks that will lead to an error
	searchString <- "\\r?\n"
	for (i in 1:length(ann$text)) {
		ann$text[i] <- stringr::str_replace_all(ann$text[i], searchString, "")
	}
	
	# ==== OUTPUT ====
	# ---- . docx file from template ----
	doc <- officer::read_docx(path = inputTemplate)
	
	# ---- . header ----
	if (l@header.insert==TRUE) {
		#preface
		if (!is.null(headerPreface)) {
			if (!is.na(headerPreface)) {
				lines <- unlist(stringr::str_split(headerPreface, "\n"))
				style <- getStyle(l, "header.preface")$docx.template.name
				for (line in lines) {
					doc <- officer::body_add_par(doc, value = line, style )
				}
			}
		}
		#title
		if (!is.null(headerTitle)) {
			if (!is.na(headerTitle)) {
				lines <- unlist(stringr::str_split(headerTitle, "\n"))
				style <- getStyle(l, "header.title")$docx.template.name
				for (line in lines) {
					doc <- officer::body_add_par(doc, value = line, style )
				}
			}
		}
		#subtitle
		if (!is.null(headerSubtitle)) {
			if (!is.na(headerSubtitle)) {
				lines <- unlist(stringr::str_split(headerSubtitle, "\n"))
				style <- getStyle(l, "header.subtitle")$docx.template.name
				for (line in lines) {
					doc <- officer::body_add_par(doc, value = line, style )
				}
			}
		}
		#description
		if (!is.null(headerDescription)) {
			if (!is.na(headerDescription)) {
				lines <- unlist(stringr::str_split(headerDescription, "\n"))
				style <- getStyle(l, "header.description")$docx.template.name
				for (line in lines) {
					doc <- officer::body_add_par(doc, value = line, style )
				}
			}
		}
		if (headerInsertSource) {
			standardsource <- paste0(
				"(",
				t@name, ", ",
				helper_format_time(min(ann$startsec)), 
				"-", 
				helper_format_time(max(ann$endsec)),
				")")
			doc <- officer::body_add_par(doc, value = standardsource, style = getStyle(l, "header.subtitle")$docx.template.name)			
		}
	}
	
	# ---- . body
	
	#get default style
	#style_default <- getStyle(l, "body.default")$docx.template.name
	
	# set style for each line 
	ann$style_tier <- getStyle(l, "body.default")$docx.template.name
	
	for (i in 1:length(ann$text)) {
		if (i==100) {	text_exdent <- text_exdent +1 }
		if (i==1000) { 	text_exdent <- text_exdent +1 }
		if (i==10000) { text_exdent <- text_exdent +1 }
		
		# ---- . content ----
		turn_initial <- paste(ann$spacebefore[i], 
							  ann$line[i], " ", 
							  ann$speaker[i],  sep="")
		
		turn <-  stringi::stri_wrap(ann$text[i], 
									width           = text_body_width, 
									indent          = 0, 
									exdent          = text_exdent,        #original was with -0
									normalize       = FALSE, 
									initial         = turn_initial, 
									whitespace_only = TRUE)
		#?stringi::stri_wrap
		
		#write each line to doc
		for (line in turn) {
			doc <- officer::body_add_par(doc, value = line, style = ann$style_tier[i])
		}
		
		#---- . layers = columns ----
		if(!is.null(layerNames)) {
			for (layer in layerNames) {
				if (is.null(layer)) { next }
				
				#get text of layer
				text.layer <- ann[i, layer] 
				if (is.na(text.layer)) { next }
				
				# get style of layer
				style_layer <- getStyle(l, layer)$docx.template.name
				
				#indenting?
				if (stringr::str_detect(text.layer, '%INDENT%')) {
					text.layer <- stringr::str_replace(text.layer, '%INDENT%', '')
					
					text.layer <- stringi::stri_wrap(text.layer, 
													 width          = text_body_width, 
													 indent         = stringi::stri_length(turn_initial) , 
													 exdent         = 0, 
													 normalize      = FALSE, 
													 initial        = "", 
													 whitespace_only=TRUE)
				} else {
					
	
				
				#-- wrap --
				text.layer <- stringi::stri_wrap(text.layer, 
												 width          = text_body_width, 
												 indent         = 0 , 
												 exdent         = 0, 
												 normalize      = FALSE, 
												 initial        = "", 
												 whitespace_only=TRUE)
				
				}
				
				#write each line to doc
				for (line in text.layer) {
					doc <- officer::body_add_par(doc, value = line, style = style_layer)
				}
			}
		}
		
	}
	
	# ==== SAVE ====
	if (!is.null(pathOutput)) {
		print(x=doc, target = pathOutput)
	}

	return(doc)
}
#==== FUNCTONS ====
getStyle <- function(l, actStyleName) {
	id <- which(l@docx.styles$act.style.name==actStyleName)
	if (length(id)==0) { 
		stop(paste0("Style '",actStyleName, " is not defined in your styles files. Add this style to your .csv file.")) 			
	} else {
		return (
			l@docx.styles[id[1],]
		)
	}
}
