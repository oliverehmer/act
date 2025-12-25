#' Export print transcript in .txt format
#' 
#' If you want to modify the layout of the print transcripts, create a new layout object with \code{mylayout <- methods::new("layout")}, modify the settings and pass it as argument \code{l}.
#' In the layout object you may also set additional filters to include/exclude tiers matching regular expressions.
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
#' @param collapse Logical; if \code{FALSE} a vector will be created, each element corresponding to one annotation. if \code{TRUE} a single string will be created, collapsed by linebreaks \\n.
#' 
#' @return Character string; transcript as text.
#' 
#' @seealso \link{corpus_export}, \link{export_eaf}, \link{export_exb}, \link{export_rpraat}, \link{export_srt}, \link{export_textgrid}, \link{export_docx} 
#' 
#' @export
#'
#' @example inst/examples/export_txt.R
#'  
#' 
export_txt <- function (t, 
						l                       = NULL, 
						pathOutput              = NULL, 
						filterTierNames         = NULL, 
						filterSectionStartsec   = NULL, 
						filterSectionEndsec     = NULL,  
						insertArrowAnnotationID = "", 
						headerPreface           = NULL, 
						headerTitle             = NULL, 
						headerSubtitle          = NULL,
						headerDescription       = NULL,
						headerInsertSource      = TRUE,
						collapse                = TRUE) {
	
	if (1==3) {
	#	library(act)
	#	t<-examplecorpus@transcripts[[1]]
	#	l                       <- NULL 
	#	pathOutput              <- NULL 
	#	filterTierNames         <- NULL 
	#	filterSectionStartsec   <- NULL 
	#	filterSectionEndsec     <- NULL  
	#	insertArrowAnnotationID <- "" 
	#	headerPreface           <- NULL 
	#	headerTitle             <- NULL 
	#	headerSubtitle          <- NULL
	#	headerDescription       <- NULL
	#	headerInsertSource      <- TRUE
	#	collapse                <- TRUE
	}
	
	#==== SETTINGS ====
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
	
	#==== CHECKS ====
	#if output folder exists
	if (!is.null(pathOutput)) {
		if (!dir.exists(dirname(pathOutput))) {
			stop("Output folder does not exist. Modify parameter 'pathOutput'.")
		}
	}
	
	#---- exit if transcript width is too short or too long
	if (l@transcript.width == -1) {
	} else if (l@transcript.width < 40) {
		return( "ERROR: The width of the transcript is to low. Minimum is 40. Check option 'l@transcript.width'")
	}
	
	#---- exit if tier length is too short or too long
	if (l@speaker.width == -1) {
	} else if (l@speaker.width==0) {
		return( "ERROR: Length of tier names is to short. Minimum is 1. Check option 'l@speaker.width'.")
	} else if (l@speaker.width < -1) {
		return("ERROR: Length of tier names is to short. Minimum is 1. Check option 'l@speaker.width'.")
	} else if (l@speaker.width > 25) {
		return("ERROR: Length of tier names is to long. Maximum is 25. Check option 'l@speaker.width'.")
	}
	
	#---- increase ----
	options(act.transcript.spacesbefore= max(0, l@spacesbefore))
	
	#====  get data ====
	#---- tier names ----
	if (is.null(filterTierNames)) {
		filterTierNames <- t@tiers$name
	}
	
	#filter the filterTierNames by regular expressions
	if (!is.null(l@filter.tier.includeRegEx)) {
		if (length(l@filter.tier.includeRegEx)>0) {
			if (!is.na(l@filter.tier.includeRegEx)) {
				if (l@filter.tier.includeRegEx!="") {
					#	filterTierNames <- grep(pattern=l@filter.tier.includeRegEx, filterTierNames, value=TRUE)
					filterTierNames <- filterTierNames[grep(pattern=l@filter.tier.includeRegEx, filterTierNames)]
				}
			}
		}
	}
	if (!is.null(l@filter.tier.excludeRegEx)) {
		if (length(l@filter.tier.excludeRegEx)>0) {
			if (!is.na(l@filter.tier.excludeRegEx)) {
				if (l@filter.tier.excludeRegEx!="") {
					IDs<-grep(pattern=l@filter.tier.excludeRegEx, filterTierNames)
					if (length(IDs)>0) {
						filterTierNames <- filterTierNames[-IDs]
					}
				}
			}
		}
	}
	
	#---- filter data ----
	t <- act::transcripts_filter_single(t, 
										filterTierNames       = filterTierNames, 
										filterSectionStartsec = filterSectionStartsec, 
										filterSectionEndsec   = filterSectionEndsec)
	
	#---- cure data ----
	t <- act::transcripts_cure_single(t, 
									  annotationsTimesReversed=TRUE, 
									  annotationsOverlap=TRUE, 
									  annotationsTimesBelowZero=FALSE, 
									  tiersMissing=FALSE, 
									  warning=TRUE)

	#---- get annotations ----
	ann <- t@annotations

	if (nrow(ann)==0) { 	return("No annotations found") }
	
	#only relevant columns
	myCols <- c("tierName", "startsec","endsec","content")
	if (!all(myCols %in% colnames(ann))) {
		return(paste("ERROR: Missing columns. Annotations needs to contain: ", paste(myCols, collapse = " ", sep="")))
	}
	
	#if annotationID column is provided too
	if ("annotationID" %in% colnames(ann)) {
		ann <- ann[, c("annotationID", myCols)]
	} else {
		ann <- cbind(annotationID=NA, ann[,myCols])
	}
	ann$tierName <- as.character(ann$tierName)
	
	#---- exit ----
	if (nrow(ann)==0 ){		return ("") 	}
	
	#---- sort annotations ----
	ann <- ann[order(ann$startsec), ]
	
	#==== INFO ====
	#---- . space before ----
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
	
	#---- . speakers ----
	#tier names
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
	
	#take the tier name
	ann$speaker <- ann$tierName
	
	#same speaker as before ? --> set to ""
	ann_speaker_previous <- c("", ann$speaker[1:length(ann$speaker)-1] )
	sameSpeaker_pos <- ann_speaker_previous==ann$speaker
	ann$speaker[sameSpeaker_pos] <- ""
	included_speakers_pos <- !sameSpeaker_pos
	
	#all other tiers: extract using regex
	if (!is.na(l@speaker.regex)) {
		ann$speaker[included_speakers_pos] <- stringr::str_extract(ann$speaker[included_speakers_pos], l@speaker.regex)
	}
	#cut to correct length and add end sign
	ann$speaker[included_speakers_pos] <- paste(substr(ann$speaker[included_speakers_pos],1, text_body_width_speaker), l@speaker.ending, sep="")
	#add missing spaces
	ann$speaker <- stringr::str_pad(ann$speaker, width=text_body_width_speaker+nchar(l@speaker.ending), side="right", pad=" ")
	
	
	#---- . text section ----
	ann <- ann[order(ann$startsec), ]
	text_exdent <- l@spacesbefore + text_body_width_speaker + nchar(l@speaker.ending) + 3
	text_body_width   <- l@transcript.width - text_exdent
	text_all <- c()	
	
	ann$text <- ann$content
	ann$text <- stringr::str_trim(ann$text)
	ann$firstBracketIsAligned <- FALSE
	ann$bracketAlignedWithAnnotationI <- 0
	#ann$text[21]
	
	#---- . brackets ----
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
					
					#start: process all annotations following the current anotation i
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
								#get positions and contents of brackets
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
								
								bracket.j <- data.frame(stringr::str_locate_all(ann$text[j], "\\[.*?\\]"), stringsAsFactors = FALSE)
								bracket.j <- bracket.j[1, ]
								bracket.j <- cbind(bracket.j, content=as.character(unlist(stringr::str_extract_all(ann$text[j], "\\[.*?\\]"))[1]), row.names = NULL) #mind the 1
								bracket.j <- cbind(bracket.j, bracketLength=stringr::str_length(bracket.j$content) , row.names = NULL)
								bracket.j <- cbind(bracket.j, before=stringr::str_trim(stringr::str_sub(ann$text[j], bracket.j$start-1, bracket.j$start-1 )), row.names = NULL)
								bracket.j <- cbind(bracket.j, after=stringr::str_trim(stringr::str_sub(ann$text[j], bracket.j$end+1, bracket.j$end+1 )), row.names = NULL) #mind the 1
								bracket.j <- cbind(bracket.j, posSpaceInside=data.frame(stringi::stri_locate(as.character(bracket.j$content), regex=" ", mode="last"), stringsAsFactors= FALSE)$start, row.names = NULL)
								
								#something went wrong, probably missing closing bracket
								if (is.null(bracket.i$bracketLength)) {		break}
								if (is.na(bracket.i$bracketLength)) {		break}
								if (is.null(bracket.j$bracketLength)) {		break}
								if (is.na(bracket.j$bracketLength)) {		break}
								
								#---- put j annotation j in the right place 
								ann$text[j] <- stringr::str_flatten(c(strrep(" ", abs(bracket.i$start-bracket.j$start)), ann$text[j]) , collapse="")
								ann$firstBracketIsAligned[j] <- TRUE
			
								#---- adjust length of brackets by inserting spaces inside the brackets
								#check which is the longer bracket
								difference <- bracket.j$bracketLength - bracket.i$bracketLength
								new <-""
								
								if (difference>0) {
									#modify i
									difference <- abs(difference)
									
									if (bracket.i$after=="" | stringr::str_detect(bracket.i$after,"\\W")) {
										#if content ends after ] or a non-word character follows --> space immediately before ]
										insert.pos <- bracket.i$end-1
										insert.char <- " "
										
										#new <- stringr::str_c(stringr::str_sub(ann$text[i], start=1, end=bracket.i$end-1), 
										#					  strrep(" ", difference), 
										#					  stringr::str_sub(ann$text[i], bracket.i$end, stringr::str_length(ann$text[i])), sep="",
										#					  collapse="")
										
									} else if (!is.na(bracket.i$posSpaceInside)) {
										#if there is a space inside the bracket --> insert spaces there
										
										insert.pos <- bracket.i$start + bracket.i$posSpaceInside-1
										insert.char <- " "
										
										#new <- stringr::str_c(stringr::str_sub(ann$text[i], start=1,  end=bracket.i$start + bracket.i$posSpaceInside-1), 
										#					  strrep(" ", difference), 
										#					  stringr::str_sub(ann$text[i], bracket.i$start +bracket.i$posSpaceInside, stringr::str_length(ann$text[i])),
										#					  sep="", collapse="")
										
									} else if (bracket.i$before=="") {
										#	if blanks or annotation starts before [ -> insert spaces after the opening bracket 
										insert.pos <- bracket.i$start
										insert.char <- " "
										
										#	 new <- stringr::str_c(stringr::str_sub( ann$text[i], start=1, end= bracket.i$start), 
										#						  strrep(" ", difference), 
										#						  stringr::str_sub(ann$text[i], bracket.i$start+1, stringr::str_length(ann$text[i])), sep="",
										#						  collapse="")
										
									} else {
										#insert underscores before ]
										insert.pos <- bracket.i$end-1
										insert.char <- "_"
										
										#new <- stringr::str_c(stringr::str_sub(ann$text[i], start=1, end= bracket.i$end-1), 
										#					  strrep("_", difference), 
										#					  stringr::str_sub(ann$text[i], bracket.i$end, stringr::str_length(ann$text[i])), sep="",
										#					  collapse="")
									}
									new <- stringr::str_c(stringr::str_sub(ann$text[i], start=1, end=insert.pos), 
														  strrep(insert.char, difference), 
														  stringr::str_sub(ann$text[i], start=insert.pos+1, end=stringr::str_length(ann$text[i])), 
														  sep="",
														  collapse="")
									ann$text[i] <- new
									
								} else if (difference<0) {
									difference <- abs(difference)
									
									#modify j
									if (bracket.j$after=="" | stringr::str_detect(bracket.j$after,"\\W")) {
										#if content ends after ] or a non-word charater follows --> space immediately before ]
										insert.pos <- bracket.i$end-1
										insert.char <- " "
										
										#				new <- stringr::str_c(stringr::str_sub(ann$text[j], start=1,  end=bracket.j$end-1), 
										#				  strrep(" ", difference), 
										#				  stringr::str_sub(ann$text[j], bracket.j$end, stringr::str_length(ann$text[j])), 
										#				  sep="", collapse="")
										
									} else if (!is.na(bracket.j$posSpaceInside)) {
										#if there is a space inside the bracket --> insert spaces there
										insert.pos <- bracket.j$start + bracket.j$posSpaceInside -1
										insert.char <- " "
										
										#new <- stringr::str_c(stringr::str_sub(ann$text[j], start=1,  end=bracket.j$start + bracket.j$posSpaceInside -1), 
										#					  strrep(" ", difference), 
										#					  stringr::str_sub(ann$text[j], bracket.j$start + bracket.j$posSpaceInside, stringr::str_length(ann$text[j])),
										#					  sep="", collapse="")
										
									} else if (bracket.j$before=="") {
										#if blanks or annotation starts before [ -> insert spaces after the opening bracket 
										insert.pos <- bracket.j$start
										insert.char <- " "
										
										#new <- stringr::str_c(stringr::str_sub( ann$text[j], start=1, end=bracket.j$start), 
										#					  strrep(" ", difference), 
										#					  stringr::str_sub(ann$text[j], bracket.j$start+1,  end=stringr::str_length(ann$text[j])), sep="",
										#					  collapse="")
										
									} else {
										#insert underscores before ]
										insert.pos <- bracket.i$end-1
										insert.char <- "_"
										
										#new <- stringr::str_c(stringr::str_sub(ann$text[j], start=1,  end=bracket.j$end-1), 
										#					  strrep("_", difference), 
										#					  stringr::str_sub(ann$text[j], bracket.j$end,  end=stringr::str_length(ann$text[j])), sep="",
										#					  collapse="")	
									}
									
									new <- stringr::str_c(stringr::str_sub( ann$text[j], start=1, end=insert.pos), 
														  strrep(insert.char, difference), 
														  stringr::str_sub(ann$text[j], insert.pos+1,  end=stringr::str_length(ann$text[j])), sep="",
														  collapse="")
						
									ann$text[j] <- new
								}
								
								#### - the following sectionis where things go wrong
								# 1) after the wrapping oof i, exery folloqing annotation should be again pre processed
								# or
								# 2) thw wrapping should be done manually by inserting spaces bases on where the brak was necessary
								
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
	
	#wrap and build entire text
	#i<-7
	for (i in 1:length(ann$text)) {
		
		if (i==100) {	text_exdent <- text_exdent +1 }
		if (i==1000) { 	text_exdent <- text_exdent +1 }
		if (i==10000) { text_exdent <- text_exdent +1 }
		
		text_line <- ann$text[i]	
		
		#remove unnecessary spaces that make up for an entire line	
		searchString <- paste("^ {", as.character(text_body_width),"}", sep="")
		for (j in 1:10){
			text_line <- stringr::str_replace(text_line, searchString, "")
		}
		#remove line breaks that will lead to an error
		searchString <- "\\r?\n"
		text_line <- stringr::str_replace_all(text_line, searchString, "")
		
		#wrap
		text_initial <- paste(ann$spacebefore[i], ann$line[i], " ", ann$speaker[i],  sep="")
		text_line <-  stringi::stri_wrap(text_line, width = text_body_width, indent=0, exdent=text_exdent, normalize=FALSE, initial=text_initial, whitespace_only=TRUE)
		
		#add line to entire text
		text_all  <- c(text_all, text_line)
		#print(text_all)
		
		#add additional  lines
		#text_addline <- l@addline1
		#if (!is.na(text_addline)) {
		#	if (stringr::str_detect(text_addline, '#INDENT#')) {
		#		text_addline <- stringr::str_replace(text_addline, '#INDENT#', '')
		#		text_addline <- stringi::stri_wrap(text_addline, width = text_body_width, indent=stringi::stri_length(text_initial) , exdent=0, normalize=FALSE, initial="", whitespace_only=TRUE)
		#	}
		#	text_all  <- c(text_all, text_addline)
		#}
		#text_addline <- l@addline2
		#if (!is.na(text_addline)) {
		#	if (stringr::str_detect(text_addline, '#INDENT#')) {
		#		text_addline <- stringr::str_replace(text_addline, '#INDENT#', '')
		#		text_addline <- stringi::stri_wrap(text_addline, width = text_body_width, indent=stringi::stri_length(text_initial) , exdent=0, normalize=FALSE, initial="", whitespace_only=TRUE)
		#	}
		#	text_all  <- c(text_all, text_addline)
		#}
	} 
	
	output <- text_all
	
	#==== header

	if (l@header.insert==TRUE) {
		header <- ''
		
		#preface
		if (!is.null(headerPreface)) {
			if (!is.na(headerPreface)) {
				header <- paste(headerPreface, "\n", sep="")
			}
		}
		#title
		if (!is.null(headerTitle)) {
			if (!is.na(headerTitle)) {
				header <- paste(headerTitle, "\n", sep="")
			}
		}
		#subtitle
		if (!is.null(headerSubtitle)) {
			if (!is.na(headerSubtitle)) {
				header <- paste(header, headerSubtitle, "\n", sep="")
			}
		}
		#description
		if (!is.null(headerDescription)) {
			if (!is.na(headerDescription)) {
				header <- paste(header, headerDescription, "\n", sep="")
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
			header <- paste(header, standardsource, "\n", sep="")		
		}
		output <- c(header, output)
	}
	
	if (collapse) {
		output <- stringr::str_c(output, sep='\n', collapse = '\n')
		output <-  stringr::str_c(c(output, '\n'), sep='', collapse = '')
	}
	
	if (!is.null(pathOutput)) {
		#pathOutput<-'/Users/oliverehmer/Desktop/test.txt'
		fileConn <- file(pathOutput)
		writeLines(output, fileConn)
		close(fileConn)
	}
	
	#	output <- stringr::str_flatten(output, collapse="\\n")
	return(output)
}


#' @rdname export_txt
#' @param ... Arguments passed to `export_txt()`.
#' 
#' @seealso \code{\link{export_txt}}
#' 
#' @export
export_printtranscript <- function(...) {
	export_txt(...)
}
