#' Export a transcript object to a print transcript
#' 
#' If you want to modify the layout of the print transcripts, create a new layout object with \code{mylayout <- methods::new("layout")}, modify the settings and pass it as argument \code{l}.
#'
#' @param t Transcript object.
#' @param l Layout object.
#' @param outputPath Character string; path where to save the transcript.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param filterSectionStartsec Double; start of selection in seconds.
#' @param filterSectionEndsec Double; end of selection in seconds.
#' @param header_heading Character string; text that will be used as heading.
#' @param header_firstinfo Character string; text that will used as first information in the header.
#' @param insert_arrow_annotationID Integer; ID of the annotation in front of which the arrow will be placed.
#' @param alignBrackets Logical; if \code{TRUE} the function will align [] brackets that signal overlapping speech (attention: experimental function; results may not satisfy)
#'
#' @return Character string; transcript as text.
#' 
#' @seealso \code{corpus_export}, \code{export_eaf}, \code{export_exb}, \code{export_rpraat}, \code{export_srt}, \code{export_textgrid} 
#' 
#' @export
#'
#' @example inst/examples/export_printtranscript.R
#' 
export_printtranscript <- function (t, 
									l=NULL, 
									outputPath=NULL, 
									filterTierNames=NULL, 
									filterSectionStartsec = NULL, 
									filterSectionEndsec = NULL,  
									insert_arrow_annotationID = "", 
									header_heading ="", 
									header_firstinfo = "" , 
									alignBrackets=FALSE) {
	#=== settings
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	} else { if (class(t)[[1]]!="transcript") 	{stop("Parameter 't' needs to be a transcript object.") 	} }

		if (is.null(l)) 	{
		l <- methods::new("layout")
	}	
	#--- check if output folder exists
	if (!is.null(outputPath)) {
		if (!dir.exists(dirname(outputPath))) {
			stop("Output folder does not exist. Modify parameter 'outputPath'.")
		}
	}
	
	#===================================== check user inputs
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
	
	#===================================== get data
	#--- filter
	t <- act::transcripts_filter_single(t, filterTierNames=filterTierNames, filterSectionStartsec = filterSectionStartsec, filterSectionEndsec = filterSectionEndsec)
	t <- act::transcripts_cure_single(t, annotationsWithReversedTimes=TRUE, overlappingAnnotations=TRUE, annotationsWithTimesBelowZero=FALSE, missingTiers=FALSE, showWarning=TRUE)
	
	#--- get annotations from transcript
	myAnnotations <- t@annotations

	if (nrow(myAnnotations)==0) { 	return("No annotations found") }
	
	#--- get only relevant columns
	myCols <- c("tier.name", "startSec","endSec","content")
	if (!all(myCols %in% colnames(myAnnotations))) {
		return(paste("ERROR: Missing columns. Annotations needs to contain: ", paste(myCols, collapse = " ", sep="")))
	}
	
	#if annotationID column is provided too
	if ("annotationID" %in% colnames(myAnnotations)) {
		myAnnotations <- myAnnotations[, c("annotationID", myCols)]
	} else {
		myAnnotations <- cbind(annotationID=NA, myAnnotations[,myCols])
	}
	myAnnotations$tier.name <- as.character(myAnnotations$tier.name)
	
	#--- exit ?
	if (nrow(myAnnotations)==0 ){		return ("") 	}
	
	#--- sort annotations by start time
	myAnnotations <- myAnnotations[order(myAnnotations$startSec), ]
	
	#===================================== space before
	myAnnotations$spacebefore <- stringr::str_pad("", width=l@spacesbefore, side="left", pad=" ")
	#insert arrow
	if (!is.na(insert_arrow_annotationID)) {
		if (length(which(myAnnotations$annotationID==insert_arrow_annotationID))>0) {
			myAnnotations$spacebefore[which(myAnnotations$annotationID==insert_arrow_annotationID)[1]] <- stringr::str_pad(l@arrow.shape, width=l@spacesbefore, side="right", pad=" ")
		}
	}
	
	#===================================== line numbers
	line_numbers <- as.character(1:nrow(myAnnotations))
	#add 0 for 1-9
	line_numbers[1:min(length(line_numbers),9)] <- stringr::str_pad(line_numbers[1:min(length(line_numbers),9)], width=2, side="left", pad="0")
	#set line numbers
	myAnnotations$line <- line_numbers
	
	#===================================== speakers
	#--- get tier names
	tierNames 				<- as.character(unique(myAnnotations$tier.name))
	if (l@pauseTier.regex=="") {
		#if no pause filter -> get all names
		tierNames_withoutPause <- tierNames
	} else {
		#if  pause filter -> get all names but the pause tier
		tierNames_withoutPause	<- tierNames[setdiff(1:length(tierNames), grep(l@pauseTier.regex, tierNames, ignore.case =TRUE, perl = TRUE))]
	}
	
	# width of the speaker abbreviations (without end character)
	text_body_width_speaker <- l@speaker.width
	if (text_body_width_speaker == -1) {
		#if full name : get maximum
		text_body_width_speaker <- max(nchar(as.character(tierNames_withoutPause)))
	}
	
	#--- take the tier name
	myAnnotations$speaker <- myAnnotations$tier.name
	
	
	#--- same speaker as before ? --> set to ""
	myAnnotations_speaker_previous <- c("", myAnnotations$speaker[1:length(myAnnotations$speaker)-1] )
	sameSpeaker_pos <- myAnnotations_speaker_previous==myAnnotations$speaker
	myAnnotations$speaker[sameSpeaker_pos] <- ""
	included_speakers_pos <- !sameSpeaker_pos
	
	#--- pause tier ? --> set to ""
	if (l@pauseTier.regex!="") {
		#set pauses to ""
		pauses_pos <- grep(l@pauseTier.regex, myAnnotations$speaker, ignore.case =TRUE, perl = TRUE)
		myAnnotations$speaker[pauses_pos] <- ""
		included_speakers_pos[pauses_pos] <- FALSE
	}
	
	#all others: cut to correct length and add end sign
	myAnnotations$speaker[included_speakers_pos] <- paste(substr(myAnnotations$speaker[included_speakers_pos],1, text_body_width_speaker), l@speaker.ending, sep="")
	
	#add missing spaces
	myAnnotations$speaker <- stringr::str_pad(myAnnotations$speaker, width=text_body_width_speaker+nchar(l@speaker.ending), side="right", pad=" ")
	
	
	#===================================== text section
	myAnnotations <- myAnnotations[order(myAnnotations$startSec), ]
	text_exdent <- l@spacesbefore + text_body_width_speaker + nchar(l@speaker.ending) + 3
	text_body_width   <- l@transcript.width - text_exdent
	text_all <- c()	
	
	
	myAnnotations$text <- myAnnotations$content
	myAnnotations$text <- stringr::str_trim(myAnnotations$text)
	myAnnotations$bracketsLeftAligned <- FALSE
	#myAnnotations$text[21]
	
	#---- brackets
	if (l@brackets.tryToAlign 		== TRUE) {
		for (i in 1:length(myAnnotations$text)) {
			
			#check if the first bracket is already aligned
			if (myAnnotations$bracketsLeftAligned[i]) {
				startWithBracketI <-2					
			} else { 
				startWithBracketI <-1	
			}
			
			#check if text contains a bracket
			openingBracketsI.nr 	<- 	stringr::str_count(myAnnotations$text[i], "\\[")
			
			#check if there are more brackets to be aligned
			if (startWithBracketI<=openingBracketsI.nr) {
				
				#search correspondance for each opening bracket
				for (x in startWithBracketI:openingBracketsI.nr) {
					
					#exit if next recourdset would be outside the data frame 
					startSearching <- i+1
					if (startSearching>length(myAnnotations$text)) {
						break
					}
					
					#start searching at the next dataset
					for (j in startSearching:length(myAnnotations$text)) {
						
						#exit: if this recordset does not temporally overlap
						if (myAnnotations$startSec[j]> myAnnotations$endSec[i]) {
							break
						} 
						
						#skip recordset : if it has already been move/aligned
						if (!myAnnotations$bracketsLeftAligned[j]) {
							openingBracketsJ.nr <- 		stringr::str_count(myAnnotations$text[j], "\\[")
							
							
							if (openingBracketsJ.nr>0 ) {
								#get positions and contents of brackets
								bracket.i <- as.data.frame(stringr::str_locate_all(myAnnotations$text[i], "\\[.*?\\]"), stringsAsFactors = FALSE)
								bracket.i <- bracket.i[x, ]
								bracket.i <- cbind(bracket.i, content=as.character(unlist(stringr::str_extract_all(myAnnotations$text[i], "\\[.*?\\]"))[x]), row.names = NULL) #mind the x
								bracket.i <- cbind(bracket.i, bracketLength=stringr::str_length(bracket.i$content) , row.names = NULL)
								bracket.i <- cbind(bracket.i, before=stringr::str_trim(stringr::str_sub(myAnnotations$text[i], bracket.i$start-1, bracket.i$start-1 )), row.names = NULL)
								bracket.i <- cbind(bracket.i, after=stringr::str_trim(stringr::str_sub(myAnnotations$text[i], bracket.i$end+1, bracket.i$end+1 )), row.names = NULL)	
								bracket.i <- cbind(bracket.i, posSpaceInside=as.data.frame(stringi::stri_locate(as.character(bracket.i$content), regex=" ", mode="last"))$start, row.names = NULL)
								
								bracket.j <- as.data.frame(stringr::str_locate_all(myAnnotations$text[j], "\\[.*?\\]"), stringsAsFactors = FALSE)
								bracket.j <- bracket.j[1, ]
								bracket.j <- cbind(bracket.j, content=as.character(unlist(stringr::str_extract_all(myAnnotations$text[j], "\\[.*?\\]"))[1]), row.names = NULL) #mind the 1
								bracket.j <- cbind(bracket.j, bracketLength=stringr::str_length(bracket.j$content) , row.names = NULL)
								bracket.j <- cbind(bracket.j, before=stringr::str_trim(stringr::str_sub(myAnnotations$text[j], bracket.j$start-1, bracket.j$start-1 )), row.names = NULL)
								bracket.j <- cbind(bracket.j, after=stringr::str_trim(stringr::str_sub(myAnnotations$text[j], bracket.j$end+1, bracket.j$end+1 )), row.names = NULL) #mind the 1
								bracket.j <- cbind(bracket.j, posSpaceInside=as.data.frame(stringi::stri_locate(as.character(bracket.j$content), regex=" ", mode="last"))$start, row.names = NULL)
								
								#something went wrong, probably missing closing bracket
								if (is.null(bracket.i$bracketLength)) {		break}
								if (is.na(bracket.i$bracketLength)) {		break}
								if (is.null(bracket.j$bracketLength)) {		break}
								if (is.na(bracket.j$bracketLength)) {		break}
								
								#--- put j annotation j in the right place 
								myAnnotations$text[j] <- stringr::str_flatten(c(strrep(" ", abs(bracket.i$start-bracket.j$start)), myAnnotations$text[j]) , collapse="")
								myAnnotations$bracketsLeftAligned[j] <- TRUE
								
								
								
								#---- adjust length of brackets by inserting spaces
								#check which is the longer bracket
								difference <- bracket.j$bracketLength - bracket.i$bracketLength
								new <-""
								
								if (difference>0) {
									#modify i
									difference <- abs(difference)
									
									if (bracket.i$after=="" | stringr::str_detect(bracket.i$after,"\\W")) {
										#if content ends after ] or a non-word charater follows --> space immediately before ]
										insert.pos <- bracket.i$end-1
										insert.char <- " "
										
										#new <- stringr::str_c(stringr::str_sub(myAnnotations$text[i], start=1, end=bracket.i$end-1), 
										#					  strrep(" ", difference), 
										#					  stringr::str_sub(myAnnotations$text[i], bracket.i$end, stringr::str_length(myAnnotations$text[i])), sep="",
										#					  collapse="")
										
									} else if (!is.na(bracket.i$posSpaceInside)) {
										#if there is a space inside the bracket --> insert spaces there
										
										insert.pos <- bracket.i$start + bracket.i$posSpaceInside-1
										insert.char <- " "
										
										#new <- stringr::str_c(stringr::str_sub(myAnnotations$text[i], start=1,  end=bracket.i$start + bracket.i$posSpaceInside-1), 
										#					  strrep(" ", difference), 
										#					  stringr::str_sub(myAnnotations$text[i], bracket.i$start +bracket.i$posSpaceInside, stringr::str_length(myAnnotations$text[i])),
										#					  sep="", collapse="")
										
									} else if (bracket.i$before=="") {
										#	if blanks or annotation starts before [ -> insert spaces after the opening bracket 
										insert.pos <- bracket.i$start
										insert.char <- " "
										
										#	 new <- stringr::str_c(stringr::str_sub( myAnnotations$text[i], start=1, end= bracket.i$start), 
										#						  strrep(" ", difference), 
										#						  stringr::str_sub(myAnnotations$text[i], bracket.i$start+1, stringr::str_length(myAnnotations$text[i])), sep="",
										#						  collapse="")
										
									} else {
										#insert underscores before ]
										insert.pos <- bracket.i$end-1
										insert.char <- "_"
										
										#new <- stringr::str_c(stringr::str_sub(myAnnotations$text[i], start=1, end= bracket.i$end-1), 
										#					  strrep("_", difference), 
										#					  stringr::str_sub(myAnnotations$text[i], bracket.i$end, stringr::str_length(myAnnotations$text[i])), sep="",
										#					  collapse="")
									}
									new <- stringr::str_c(stringr::str_sub(myAnnotations$text[i], start=1, end=insert.pos), 
														  strrep(insert.char, difference), 
														  stringr::str_sub(myAnnotations$text[i], end=insert.pos+1, stringr::str_length(myAnnotations$text[i])), sep="",
														  collapse="")
									myAnnotations$text[i] <- new
									
								} else if (difference<0) {
									difference <- abs(difference)
									
									#modify j
									if (bracket.j$after=="" | stringr::str_detect(bracket.j$after,"\\W")) {
										#if content ends after ] or a non-word charater follows --> space immediately before ]
										insert.pos <- bracket.i$end-1
										insert.char <- " "
										
										#				new <- stringr::str_c(stringr::str_sub(myAnnotations$text[j], start=1,  end=bracket.j$end-1), 
										#				  strrep(" ", difference), 
										#				  stringr::str_sub(myAnnotations$text[j], bracket.j$end, stringr::str_length(myAnnotations$text[j])), 
										#				  sep="", collapse="")
										
									} else if (!is.na(bracket.j$posSpaceInside)) {
										#if there is a space inside the bracket --> insert spaces there
										insert.pos <- bracket.j$start + bracket.j$posSpaceInside -1
										insert.char <- " "
										
										#new <- stringr::str_c(stringr::str_sub(myAnnotations$text[j], start=1,  end=bracket.j$start + bracket.j$posSpaceInside -1), 
										#					  strrep(" ", difference), 
										#					  stringr::str_sub(myAnnotations$text[j], bracket.j$start + bracket.j$posSpaceInside, stringr::str_length(myAnnotations$text[j])),
										#					  sep="", collapse="")
										
									} else if (bracket.j$before=="") {
										#if blanks or annotation starts before [ -> insert spaces after the opening bracket 
										insert.pos <- bracket.j$start
										insert.char <- " "
										
										#new <- stringr::str_c(stringr::str_sub( myAnnotations$text[j], start=1, end=bracket.j$start), 
										#					  strrep(" ", difference), 
										#					  stringr::str_sub(myAnnotations$text[j], bracket.j$start+1,  end=stringr::str_length(myAnnotations$text[j])), sep="",
										#					  collapse="")
										
									} else {
										#insert underscores before ]
										insert.pos <- bracket.i$end-1
										insert.char <- "_"
										
										#new <- stringr::str_c(stringr::str_sub(myAnnotations$text[j], start=1,  end=bracket.j$end-1), 
										#					  strrep("_", difference), 
										#					  stringr::str_sub(myAnnotations$text[j], bracket.j$end,  end=stringr::str_length(myAnnotations$text[j])), sep="",
										#					  collapse="")	
									}
									
									new <- stringr::str_c(stringr::str_sub( myAnnotations$text[j], start=1, end=insert.pos), 
														  strrep(insert.char, difference), 
														  stringr::str_sub(myAnnotations$text[j], insert.pos+1,  end=stringr::str_length(myAnnotations$text[j])), sep="",
														  collapse="")
									
									
									myAnnotations$text[j] <- new
								}
								
								#---- align j annotation
								#wrap i without initioal or exdent
								text_i_wrapped <- myAnnotations$text[i]
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
								bracket.i <- cbind(bracket.i, posSpaceInside=as.data.frame(stringi::stri_locate(as.character(bracket.i$content), regex=" ", mode="last"))$start, row.names = NULL)
								
								
							} #there are opening brackets in j
						} #recordset has already been aligned
					} # j
				} #next w
			} #i bracket needs still to be processed
		} #next i
	} #if brackets need to be aligned
	
	
	#wrap and build entire text
	for (i in 1:length(myAnnotations$text)) {
		
		if (i==100) {	text_exdent <- text_exdent +1 }
		if (i==1000) { 	text_exdent <- text_exdent +1 }
		if (i==10000) { text_exdent <- text_exdent +1 }
		
		text_line <- myAnnotations$text[i]	
		
		#remove unnecessary spaces that make up for an entire line	
		searchString <- paste("^ {", as.character(text_body_width),"}", sep="")
		for (j in 1:10){
			text_line <- stringr::str_replace(text_line, searchString, "")
		}
		#remove line breaks that will lead to an error
		searchString <- "\\r?\\n"
		text_line <- stringr::str_replace_all(text_line, searchString, "")
		
		#wrap
		text_initial <- paste(myAnnotations$spacebefore[i], myAnnotations$line[i], " ", myAnnotations$speaker[i],  sep="")
		text_line <-  stringi::stri_wrap(text_line, width = text_body_width, indent=0, exdent=text_exdent, normalize=FALSE, initial=text_initial, whitespace_only=TRUE)
		
		#add line to entire text
		text_all  <- c(text_all, text_line)
		
		#add additional  lines
		if (l@additionalline1.insert == TRUE) {
			text_additionalline <- l@additionalline1.text
			if (l@additionalline1.indent == TRUE) {
				text_additionalline <- stringi::stri_wrap(text_additionalline, width = text_body_width, indent=stringi::stri_length(text_initial) , exdent=0, normalize=FALSE, initial="", whitespace_only=TRUE)
			} 
			text_all  <- c(text_all, text_additionalline)
		}
		if (l@additionalline2.insert == TRUE) {
			text_additionalline <- l@additionalline2.text
			if (l@additionalline2.indent == TRUE) {
				text_additionalline <- stringi::stri_wrap(text_additionalline, width = text_body_width, indent=stringi::stri_length(text_initial), exdent=0, normalize=FALSE, initial="", whitespace_only=TRUE)
			} 
			text_all  <- c(text_all, text_additionalline)
		}
	} 
	
	output <-text_all
	
	#=== header
	if (l@header.insert==TRUE) {
		#heading
		header <- paste(header_heading, "\n(", sep="")
		
		#first info
		if (header_firstinfo!="") {
			header <- paste(header, header_firstinfo, ", ", sep="")
		}
		header <- paste(header, t@name , ", ", sep="")
		
		if (is.null(filterSectionStartsec)) {
			header <- paste(header, "0-", sep="")
		} else {
			if (filterSectionStartsec<0) {
				header <- paste(header, "0-", sep="")
			} else {
				header <- paste(header, round(filterSectionStartsec, digits=1), "-", sep="")
			}
		}
		
		if (is.null(filterSectionEndsec)) {
			header <- paste(header, round(t@length, digits=0), " sec)\n", sep="")
		} else {
			if (filterSectionEndsec<0) {
				header <- paste(header, round(t@length, digits=0), " sec)\n", sep="")
			} else {
				header <- paste(header, round(filterSectionStartsec, digits=1), " sec)\n", sep="")
			}
		}
		output <- c(header, output)
	}
	
	if (is.null(outputPath)) {
	} else {
		fileConn <-file(outputPath)
		writeLines(output, fileConn)
		close(fileConn)
	}
	
	#	output <-stringr::str_flatten(output, collapse="\\n")
	
	return(output)
}