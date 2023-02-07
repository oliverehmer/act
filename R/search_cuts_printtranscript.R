#' Create print transcripts for all search results
#'
#' Print transcripts in the style of conversation analysis will be created for each search result.
#' The transcripts will be inserted into the column defined in \code{s@cuts.column.printtranscript}.
#' All transcripts will be stored in \code{s@cuts.printtranscripts}.
#'
#' If you want to modify the layout of the print transcripts, create a new layout object with \code{mylayout <- methods::new("layout")}, modify the settings and pass it as argument \code{l}.
#'
#' @param x Corpus object.
#' @param s Search object.
#' @param cutSpanBeforesec Double; Start the cut some seconds before the hit to include some context; the default NULL will take the value as set in @cuts.span.beforesec of the search object.
#' @param cutSpanAftersec Double; End the cut some seconds before the hit to include some context; the default NULL will take the value as set in @cuts.span.beforesec of the search object.
#' @param l Layout object.
#' @param outputFolder Character string; if parameter is not set, the print transcripts will only be inserted in \code{s@results}; if the path to a existing folder is given transcripts will be saved in '.txt' format.
#'
#' @return Search object; 
#' @export
#'
#' @example inst/examples/search_cuts_printtranscript.R
#' 
search_cuts_printtranscript <- function(x, 
										s,
										cutSpanBeforesec = NULL,
										cutSpanAftersec = NULL,
										l=NULL, 
										outputFolder=NULL ) {
	#x <- corpus
	#s <- suche
	#cutSpanBeforesec<-0
	#cutSpanAftersec <- 0
	#l<-NULL
	#outputFolder<-NULL
	
	# x            = korpus 
	# s            <- suche
	# cutSpanBeforesec <- "/Users/oliverehmer/Desktop/suchergebnisse/"
	# cutSpanAftersec <- NULL
	# l<-NULL 
	# outputFolder<-NULL 
	# cutSpanBeforesec <- c(1,2)
	#cutSpanBeforesec <- as.integer(1)
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{stop("Parameter 'x' needs to be a corpus object.") } }
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		}	else { if (!methods::is(s, "search")	)	{stop("Parameter 's' needs to be a search object.") 	} }
	if (is.null(s@results$transcript.name)) 		{ stop("Data frame s@results does not contain column 'transcript.name'") 	}
	
	if (is.null(l)) 	{
		l <- methods::new("layout")
	}	
	if (!is.null(cutSpanBeforesec)) 	{
		if (length(cutSpanBeforesec)!=1) {
			stop("Parameter 'cutSpanBeforesec' needs to contain only one element as a numeric value.") 
		}
		if (!is.numeric(cutSpanBeforesec)) {
			stop("Parameter 'cutSpanBeforesec' needs to be a numeric value.") 
		}
		s@cuts.span.beforesec       <- cutSpanBeforesec
	}
	if (!is.null(cutSpanAftersec)) 	{
			if (length(cutSpanAftersec)!=1) {
				stop("Parameter 'cutSpanAftersec' needs to contain only one element as a numeric value.") 
			}
			if (!is.numeric(cutSpanAftersec)) {
				stop("Parameter 'cutSpanAftersec' needs to be a numeric value.") 
			}
		s@cuts.span.aftersec       <- cutSpanAftersec
	}

	
	#--- check if output folder is given
	destination_folder <- NULL
	if (!is.null(outputFolder)) {
		destination_folder <- normalizePath(outputFolder, winslash = "/")
		if (dir.exists(destination_folder)==FALSE) 	{
			stop("Output folder does not exist.")
		}
		
		#create a sub folder for the search
		if (s@name!="") 	{
			destination_folder <- file.path(destination_folder, s@name)
			dir.create(destination_folder, showWarnings = FALSE)
			if (file.exists(destination_folder)==FALSE) 		{
				stop("Unable to create output directory")
			}
		}
	}	
	myWarnings <- ""
	
	#create sub folders for text
	if (!is.null(destination_folder)) {
		dir.create(file.path(destination_folder, "transcripts"), showWarnings = FALSE)
	}
	
	helper_progress_set("Creating transcripts",max(1,nrow(s@results)))
	
	alltranscripts <- c()
	#i<-1
	if (nrow(s@results)>0) {
		for (i in 1:nrow(s@results)) 	{
			helper_progress_tick()
			#=== get transcript
			t <- NULL
			
			if (is.null(s@results$transcript.name[i])) {
				#transcript not found
				myWarnings <- paste(myWarnings, sprintf("- result %s '%s': transcript '%s' not found in corpus. ", i, as.character(s@results[i, options()$act.export.filename.fromColumnName]),  as.character(s@results$transcript.name[i]) ), collapse="\n", sep="\n")
				
			} else {
				t <- x@transcripts[[ s@results$transcript.name[i] ]]
				
				if (is.null(t)) {
					#transcript not found
					myWarnings <- paste(myWarnings, sprintf("- result %s '%s': transcript '%s' not found in corpus. ", i, as.character(s@results[i, options()$act.export.filename.fromColumnName]),  as.character(s@results$transcript.name[i]) ), collapse="\n", sep="\n")
				}
			}
			
			if (!is.null(t)) {
				#=== assemble_file NAME
				filename <- as.character(s@results[i, options()$act.export.filename.fromColumnName])
				
				if (!exists("filename")) {
					filename <- as.character(i)
				} else if (is.na(filename)) {
					filename <- as.character(i)
				} else if (length(filename)==0) {
					filename <- as.character(i)
				} else {
					if (filename == "") {filename <- as.character(i)}
				}
				
				#=== get start & end
				startSec 	<- max(0, s@results$startSec[i] - s@cuts.span.beforesec)
				endSec 		<- min(s@results$endSec[i] + s@cuts.span.beforesec, t@length.sec)
				
				#arrow
				if (!l@arrow.insert) {
					arrowID <- -1
				} else {
					arrowID <- s@results$annotationID[i]
				}
				
				#header
				header_heading 		<- ""
				header_firstinfo 	<- ""
				if (l@header.insert==TRUE) {
					if (l@header.heading.fromColumnName %in% colnames(s@results)) {
						header_heading <- as.character(s@results[i, l@header.heading.fromColumnName])
					}
					if (l@header.firstInfo.fromColumnName %in% colnames(s@results)) {
						header_firstinfo <- as.character(s@results[i, l@header.firstInfo.fromColumnName])
					}
				}
				
				#assemble file PATH
				myFilepath <- NULL
				if (!is.null(destination_folder)) {
					myFilepath <- file.path(destination_folder, "transcripts", paste(filename, ".txt", sep=""))
				} 
				
				printtrans <- act::export_printtranscript(   t = x@transcripts[[ s@results$transcript.name[i] ]],
															 l = l,
															 outputPath				   = myFilepath,
															 filterSectionStartsec     = startSec,
															 filterSectionEndsec       = endSec,
															 header_heading 	 	   = header_heading,  						   
															 header_firstinfo 	 	   = header_firstinfo,  						 
															 insert_arrow_annotationID = arrowID								
				)    						
				
				#insert into column
				#add  column if missing
				if (!s@cuts.column.printtranscript %in% colnames(s@results)) {
					s@results <- cbind(s@results, newcolumn=as.character(""), stringsAsFactors=FALSE)
					colnames(s@results)[ncol(s@results)] <- s@cuts.column.printtranscript
				}
				#insert transcript into search results
				output <- stringr::str_flatten(printtrans, collapse="\n")
				s@results[i, s@cuts.column.printtranscript] <- output
				
				#cummulate transcripts
				alltranscripts <- c(alltranscripts, printtrans, "","")
				
			}
		} #next i
		
		s@cuts.printtranscripts <- stringr::str_flatten(alltranscripts, collapse="\n")
		
		# if output folder is given
		if (!is.null(destination_folder)) { 
			if (!destination_folder=="NULL") {
				
				#--- save cummulated transcripts as TXT
				filename <- paste("searchResults_",  s@name, ".txt", sep="")
				myFilepath 	<- file.path(destination_folder, filename)
				fileConn 	<- file(myFilepath)
				writeLines(alltranscripts, fileConn)
				close(fileConn)
				
				#--- save modified results
				# R
				filename <- paste("searchResults_", s@name, ".RData", sep="")
				path_R 	    <-	file.path(destination_folder, filename)
				save(s, file = path_R)

				# CSV
				filename <- paste("searchResults_", s@name, ".csv", sep="")
				path_CSV 		<- file.path(destination_folder, filename)
				act::search_results_export(s, path_CSV, saveAsCSV = TRUE)

				# XLSX
				filename <- paste("searchResults_", s@name, ".xlsx", sep="")
				path_XLSX  <- file.path(destination_folder, filename)
				act::search_results_export(s, path_XLSX)
			}
		}
	}
	#=== print warnings
	if (!myWarnings=="") {
		warning(myWarnings)		
	}

	#=== give modified results back
	return(s)
}