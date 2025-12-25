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
#' @param l Layout object.
#' @param exportTxt Logical; If \code{TRUE} .docx transcript will be exported.
#' @param exportDocx Logical; If \code{TRUE} .txt transcript will be exported.
#' @param headerInsertSource Logical; if \code{TRUE} standard information about the source and location of the sequence will be inserted after the heading.
#' @param cutSpanBeforesec Double; Start the cut some seconds before the hit to include some context; the default NULL will take the value as set in @cuts.span.beforesec of the search object.
#' @param cutSpanAftersec Double; End the cut some seconds before the hit to include some context; the default NULL will take the value as set in @cuts.span.beforesec of the search object.
#' @param folderOutput Character string; if parameter is not set, the print transcripts will only be inserted in \code{s@results}; if the path to a existing folder is given transcripts will be saved in '.txt' format.
#'
#' @return Search object; 
#' 
#' @export
#'
#' @example inst/examples/search_cuts_printtranscript.R
#' 
search_cuts_printtranscript <- function(x, 
										s,
										l                  = NULL, 
										exportTxt          = TRUE,
										exportDocx         = TRUE,
										headerInsertSource = TRUE,
										cutSpanBeforesec   = 0,
										cutSpanAftersec    = 0,
										folderOutput       = NULL ) {
	if (1==2) {
		x <- corpus
		s <- s
		l<-NULL
		exportTxt <- TRUE
		exportDocx <- TRUE
		cutSpanBeforesec<-0
		cutSpanAftersec <- 0
		folderOutput<-'/Users/oliverehmer/Downloads'
	}
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{stop("Parameter 'x' needs to be a corpus object.") } }
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		}	else { if (!methods::is(s, "search")	)	{stop("Parameter 's' needs to be a search object.") 	} }
	if (is.null(s@results$transcriptName)) 		{ stop("Data frame s@results does not contain column 'transcriptName'") 	}
	
	if (is.null(l)) 	{
		l <- methods::new("layout")
	}	
	
	if (is.null(cutSpanBeforesec)) 	{
		cutSpanBeforesec <- 0
	} else {
		if (length(cutSpanBeforesec)!=1) {
			stop("Parameter 'cutSpanBeforesec' needs to contain only one element as a numeric value.") 
		}
		if (!is.numeric(cutSpanBeforesec)) {
			stop("Parameter 'cutSpanBeforesec' needs to be a numeric value.") 
		}
		s@cuts.span.beforesec       <- cutSpanBeforesec
	}
	
	if (is.null(cutSpanAftersec)) 	{
		cutSpanAftersec <- 0
	} else {
		if (length(cutSpanAftersec)!=1) {
			stop("Parameter 'cutSpanAftersec' needs to contain only one element as a numeric value.") 
		}
		if (!is.numeric(cutSpanAftersec)) {
			stop("Parameter 'cutSpanAftersec' needs to be a numeric value.") 
		}
		s@cuts.span.aftersec       <- cutSpanAftersec
	}
	
	if (!options()$act.export.filename.fromColumnName %in% colnames(s@results)) {
		stop("The column defined in the option 'options()$act.export.filename.fromColumnName' does not exist in the data.frame with the search results.")
	}
	
	#--- check if output folder is given
	folder.destination <- NULL
	if (!is.null(folderOutput)) {
		folder.destination <- normalizePath(folderOutput, winslash = "/", mustWork = FALSE)
		if (dir.exists(folder.destination)==FALSE) 	{
			stop("Output folder does not exist.")
		}
		
		#create a sub folder for the search
		if (s@name!="") 	{
			folder.destination <- file.path(folder.destination, s@name)
			dir.create(folder.destination, showWarnings = FALSE)
			if (file.exists(folder.destination)==FALSE) 		{
				stop("Unable to create output directory")
			}
		}
	}	
	myWarnings <- ""
	
	#create sub folders for text
	dir.create(file.path(folder.destination, "transcripts"), showWarnings = FALSE)
	
	helper_progress_set("Creating transcripts",max(1,nrow(s@results)))
	
	trans.txt.all <- c()
	#i<-1
	if (nrow(s@results)>0) {
		for (i in 1:nrow(s@results)) 	{
			helper_progress_tick()
			
			#==== GET TRANSCRIPT ====
			t <- NULL
			if (is.null(s@results$transcriptName[i])) {
				#transcript not found
				myWarnings <- paste(myWarnings, sprintf("- result %s '%s': transcript '%s' not found in corpus. ", i, as.character(s@results[i, options()$act.export.filename.fromColumnName]),  as.character(s@results$transcriptName[i]) ), collapse="\n", sep="\n")
				
			} else {
				t <- x@transcripts[[ s@results$transcriptName[i] ]]
				if (is.null(t)) {
					#transcript not found
					myWarnings <- paste(myWarnings, sprintf("- result %s '%s': transcript '%s' not found in corpus. ", i, as.character(s@results[i, options()$act.export.filename.fromColumnName]),  as.character(s@results$transcriptName[i]) ), collapse="\n", sep="\n")
				}
			}
			#next t is null
			if (is.null(t)) { next}
			
			#==== FILE NAME ====
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
			
			#==== TIME ====
			startSec 	<- max(0, s@results$startSec[i] - s@cuts.span.beforesec)
			endSec 		<- min(s@results$endSec[i] + s@cuts.span.beforesec, t@length.sec)
			
			#==== ARROW ====
			if (!l@arrow.insert) {
				arrowID <- -1
			} else {
				arrowID <- s@results$annotationID[i]
			}
			#View(s@results)
			#==== HEADER ====
			headerPreface	 	<- NULL
			headerTitle 		<- NULL
			headerSubtitle	 	<- NULL
			headerDescription 	<- NULL
			if (l@header.insert==TRUE) {
				if ("header.preface" %in% colnames(s@results)) {
					headerPreface <- as.character(s@resultsheader.preface[i])
				}
				if ("header.title" %in% colnames(s@results)) {
					headerTitle <- as.character(s@results$header.title[i])
				} else {
					#if title is not given, take resultID
					if ("header.title" %in% colnames(s@results)) {
						headerTitle <-  as.character(s@results$resultID[i])
					}
				}
				if ("header.subtitle" %in% colnames(s@results)) {
					headerSubtitle <- as.character(s@results$header.subtitle[i])
				}
				if ("header.description" %in% colnames(s@results)) {
					headerDescription <- as.character(s@results$header.description[i])
				}
			}
			
			#==== CREATE TRANS =====
			#---- .txt ----
			if (exportTxt) {
				#path
				path.file <- NULL
				if (!is.null(folder.destination)) {
					path.file <- file.path(folder.destination, "transcripts", paste(filename, ".txt", sep=""))
				} 
				
				#export
				trans.txt <- act::export_txt(   t = x@transcripts[[ s@results$transcriptName[i] ]],
												l = l,
												pathOutput				  = path.file,
												filterSectionStartsec     = startSec,
												filterSectionEndsec       = endSec,
												headerPreface 	 	      = headerPreface,  	
												headerTitle 	 	      = headerTitle,  						   
												headerSubtitle 	          = headerSubtitle,  			
												headerDescription 	      = headerDescription,  
												headerInsertSource        = headerInsertSource,
												insertArrowAnnotationID   = arrowID								
				)    						
				
				#---- . store values to columns ----
				#add  column if missing
				if (!s@cuts.column.printtranscript %in% colnames(s@results)) {
					s@results <- cbind(s@results, newcolumn=as.character(""), stringsAsFactors=FALSE)
					colnames(s@results)[ncol(s@results)] <- s@cuts.column.printtranscript
				}
				#insert transcript into search results
				output <- stringr::str_flatten(trans.txt, collapse="\n")
				s@results[i, s@cuts.column.printtranscript] <- output
				
				#accumulate transcripts
				trans.txt.all <- c(trans.txt.all, trans.txt, "","")
			}
			
			if (exportDocx) {
				#path
				path.file <- NULL
				if (!is.null(folder.destination)) {
					path.file <- file.path(folder.destination, "transcripts", paste(filename, ".docx", sep=""))
				} 
				
				#export
				trans.doxc<- act::export_docx(  t = x@transcripts[[ s@results$transcriptName[i] ]],
												l = l,
												pathOutput				  = path.file,
												filterSectionStartsec     = startSec,
												filterSectionEndsec       = endSec,
												insertArrowAnnotationID   = arrowID,			
												headerPreface 	 	      = headerPreface,  	
												headerTitle 	 	      = headerTitle,  						   
												headerSubtitle 	          = headerSubtitle,  			
												headerDescription 	      = headerDescription,
												headerInsertSource        = headerInsertSource
				) 
			}
		} #next i
	}
	s@cuts.printtranscripts <- stringr::str_flatten(trans.txt.all, collapse="\n")
	
	#==== SAVE TO FILES ====
	# if output folder is given
	if (!is.null(folder.destination)) { 
		if (!folder.destination=="NULL") {
			
			#---- . txt ----
			if(exportTxt) {
				path.file 	<- file.path(folder.destination, paste("searchResults_",  s@name, ".txt", sep=""))
				fileConn 	<- file(path.file)
				writeLines(trans.txt.all, fileConn)
				close(fileConn)
			}
			
			#---- . docx ----
			if(exportDocx) {
				path.file 	<- file.path(folder.destination, paste("searchResults_",  s@name, ".docx", sep=""))
				
				#if template path is not set, use the standard template
				path.template <- l@docx.template.path
				if (path.template==""){
					path.template <- system.file("extdata", "docx", "template_transcript.docx", package="act")	
				}
				
				#
				folder.input <- file.path(folder.destination, collection)
				folder.input <- folder.destination
				result <- merge_docx (folderInput        = folder.input,
									  pathTemplateInput  = path.template,  
									  pathOutput         = file.path(folder.destination, "merged_transcripts.docx"),
									  recursive          = TRUE)
			}
			
			#---- . R ----
			path_R 	    <-	file.path(folder.destination, 	 paste("searchResults_", s@name, ".RData", sep=""))
			save(s, file = path_R)
			
			#---- . csv ----
			path_CSV 		<- file.path(folder.destination, 	paste("searchResults_", s@name, ".csv", sep=""))
			act::search_results_export(s, path_CSV, saveAsCSV = TRUE)
			
			#---- . xlsx ----
			path_XLSX  <- file.path(folder.destination, paste("searchResults_", s@name, ".xlsx", sep=""))
			act::search_results_export(s, path_XLSX)
		}
		
	}
	#=== print warnings
	if (!myWarnings=="") {
		warning(unique(myWarnings))	
	}
	
	#=== give modified results back
	return(s)
}