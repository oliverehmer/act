#' Create .srt subtitles for all search results
#'
#' Subtitles in 'Subrib Title' .srt format will be created for each search result.
#' The subtitles will be inserted into the column defined in \code{s@cuts.column.srt}.
#' 
#' \emph{Span} \cr
#' If you want to extend the cut before or after each search result, you can modify \code{@cuts.span.beforesec} and \code{@cuts.span.aftersec} in your search object.
#' If you want to modify the layout of the print transcripts, create a new layout object with \code{mylayout <- methods::new("layout")}, modify the settings and pass it as argument \code{l}.
#'
#' @param x Corpus object.
#' @param s Search object.
#' @param outputFolder Character string; if parameter is not set, the srt subtitles will only be inserted in \code{s@results}; if the path to a existing folder is given transcripts will be saved in '.srt' format.
#' @param speaker.show Logical; if \code{TRUE} name of speaker will be shown before the content of the annotation.
#' @param speaker.width Integer; width of speaker abbreviation, -1 for full name without shortening.
#' @param speaker.ending Character string; string that is added at the end of the speaker name.
#'
#'
#' @return Search object; 
#' @export
#'
#' @example inst/examples/search_cuts_srt.R
#' 
#' 
search_cuts_srt <- function(x, 
							s, 
							outputFolder=NULL, 
							speaker.show=TRUE, 
							speaker.width=3, 
							speaker.ending=":" ) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		} else { if (class(s)[[1]]!="search")		{stop("Parameter 's' needs to be a search object.") 	} }
	if (is.null(s@results$transcript.name)) 		{ stop("Data frame s@results does not contain column 'transcript.name'") 	}
	
	
	#--- check if output folder is given
	destination_folder <- NULL
	if (!is.null(outputFolder)) {
		destination_folder <- normalizePath(outputFolder)
		if (destination_folder!="") {
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
	}
	
	myWarnings <- ""
	
	#create sub folders for text
	if (!is.null(destination_folder)) {
		dir.create(file.path(destination_folder, "srt"), showWarnings = FALSE)
	}
	
	#set progress bar
	helper_progress_set("Creating subtitles",max(1,nrow(s@results)))
	
	if (nrow(s@results)) {
		
		i <- 1
		for (i in 1:nrow(s@results)) 	{
			#update progress bar
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
				endSec 		<- min(s@results$endSec[i] + s@cuts.span.beforesec, t@length)
				
				#assemble file PATH
				myFilepath <- NULL
				if (!is.null(destination_folder)) {
					myFilepath <- file.path(destination_folder, "srt", paste(filename, ".srt", sep=""))
				} 
				
				srt <- act::export_srt(   t                     = x@transcripts[[ s@results$transcript.name[i] ]],
										  outputPath            = myFilepath,
										  filterTierNames       = s@filter.tier.names,
										  filterSectionStartsec = startSec,
										  filterSectionEndsec   = endSec,
										  speaker.show          = TRUE, 
										  speaker.width         = 3, 
										  speaker.ending        = ":"
				)   
				
				
				#insert into column
				#add srt column if missing
				if (!s@cuts.column.srt %in% colnames(s@results)) {
					s@results <- cbind(s@results, newcolumn=as.character(""), stringsAsFactors=FALSE)
					colnames(s@results)[ncol(s@results)] <- s@cuts.column.srt
				}
				#insert srt into search results
				output <- stringr::str_flatten(srt, collapse="\n")
				s@results[i, s@cuts.column.srt] <- output
				
			}
		} #next i
		
		
		# if output folder is given
		if (!is.null(destination_folder)) {
			
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
	#=== print warnings
	if (!myWarnings=="") {
		warning(myWarnings)		
	}
	
	#=== give modified results back
	return(s)
}